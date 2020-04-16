# county_data_module

# Helper functions
  
  # leaflet print
  ## !! Only works in chrome and firefox
  jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
  
# User Interface ####
county_data_UI <- function( id ) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
                material_row(
                  textOutput( ns('stateList')) ,
                  material_column( width = 5, offset = 1 ,
                                   plotlyOutput( ns('chartTS') ) ,
                                   # plotOutput( ns('chartTS') ) ,
   
                                   material_slider( ns('slopeCut') ,
                                                       'Slope cutoff' ,
                                                       min_value = .01 ,
                                                       max_value = .2 ,
                                                       initial_value = .1 ,
                                                    step_size = .01
                                                    ) 
                                   ) ,
                  material_column( width = 5, 
                                   
                                   # leaflet print plugin 
                                   ## !! Only works in chrome and firefox
                                   tags$head(tags$script(src = jsfile)) ,
                                   leafletOutput( ns('map') ) ,
                                   "Map displays most recent value"
                                   )
                  )

}

# Server function ####
county_data <- function( input, output, session, data , model , 
                         input_variables
                         ) {

  countyCount = reactive({
    
    # glimpse( data() )
    req( data() )
    cd = count( data() , county   )
    return( cd )

  }) 
  
  dataTS = reactive({
    req( data() )
    
    ts =  data_ts( data() ) 
  })
  

  # ggplot of time-series
  ggplotTS = reactive({
      
    req( dataTS() )
    # glimpse( dataTS) # test
    print( 'input vars') ; print( input_variables() )
    selected_vars = rlang::syms( input_variables() ) 

    d = dataTS() %>% pivot_longer( cols = starts_with( input_variables()  ) )
    # print( 'pivot data') ; glimpse( d )
    
    g = 
      # dataTS() %>%
      d %>%
      mutate( county = paste( county , state , sep = ", ") ) %>%
      ggplot( aes( x = date, y = value ,  group = fips , 
                   label = county )
              ) +
      geom_line() +
      facet_wrap( ~ name , ncol = 1 , scales = 'free' , strip.position = "top") + 
      scale_x_date( date_labels = "%m/%d" ) +
      theme_minimal() +
      labs( x = "" , y = "") 
    
    if ( is_tsibble( model() ) ){

      glimpse( model() )
      
      # Cut offs
        low.inc.cut = 10
        slope.cut = input$slopeCut # 0.1
  
      m = model() 
      
      m =  m %>%
        group_by( state, county, fips , name ) %>%
        mutate( 
          deriv1 = difference( value , lag = 1 ) ,
          lead_x = lead( date ) ,
          lead_y = lead( value ) ,
          # deriv1.7dave = slider::slide_dbl( deriv1 , mean , .before = 6 ) ,
          cat = case_when(  
                deriv1 > -slope.cut & deriv1 < slope.cut ~ 'plateau' ,
                deriv1 >= slope.cut ~ "growing" ,
                deriv1 <= -slope.cut ~ "declining"
                )  
        ) %>% ungroup()
      
      print( 'chart m' ); 
      glimpse( m )
      
      g = g + 
        # fitted line
        # geom_line( data = m , aes( y = y , color = cat , group = 1 ) ) +
        geom_segment( data = m ,
                      aes(x = date, xend = lead_x , 
                          y = value, yend = lead_y , color = cat ,
                          group = fips )
        ) +
        scale_color_manual( 
          values = c( "growing" = "red", "plateau" = "blue" , "declining" = "green") ,
          )
    }
      return( g )
  }) 

  tmapData = reactive({ 
    req( data() )

    if ( nrow( data() ) == 0 ) return( NULL )
      
    # if missing lat/lonfg, add in
    if ( !all( c('lat', 'long') %in% names( data() ) ) ){
      geoFIPS = readRDS( 'geoFIPS.rds') %>% select( fips, lat, long  )
      d = data() %>% left_join( geoFIPS , by = "fips" ) 
      
    } else {
      
      d = data()
      # glimpse( d )
    }
    
    d.last = d %>% 
      group_by( fips , county ) %>% 
      arrange( desc( date ) ) %>%
      filter( row_number() == 1 ) %>%
      filter( !is.na( lat ) , !is.na( long ) ) 
  
    # convert to sf
    dsf = st_as_sf( d.last , coords = c( "long", "lat" ) ) 

    return( dsf )
    }) 
### outputs ####

  output$chartTS = renderPlotly({ 
    req( ggplotTS() ) 
    # ggplotTS() 
    ggplotly( ggplotTS() )  # %>% hide_legend()
    })

   output$map = renderLeaflet({
   req( tmapData() )
   tm = tm_shape( tmapData() ) +
     tm_dots( size = 'cases' , col = 'cases' , alpha = .5 )
   tmap_leaflet( tm ) %>%
     onRender(
          "function(el, x) {
            L.easyPrint({
              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
              filename: 'countyCoronaMap',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
        )
   })
   
  output$countyCount =  renderTable({
    
    req( countyCount() )
    countyCount()
  })
  
  return( list(  ) )
    
}

