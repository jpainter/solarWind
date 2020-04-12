# county_data_module

# Helper functions

# User Interface ####
county_data_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
                material_row(
                  textOutput( ns('stateList')) ,
                  material_column( width = 5, offset = 1 ,
                                   plotlyOutput( ns('chartTS') )
                                   # plotOutput( ns('chartTS') )
                                   ) ,
                  material_column( width = 5, 
                                   # tableOutput( ns('countyCount') )
                                   leafletOutput( ns('map') ) ,
                                   "Map displays most recent value"
                                   )
                  )

}

# Server function ####
county_data <- function( input, output, session, data , model 
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

    g = 
      dataTS() %>%
      ggplot( aes( x = date, y = cases ,  group = fips ) ) +
      geom_line() +
      # autoplot( cases ) +
      scale_x_date( date_labels = "%m/%d" ) +
      theme_minimal() +
      labs( x = "" , y = "") 
    
    if ( is_tsibble( model() ) ){

      glimpse( model() )
      
      # Cut offs
        low.inc.cut = 10
        slope.cut = 0.1
  
      m =  model() %>%
        mutate( 
          deriv1 = difference( y , lag = 1 ) ,
          deriv1.7dave = slider::slide_dbl( deriv1 , mean , .before = 6 ) ,
          cat = case_when(  
                deriv1 > -slope.cut & deriv1.7dave < slope.cut ~ 'plateau' ,
                deriv1 >= slope.cut ~ "growing" ,
                deriv1 <= -slope.cut ~ "declining"
                )  
        )
      
      print( 'chart m'); 
      glimpse( m )
      
      g = g + 
        # fitted line
        # geom_line( data = m , aes( y = y , color = cat , group = 1 ) ) +
        geom_segment( data = m ,
                      aes(x = date, xend = lead( date ), 
                          y = y, yend = lead( y ) , color = cat )
        ) +
        scale_color_manual( 
          values = c( "growing" = "red", "plateau" = "blue" , "declining" = "green") ,
          )
                   # linetype = "dashed" ) 
        
        # 7 day average of first derivative of fitted values
        # geom_line( data = m , aes( y = deriv1.7dave ) ,linetype = "dotted") 
      
        # autolayer( m , y , linetype = "dashed" , color = cat ) +
        # autolayer(  m , deriv1 , linetype = "dashed" ) +
        # autolayer(  m , deriv1.7dave , linetype = "dashed" ) 

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

   # output$map = renderLeaflet({
      # req( tmapData() )
      # tm = tm_shape( tmapData() ) +
      #   tm_dots( size = 'cases' , col = 'cases' , alpha = .5 )
      # tmap_leaflet( tm )
    # })
   
  output$countyCount =  renderTable({
    
    req( countyCount() )
    countyCount()
  })
  
  return( list(  ) )
    
}

