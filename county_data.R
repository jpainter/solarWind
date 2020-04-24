# county_data_module

# Helper functions
  
  # leaflet print
  ## !! Only works in chrome and firefox
  jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
  
# User Interface ####
county_data_UI <- function( id ) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
                fluidRow( 
                  style="padding-left: 10px;" ,
                  textOutput( ns('stateList')) ,
                  material_column( width = 6, 

                                   textOutput( ns("movingAverageText" ) ) ,  
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
                  material_column( width = 6, 
                                   
                                   # leaflet print plugin - Only works in chrome and firefox
                                   tags$head(tags$script(src = jsfile)) ,
                                   leafletOutput( ns('map') ) ,

                                   # dateRangeInput(  ns('asOf') , 'Status as of (date)' , 
                                   #                  start = ymd( "2020-03-01" ), end = Sys.Date() , 
                                   #                  format = "yyyy-mm-dd", 
                                   #                  startview = "day" ) ,
                                   
                                   sliderInput( ns('asOf') ,
                                                'Status as of (date)' ,
                                               min = as.Date("2020-03-01" ,"%Y-%m-%d"),
                                               max = as.Date( Sys.Date() ,"%Y-%m-%d"),
                                               value = Sys.Date() - days(2),
                                               timeFormat="%Y-%m-%d")
                                   )
                  
                  ) ,

                fluidRow(  
                    plotlyOutput( ns('histograms') )
                  )
)

}

# Server function ####
county_data <- function( input, output, session, data , model , 
                         forecastData ,
                         input_variables ,
                         movingAverageDays 
                         ) {
 # moving average text box
  output$movingAverageText = renderText(  paste( movingAverageDays() , "day moving average" )  )
  
  countyCount = reactive({
    
    # glimpse( data() )
    req( data() )
    cd = count( data() , county   )
    return( cd )

  }) 
  
  # Update date slider
  observeEvent( data() ,{
                
                print( 'update date')
                maxDate = max( data() %>% pull(date) ) 
                print( maxDate ) ; print( class( maxDate ))
                
                updateSliderInput(  session, inputId = "asOf" , 
                                   max = maxDate  ,
                                   value = maxDate )
  })
  
  dataTS = reactive({
    req( data() )
    print( 'dataTS')
    
    d = data() %>% mutate( status = value )
    m = model() 
  
    print( 'joining data with model' )
    # glimpse( d ) ; glimpse( m )
    if ( is_tsibble( m ) ){
      
        d = left_join( d %>% as_tibble %>% select( -status ), 
                     m %>% as_tibble %>% rename( fit = value ) # change name to avoid conflict with raw value
                     # y is the fitted value of the model .. Eq to .fitted
                     ,  by = c( 'state', 'county', 'fips' , 'name' , 'date' )) 
        
        glimpse( d )
        
        # Cut offs
        low.inc.cut = 10
        slope.cut = input$slopeCut # 0.1
      
        # modeled data
        m =  d %>%
          group_by( state, county, fips , name ) %>%
          mutate( 
            value.scale = scale( fit ) ,
            deriv1 = difference( value.scale , lag = 1 ) ,
            lead_x = lead( date ) ,
            lead_y = lead( fit ) ,
            deriv1.7dave = slider::slide_dbl( deriv1 , mean , .before = 6 ) ,
            cat = case_when(  
                  deriv1 > -slope.cut & deriv1 < slope.cut ~ 'plateau' ,
                  deriv1 >= slope.cut ~ "growing" ,
                  deriv1 <= -slope.cut ~ "declining"
                  ) ) %>%
          
          # Sustained decline
          mutate( 
            # Stuck at high incidence is ... last 5 days at plateau where incidence > 10/10^5
            higIncidence = slider::slide_lgl( incidence , ~  all( .x >= low.inc.cut ) , .before = 4 ) ,
            StuckPlateau = slider::slide_lgl( cat, ~ all( .x %in% 'plateau' ) , .before = 4 ) ,
            
            # Decline when ...
            sustainedDecline7 = slider::slide_lgl(cat , ~ all( ! .x %in% 'growing' ) , .before = 6 ) ,
            sustainedDecline14 = slider::slide_lgl(cat , ~ all( ! .x %in% 'growing' ) , .before = 13 )
            
            
            ) %>%
          mutate( 
            
            status = case_when(
              sustainedDecline14 & ! ( higIncidence &  StuckPlateau ) ~ 'sustained decline (14 days)' ,
              sustainedDecline7 & ! ( higIncidence &  StuckPlateau ) ~ 'sustained decline (7 days)' ,
              TRUE ~ cat ) , 
            
            status = factor( lead( status ) , 
                             levels = c( 'growing', 'plateau', 'declining', 
                                         'sustained decline (7 days)' ,
                                         'sustained decline (14 days)'
                                         ) ) , # geom seg wants previous cat to coincide with slope
            ) 
        # %>%
        #   
        #   select( state, county, fips , name , cat, status, date , value ,
        #           lead_x , lead_y , y  ) 
        
        print( 'class m') ; print( class( m ) ) ; 
        
        # glimpse( m ) ; saveRDS( m , 'testM.rds')
        
        # d = left_join( d %>% as_tibble %>% select( - status ), 
        #                m %>% as_tibble   , 
        #                by = c( 'state', 'county', 'fips' , 'name' , 'date' )) 
        
        # print( 'class d') ; print( class( d ) )
        
        d = m %>% as_tsibble( key = c(state, county , fips , name ) , 
                             index = date ) 
    }
    
    # glimpse(d)
    # saveRDS( d, 'catD.rds')
    
    return( d )
  })
  

  # ggplot of time-series
  ggplotTS = reactive({
      
    req( dataTS() )
    # glimpse( dataTS) # test
    print( 'input vars') ; print( input_variables() )
    selected_vars = rlang::syms( input_variables() ) 

    d = dataTS() 

    print( 'dataTS pivot data') ; 
    glimpse( d )
    
    # Dynamic Facets
    facets = as.formula( paste( "~", "name") )
    
    # start date
    if ( length( unique( d$fips ) ) > 5 ){
      min.num = 5
    } else {
      min.num = 0
    }
      
    start_date = d %>% as_tibble() %>%
      filter( cases <= min.num ) %>% 
      group_by( county, state, fips ) %>%
      summarise( date = max( date , na.rm = TRUE ) ) %>%
      ungroup() 
    
    print( 'start date' ) ; print( start_date )
    
    start_date = min( start_date$date )
    
    # end date
    end_date = d %>% pull( date ) %>% max
    
    print( 'min date'); print( start_date )
    
    d = d %>%
      ungroup %>%
      mutate( county = paste( county , state , sep = ", ") ) 
    
    g = 
      d %>%
      ggplot( aes( x = date, y = value ,  group = fips , 
                   label = county )
              )
    
    # bar chart vs line chart
    print( 'distinct fips') ; print( unique( d$fips ) ) 
    if ( length( unique( d$fips ) ) > 1 ){
      g = g + geom_line() 
    } else {
      g = g + geom_col() 
    }
     
     # facets 
     g = g +
       facet_wrap( facets, 
                  ncol = 1 , scales = 'free' , 
                  strip.position = "top" , labeller = label_value ) + 
      scale_x_date( limits = c( start_date , end_date ) , 
                    date_labels = "%m/%d" ) +
      theme_minimal() +
      labs( x = "" , y = "" , 
            subtitle = paste( movingAverageDays() , "day moving average" ) ,
            source = "https://data.usafacts.org" ) 
    
    # Add model
    if ( is_tsibble( model() ) ){
   
      print( 'chart m' ); 
      # glimpse( m )
      
      g = g + 
        # fitted line
        # geom_line( data = m , aes( y = y , color = cat , group = 1 ) ) +
        geom_segment( data = d , aes(x = date, xend = lead_x , 
                          y = fit, yend = lead_y , color = status ,
                          group = fips )
        ) +
        # scale_color_brewer( palette =  "RdYlGn",  type = "div" , drop = FALSE )
        
        scale_color_manual( values = brewer.pal(5, "RdYlGn" )  ,
                           drop = FALSE ) +
        
        # Legend position
        # Warning: plotly.js does not (yet) support horizontal legend items 
        theme( legend.position = 'top' 
               , legend.justification='left'
               # , legend.direction = 'horizontal'
               )
  
    }
    
    # Add forecast
    if ( is_tsibble( forecastData() ) ){

      print( 'forecastData' )
      # glimpse( forecastData() )

      f = forecastData()  %>% hilo %>% unnest( `95%` )

      print( 'chart f' );
      # glimpse( f )

      g = g +
        # fitted line
        # geom_line( data = m , aes( y = y , color = cat , group = 1 ) ) +
        geom_line( data = f , aes(x = date, y = pred , group = fips ) ,
                   color = 'brown' , alpha=0.5 ) +
        geom_ribbon( data = f ,
                     aes( ymin = .lower, ymax = .upper ) , 
                     color = 'brown' , alpha=0.2 )
      # +
      #   scale_color_manual(
      #     values = c( "growing" = "red", "plateau" = "blue" , "declining" = "green") ,
      #     )
    }
      return( g )
  }) 

  tmapData = reactive({ 
    req( dataTS() )
    
    print( 'tmapData' )
    
    if ( nrow( dataTS() ) == 0 ) return( NULL )
      
    # if missing lat/lonfg, add in
    if ( !all( c('lat', 'long') %in% names( dataTS() ) ) ){
      geoFIPS = readRDS( 'geoFIPS.rds') %>% 
        select( fips, lat, long  )
      
      d = dataTS() %>% left_join( geoFIPS , by = "fips" ) 
      
    } else {
      
      d = dataTS()
      # glimpse( d )
    }
    
    d.last = d %>% as_tibble() %>%
      group_by( state, county, fips , name ) %>% 
      arrange( state, county, fips , name , desc( date ) ) %>%
      mutate( status = lead( status , 1 ) ) %>%
      # filter( row_number() == 1 ) %>%
      filter( date %in% input$asOf ) %>%
      filter( !is.na( lat ) , !is.na( long ) ) 
  
    # convert to sf
    dsf = st_as_sf( d.last , coords = c( "long", "lat" ) ) 

    print( 'd.last') ;
    glimpse( d.last )
        
    return( dsf )
    }) 
  
  ggHistogram = reactive({
    req( tmapData) 
    
    if ( is_tsibble( model() ) ){
      
      print( 'histogram' )
    
     d = count( tmapData() , name , status )
      
     g =  ggplot( d , aes( x = status , y = n , fill = status ) ) +
        geom_col() +
        scale_fill_manual( values = brewer.pal(5, "RdYlGn" )  ,
                            drop = FALSE ) +
       facet_wrap( ~ name , nrow = 1 )
     
     return( g )
     
    } else { return( ggplot() ) }
    

  })
  
  
### outputs ####

  output$chartTS = renderPlotly({ 
    req( ggplotTS() ) 
    # ggplotTS() 
    ggplotly( ggplotTS() )  %>% 
      layout(
       # title = "New plot title",
       legend = list(orientation = "h",
                   y = 0, x = 0) )
    })
  
   output$map = renderLeaflet({
   req( tmapData() )
     
  # Palette
   if ( !is_tsibble( model() ) ){ 
     pal = "Reds"
   } else {
     pal = brewer.pal(5, "RdYlGn" ) # 
     # pal = c( growing = "red", plateau = "blue" , declining = "green")
   }
     
   tm = tm_shape( tmapData() ) +
     tm_dots( id = 'county' , size = 'value' , 
              scale = 3 , 
              popup.vars =  # TRUE ,
              c(
                " " = "name" , " " = "value" ,
                "Cumulative Cases:" = "cases",
                "Cumulative Deaths:" = "deaths" ,
                "Population" = "pop" , "Date:" = "date") ,
              col = 'status' , alpha = .5  ,
              palette =  pal ,
              legend.hist = TRUE 
              ) +
     tm_view( view.legend.position = c('left' , 'bottom'),
              # projection = "WGS84" , 
              dot.size.fixed = FALSE )
  
   tmap_leaflet( tm ) %>%
     # addLegend( position =  "bottomright" , 
     #            pal=pal , values = ~value ) %>%
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
   
   # Histograms
   output$histograms = renderPlotly({ 
     req( tmapData() ) 

     ggplotly( ggHistogram() )   
     
   })
   
  # currently unused
  output$countyCount =  renderTable({
    
    req( countyCount() )
    countyCount()
  })
  
  return( list(  ) )
    
}

