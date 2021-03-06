# county_data_module

# Helper functions
  
  # leaflet print
  ## !! Only works in chrome and firefox
  jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
  
# User Interface ####
county_chart_UI <- function( id ) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
                fluidRow( 
                  style="padding-left: 10px;" ,
                  textOutput( ns('stateList')) ,
                  material_column( width = 12, 

                                   textOutput( ns("movingAverageText" ) ) ,  
                                   plotlyOutput( ns('chartTS') ) ,
                                   # plotOutput( ns('chartTS') ) ,
   
                                   material_slider( ns('slopeCut') ,
                                                       'Slope cutoff' ,
                                                       min_value = .01 ,
                                                       max_value = .2 ,
                                                       initial_value = .01 ,
                                                    step_size = .01
                                                    ) ,
                                    sliderInput( ns('asOf') ,
                                                'Status as of (date)' ,
                                               min = as.Date("2020-03-01" ,"%Y-%m-%d"),
                                               max = as.Date( Sys.Date() ,"%Y-%m-%d"),
                                               value = Sys.Date() - days(2),
                                               timeFormat="%Y-%m-%d" ,
                                               animate = TRUE )                                  
                                   ) 
                  
                  ) 
)

}

# Server function ####
county_chart <- function( input, output, session, data , 
                          model , 
                          model_type , 
                         forecastData ,
                         # precastData ,
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
  
  
  start_date = reactive({ 
    
    req( data() )
    
    # start date
    
    print( paste( 'nrow( data() )' ,nrow( data() ) ))
    if ( nrow( data() )  > 150 ){
      min.num = 5
    } else {
      min.num = 1
    }
    print( paste( 'min.num' , min.num ))
    # test saveRDS( data() , 'data.rds')
    
    d = data()%>% as_tibble() %>%
      filter( cases <= min.num ) %>% 
      group_by( county, state, fips ) %>%
      summarise( date = max( date , na.rm = TRUE ) ) %>%
      ungroup() 
    
    print( 'start date d')
    print( head( d ) )
    
    if ( nrow(d) > 0 ){
      start_date = min( d$date ) 
    } else {
      start_date = Sys.Date() - days(30)
    }
    
    
    print( 'start date')
    print( start_date )
    
    
    return( start_date )
  })
  
  end_date = reactive({
    req( data())
    
    # end date
    end_date = data() %>% pull( date ) %>% max
    
    print( 'end_date date type') ; print( class( end_date ) ) ; print( end_date )
    
    return( end_date ) 
    
  })
  
  # Update date slider
  observeEvent( data() ,{
                
                print( 'update date')
                minDate = start_date()
                
                maxDate = max( data() %>% pull(date) ) 
                print( maxDate ) ; print( class( maxDate ))
                
                updateSliderInput(  session, inputId = "asOf" , 
                                   max = maxDate  ,
                                   value = maxDate )
  })
  
  dataTS = reactive({
    req( data() )
    print( 'dataTS') ; print( 'testing model') ; print( is.na( model())) ; print( model() ) ; 
    
    d = data() %>% mutate( status = value )
    m = model() 
  
    print( 'joining data with model' )
    # glimpse( d ) ; glimpse( m )
    if ( is_tsibble( m ) ){
      
        d = left_join( d %>% as_tibble %>% select( -status ), 
                     m %>% as_tibble %>% rename( fit = value ) # change name to avoid conflict with raw value
                     # y is the fitted value of the model .. Eq to .fitted
                     ,  by = c( 'state', 'county', 'fips' , 'name' , 'date' )) 
        
        # glimpse( d )
        
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
    # glimpse( d )
    
    # Dynamic Facets
    facets = as.formula( paste( "~", "name") )

      
    print( 'start date' ) ; print( start_date() )
    
    print( 'end date'); print( end_date() )
    
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
      g = g + geom_line( alpha = .7 , size = .7 ) 
      
    } else {
      g = g + geom_col() 
    }
    
    print( 'vertical line') ; print( input$asOf ) 
     
     # facets 
     g = g +
       facet_wrap( facets, 
                  ncol = 1 , scales = 'free' , 
                  strip.position = "top" , labeller = label_value ) + 
       
      scale_x_date( limits = c( start_date() , end_date() ) , 
                    date_labels = "%m/%d" ) +
       
      geom_vline( xintercept = as.numeric( ymd( input$asOf ) ) , 
                  size = .3, alpha = .5 , color = 'blue' ) +
       
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
                          group = fips ) ,
                      alpha = .75 , size = .75
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
     print( 'forecast') ;
     # glimpse(forecastData())
     
    if ( is_tsibble( forecastData() ) ){

      print( 'forecastData' )
      
      if (! model_type() %in% 'Spline' ){
        f = forecastData()  %>% hilo %>% unnest( `95%` )
      
        } else {
        f = forecastData()  %>% 
          mutate(
            .lower = value , 
            .upper = value
          )
      }
            
      glimpse( f )
      
      # expand date range if forecast past last date in data
      if ( max( f$date ) > end_date() ){ end_date = max( f$date ) 
      } else { end_date = end_date() }

      print( 'chart f' );
      # glimpse( f )

      g = g +
        # fitted line
        # geom_line( data = m , aes( y = y , color = cat , group = 1 ) ) +
        geom_line( data = f , aes(x = date, y = value , group = fips ) ,
                   color = 'brown' , alpha=0.5 ) +
        geom_ribbon( data = f ,
                     aes( ymin = .lower, ymax = .upper , group = fips ) , 
                     color = 'brown' , alpha=0.2 ) +
        scale_x_date( limits = c( start_date() , end_date ) ) 
                      
      # +
      #   scale_color_manual(
      #     values = c( "growing" = "red", "plateau" = "blue" , "declining" = "green") ,
      #     )
    }
     
     # if ( is_tsibble( precastData() ) ){
     #   
     #   print( 'forecastData' )
     #   # glimpse( forecastData() )
     #   
     #   p = precastData()  %>% hilo %>% unnest( `95%` )
     #   
     #   end_date = max( p$date )
     #   
     #   print( 'chart p' );
     #   # glimpse( p )
     #   
     #   g = g +
     #     geom_line( data = p , aes(x = date, y = value , group = fips ) ,
     #                color = 'blue' , alpha=0.5 ) +
     #     geom_ribbon( data = p ,
     #                  aes( ymin = .lower, ymax = .upper ) , 
     #                  color = 'blue' , alpha=0.2 ) 
     #   
     # }
      return( g )
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
  
  return( list(  ) )
    
}

