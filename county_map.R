# county_data_module

# Helper functions
  
  # leaflet print
  ## !! Only works in chrome and firefox
  jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
  
# User Interface ####
county_map_UI <- function( id ) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  fluidPage(
                  material_column( width = 12 , 
                                   
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
                                               timeFormat="%Y-%m-%d" ,
                                               animate = TRUE ) ,
                                   
                                    material_slider( ns('slopeCut') ,
                                                       'Slope cutoff' ,
                                                       min_value = .01 ,
                                                       max_value = .2 ,
                                                       initial_value = .1 ,
                                                    step_size = .01
                                                    ) 
                  
                  ) 
)

}

# Server function ####
county_map <- function( input, output, session, data , 
                        model , 
                         forecastData ,
                         # precastData ,
                         input_variables ,
                         movingAverageDays 
                         ) {
 # moving average text box
  output$movingAverageText = renderText(  paste( movingAverageDays() , "day moving average" )  )
  
  start_date = reactive({ 
    
    req( data() )
    
    # start date
    if ( length( unique( data()$fips ) ) > 5 ){
      min.num = 5
    } else {
      min.num = 0
    }
    
     d = data()%>% as_tibble() %>%
      filter( cases <= min.num ) %>% 
      group_by( county, state, fips ) %>%
      summarise( date = max( date , na.rm = TRUE ) ) %>%
      ungroup() 
    
    start_date = min( d$date )
    
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
  
  tmapData = reactive({ 
    req( dataTS() )
    
    print( 'tmapData' ) ; glimpse( dataTS() )
    
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
  
  
### outputs ####

   output$map = renderLeaflet({
   req( tmapData() )
     
  # Palette
   if ( !is_tsibble( model() ) ){ 
     pal = "Reds"
   } else {
     pal = brewer.pal(5, "RdYlGn" ) # 
     # pal = c( growing = "red", plateau = "blue" , declining = "green")
   }
     
    # split features into map for each level
    # split_tmapData = split( tmapData() , tmapData()$name )
    #  
    # st_crs( split_tmapData ) = 4326
    # 
    # print( 'split_tmapData') ; glimpse( split_tmapData )
    
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
   
  
  return( list(  ) )
    
}

