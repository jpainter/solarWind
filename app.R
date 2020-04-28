#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Libraries ####
# Function to test if package is installed

libraries = readLines( con = file( 'requirements.txt' ) , warn=FALSE )
libraries = gsub(" ", "" ,  libraries)

# pkgTest <- function( package.list = libraries ){
# 
#   missing.packages = setdiff( package.list , rownames( installed.packages() ) )
# 
#   if ( length( missing.packages ) > 0 & nchar( missing.packages[1] ) ){
#     print( missing.packages )
# 
#         install.packages( missing.packages
#                           # , dependencies = TRUE ,
#                           # , type="source" ,
#                           # , repos = "https://cran.rstudio.com"
#                           )
#     }
# 
# }

# Test if packages installed
# pkgTest( libraries )

# load the packages
suppressMessages(
  lapply( libraries , library  , character.only = TRUE)
)


# load modules ####
source( 'county_data.R' )
source( 'dataManagementFunctions.R' )

# Define UI #####
ui <- material_page(
    
    title = "Source: USAFacts (https://data.usafacts.org)",  # "Solar Wind and Coronal Hot Spots" ,
    nav_bar_fixed = TRUE ,
    useShinyjs(),
    include_fonts = T,
    nav_bar_color = "blue" ,
    
    material_modal(
      modal_id = "message",
      button_text = "wait up to 30 sec",
      button_icon = "open_in_browser",
      title = "Preparing time-series" ,
      tags$p("Modal Content") ,
      display_button = FALSE
    ) , 
 
  # SIDEBAR 
  material_side_nav(

    # image_source = "side_nav.jpeg",  
    fixed = TRUE ,
   
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "County Data" = "county_data" 
        # ,  "Data table" = "data_table"
      ),
      
      icons = c("insert_chart") # "view_list"
    ) ,
    
    # br(), 

    material_row(       style="padding-left: 10px;" ,
                        
      material_column( "Location" , width = 12 , 
                        
      material_dropdown( "statePulldown" , "State", 
                         choices = NULL , multiple = FALSE ) ,
      
      material_checkbox("countiesYN" , "Display county-level data", initial_value = TRUE ) ,
                        
      material_dropdown( "countyPulldown" , "County", 
                         choices = NULL , multiple = TRUE ) ,
    
      material_checkbox( 'topYN' , "Filter to top ...", initial_value = TRUE ) ,
      
      material_slider( 'top' , "Filter to top...(1-100)" , 
                     min_value = 1 , max_value = 200 , initial_value = 5 ) 
    
    ) ) ,
    
    material_row(       style="padding-left: 10px;" ,
                        
      material_dropdown( "variable" , "Variable (may select multiple)", 
                         choices = c( "cumulativeCases" , 
                                      "cumulativeCaseIncidence" ,
                                      "dailyCases",
                                      "dailyCaseIncidence" ,
                                      "dailyActiveCases" , 
                                      "dailyActiveIncidence" ,
                                      "cumulativeDeaths" , 
                                      "cumulativeMortality" ,
                                      "dailyDeaths",
                                      "dailyMortality") ,
                         selected = c( "dailyCases","dailyCaseIncidence" ) ,
                         multiple = TRUE 
                         )
      ) ,

    material_row(       style="padding-left: 10px;" ,
                        
      material_column( "Transformations" , width = 12 , 
        
        material_checkbox( 'scale' , 'Log(e)' , initial_value = FALSE )  ,       
                        
        material_slider( "movingAverage" , "Moving average (days)", 
                         min_value = 1 , max_value = 14 ,
                       initial_value = 3
                         )
      ) ) ,
    # br() , 
    # Models
    material_row(       style="padding-left: 10px;" ,
                        
      material_checkbox( "modelYN" , "Add Model", 
                         initial_value = FALSE
                         )
      ) ,
    material_row(       style="padding-left: 10px;" ,
                        
      material_dropdown( "model" , "Model type", 
                         choices = c( "ARIMA" ,"ETS" , # "STL" , 
                                      # "TSLM" , 
                                      "NNETAR" , "Spline") ,
                         selected = "Spline"
                         ) ,
      material_checkbox( "forecastYN" , "Add Forecast", 
                         initial_value = FALSE
                         ) ,
      material_checkbox( "precastYN" , "Show Precast", 
                         initial_value = FALSE
      )
      )
    
     )   ,
  
   # MAIN window
    material_row( align = 'left' , 
                  
      material_column( width = 6 , 
                       h5( textOutput( "stateTextOutput" ) ) 
                       # h5( textOutput( "source" )  ) 
                       ) ,
      material_column( width = 6 , 
                       
        # material_button( 'usaFacts' , 'Fetch USA Facts data' ) ,
        h5( textOutput( "lastDate" ) )
        ) ,
      
     # material_column( width = 6 , 
     #    material_file_input( 'dataFile' , 'Select data file*' ) ,
     #  ) ,    
     
    
      ) ,
  
   # TAB Modules -- content for main window
    material_side_nav_tab_content(
      side_nav_tab_id = "county_data",
      county_data_UI( 'countyDataModule' )
    ) 
  
   # , material_side_nav_tab_content(
   #    side_nav_tab_id = "data_table",
   #    county_data_UI( 'countyDataModule' )
   #  ) 

)

# Server ####

server <- function( input, output, session ) {
  
  # stop shiny when browser closes
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # load data 
  
   # uploaded county data ####
    data_file <- reactive({
      
      if (!is.null(input$dataFile)){
      
        inFile <- input$dataFile
    
        inFile$datapath
        
      } else {
        NULL
      }
    })
    
     allCountyData = reactive({
   
           # USA Facts
           # 1. if data file exists, load it.  Display last day available
         
           if ( file.exists( 'usaFacts.rds') ){
             
             print( 'loading usa data' )
             usa = readRDS( 'usaFacts.rds' )
             lastDate = max( usa$date , na.rm = TRUE ) 
             output$lastDate = renderText( paste( "Most recent data:" , as.character(lastDate) ) )
             
           } else { 
             
             print( 'usa data file not found' )
             lastDate = ymd( "2020-01-22") 
             output$lastDate = renderText( "no data" )
             
             }
           
           # 2. Update missing data 
           if ( ymd( Sys.Date() ) != lastDate ){
             
              print( 'updating usa data')
             
              if ( !file.exists( 'api.txt' )){ 
                
                output$lastDate = renderText( "API key needed to access data. Looking for key in file, 'api.txt'" )
 
                material_modal('noApi', 
                               button_text = 'require api key' ,
                               title="No API file found" )
                return()
              }
                
              
              key = read_lines( 'api.txt' )
              
              days = seq( lastDate + days(1) , ymd( Sys.Date() ), by="days" ) 
              
              print( 'getting data for this number of days') ; print(length(days))
              
              # material_modal( 'downloding' , 'ok' , 'Fetching Data from USAFacts' )
              
              # Get data via API
              usa.new =
                 withProgress( 
                   message = "Fetching data from USAFacts\n" ,
                   detail = 'a sepearate request for each day' ,
                   value = 0 ,
                   {
                     future_map_dfr( days , ~{ 
                       url =  paste0( "https://data.usafacts.org/covid-nineteen?api-key=" ,
                                                key , "&date=" , .x  )
                       d = get( url ) 
                       if ( !is.null(d) ) d = d %>% mutate( date = ymd( .x ) ) 
                       incProgress( 1 / length( days ) )
                     } )
                     }
              )
           
           if ( exists( 'usa.new' ) ) {
             
             print( 'creating updated usa data file' )
             
             if ( exists( "usa" ) ){
               usa = bind_rows( usa , usa.new ) 
             } else {
               usa = usa.new
             }
             
            lastDate = max( usa$date , na.rm = TRUE ) 
             
             # save data to file 
             saveRDS( usa , 'usaFacts.rds' )
           }

           print( 'usa to data' )
           data = usa %>% 
              rename( state = stateName , 
                      county = countName , 
                      cases = confirmed )  %>%
              mutate( 
                countyFipsCode = ifelse( is.na( countyFipsCode ) , "000" , countyFipsCode) ,
                fips = paste0( stateFipsCode , countyFipsCode )
                ) 
            
            print( 'data' ) ; 
            glimpse( data )
     
           }  
                  
            return( data )
        
       })
     
     states = reactive({ 
       req( allCountyData() )
       
       # print('test')
       # glimpse( allCountyData()  )
       st = allCountyData() %>% arrange( state ) %>% pull( state ) %>% unique 
  
       return( st )
       })
 
    observeEvent( states()  , {
       # print( states() )
       update_material_dropdown( session, input_id = 'statePulldown' ,
                                 choices =  c( 'US' , states() ) ,
                                 value = 'US'
                                 ) 
     })
    
    # turn off model when selecting new state
    observeEvent( input$statePulldown , {
      update_material_checkbox( session, input_id = "modelYN" , value = FALSE )
    })
  
  counties = reactive({ 
     req( allCountyData() )
     req( input$statePulldown )

     cty = allCountyData() %>% 
       filter( state %in% input$statePulldown) %>% 
       arrange( county ) %>%
       pull( county ) %>% unique 
     
     return( cty )
     })
   
  observeEvent( counties()  , {
     # print( states() )
     update_material_dropdown( session, input_id = 'countyPulldown' ,
                               choices =  c( 'ALL' , counties() ) ,
                               value = 'ALL'
                               )
   })
 
  # Select county data ####  
  selectedCountyData = reactive({
    
      req( input$countyPulldown  )
      req( input$variable  )
     
     print( input$countyPulldown  )
     
     print( 'allCountyData before selection') ;
     # glimpse( allCountyData() )
     
     # if needed, add Lat/Long and pop
     if ( !all( c('lat', 'long') %in% names( allCountyData()  ) ) ){
       
      geoFIPS = readRDS( 'geoFIPS.rds') %>% select( fips, lat, long , pop )
      
      d = allCountyData()  %>% left_join( geoFIPS , by = "fips" ) 
      
      print( 'allCountyData after adding geo') ; 
      # glimpse( d )
  
    } else {
      
      d = allCountyData()
    }
     
     if ( input$statePulldown != 'US' ){
       
          print( 'state filter' )
          d = filter( d , state %in% input$statePulldown  ) 
     
     } 
     
     if ( input$countyPulldown != 'ALL' ){
       
          print( 'county filter' )
          d = filter( d,  county %in% input$countyPulldown )
     }
     
     # CountiesYN 
     if ( ! input$countiesYN ){

       # Aggregate by state
       d = d %>% ungroup %>%
         mutate( fips = str_sub( fips, 1, 2 ) ,
                 county = "" ) %>%
         group_by( state , county , fips , date ) 
       
        # get latlong before summarising
        latlong = d %>% 
          mutate_at( vars( lat, long ), as.numeric ) %>%
          group_by( state , county , fips ) %>%
          summarise_at( vars( lat, long ), mean, na.rm = TRUE )
              
        d = d %>%
         summarise_at( vars(cases, deaths, recovered , pop ) , 
                       sum , na.rm = TRUE )  %>%
         ungroup() %>%
         left_join( latlong , by = c( 'state' , 'county' , 'fips' ) )
     
       print( 'state' ); glimpse( d )
     
       } else {

      # Remove unassigned or not linked with location...may cause duplicates
      d = d %>% filter( ! county %in% 'Unassigned' )
   
     } 
     
     print( 'defining variables') 
     d = d %>% 
       group_by( state, county, fips ) %>%
       arrange( state, county, fips , date ) %>%
       mutate( 
         recoveredCases = cumsum( ifelse( is.na( recovered ) , 0 , recovered ) ) , 
         cumulativeCases = cases ,
         cumulativeCaseIncidence = cases * 1e5 / pop ,
         dailyCases = difference( cumulativeCases ) ,
         dailyCaseIncidence = dailyCases * 1e5 / pop ,
         incidence = dailyCaseIncidence , # used to classify high plateau, need in every row, not of pivot
         dailyActiveCases = cumulativeCases - recoveredCases ,
         dailyActiveIncidence = dailyActiveCases * 1e5 / pop
               
          ) 
     
     print( paste('has deaths' , 'deaths' %in% names( d )  )) 
     if ( 'deaths' %in% names( d ) ){
       d = d %>%
       mutate( cumulativeDeaths = deaths ,
               cumulativeMortality = deaths * 1e5 / pop ,
               dailyDeaths = difference( cumulativeDeaths ) ,
               dailyMortality = dailyDeaths * 1e5 / pop 
          )
     }
     
     # TEST
     # saveRDS( d , 'test_data.rds')
     
     # select Variable
     # vars = rlang::syms( input$variable ) 
     # print( 'vars'); print( input$variable ) ; 
     glimpse( d )
     print( 'pivoting longer');
     
     # Pivot longer 
     d = d %>% 
      pivot_longer( cols = starts_with( input$variable  ) ) %>%
      dplyr::select( state, county, fips, date, cases, deaths, recovered, incidence ,
              lat, long , pop , name, value )
     
     
     glimpse( d )
     
    print( 'tsibble' )
    d = d %>% 
      as_tsibble( key = c(state, county , fips , name ) , 
                             index = date ) 
    # glimpse( d ) 
    
    # Moving average
    print( 'moving average' )
    material_spinner_show(session, "moving_average")
    # open_material_modal( session , 'message')
    
    d = d %>%
       group_by( state, county , fips, name )  %>%
       mutate_at( vars( value ) ,
         ~ slider::slide_dbl( .x, 
                              mean, na.rm = TRUE ,
                              .before = input$movingAverage - 1)
               ) 
    material_spinner_hide(session, "moving_average")
    # close_material_modal( session , 'message')
    
     # Scale
     print( 'log(e)' )
          glimpse( d )
     if (input$scale ){
           d = d %>%
             group_by( state, county , fips , name )  %>%
             mutate_at( vars( value  ), ~log( .x + 1 ) ) 
     }

    
     # All - Top
     if ( input$countyPulldown %in% 'ALL' & input$topYN ){
       
       print( 'top filter' )
       print( 'is tsibble ') ; print( is_tsibble( d ) )
       
       top = d %>% 
         as_tibble %>%
         group_by( state, county, fips, name ) %>%
         arrange( state, county, fips, name , desc( date ) ) %>% 
         filter( row_number() == 1 ) %>% # most recent day
         ungroup() %>%
         group_by( name ) %>%
         arrange( desc( value ) ) %>%
         select( state, fips, county, name  ) %>% 
         filter( row_number() <= input$top ) # top 100 
        
        print( 'top selection') ; 
        # glimpse( top )
       
        d = semi_join( d, top , 
                       by = c('state' , 'county' , 'fips' , 'name') 
                       ) %>%
          mutate( value = ifelse( is.nan( value ) , NA , value ) ) 
        
          print( 'after top duplicates' )
          # glimpse( duplicates( d , 
          #       key = c( state, county, fips , name ) , 
          #       index = date ) )
    
          # TEST
          # saveRDS( d , 'test_data.rds')
          
          d = d %>% 
          as_tsibble( key = c(state, county , fips , name ) , 
                             index = date ) %>%
            fill_gaps()
          
          print( 'is tsibble ') ; print( is_tsibble( d ) )
     }
    
    print( 'd' )
    # glimpse( d )

     return( d )
   })
   
  # Model ####
  model = reactive({
    
    req( selectedCountyData() )

    print( 'model d') ; 
    d = selectedCountyData() 
    
    # print( d )

    # ensure no missing values:  set NA to 0 
    d$value[ is.na( d$value) ] = 0
    
    # if ( input$modelYN & input$countyPulldown != 'ALL' ){
    if ( input$modelYN  ){
      
      # ETS
      if ( input$model %in% 'ETS' ){ 
        m = d %>%
          fabletools::model(. ,  ets = ETS( value ) )  
      }
      
      # STL
      if ( input$model %in% 'STL' ){ 
        m = d %>%
          fabletools::model(. ,  stl = STL( value  ~ trend( window = 7 )) )  
      }
      
      # ARIMA
      if ( input$model %in% 'ARIMA' ){ 
        m = d %>%
          fabletools::model( . ,  arima = ARIMA( value ) )  
      }
      
      # NNETAR
      if ( input$model %in% 'NNETAR' ){ 
        m = d %>%
          fabletools::model(. , nnetar = NNETAR( box_cox( value, .1 ) ,  period = '7 days'  ) ) 
      }
      
      # TSLM
      # if ( input$model %in% 'TSLM' ){ 
      #   m = d %>%
      #   model( tslm = TSLM( value  ) )  %>%
      #   augment %>%
      #   mutate( value = ifelse( .fitted < 1 , 0 , .fitted ) )
      #   }
      
      # Spline
      if ( input$model %in% 'Spline' ){ 
        
        t = d %>%
          select( state, county , fips, date, name, value  ) %>%
          group_by( state, county , fips , name ) %>%
          nest( data = c( date, value ) ) 
        
        tss = t %>% mutate( ss = map( data , 
                                      ~smooth.spline( x = data[[1]]$date, 
                                                      y = data[[1]]$value , 
                                                      spar = .5 ) )
                            # ~smooth.spline( x = .x$date , y = .x$value , spar = .5 ) )
                            # ~smooth.spline( x = date , y = value , spar = .5 ) )
        )
        
        m = tss
      }
      
      return( m )
      
    } else { return( NA ) }
        
      
  })
  
  # Model data ####
  modelData = reactive({
    req( selectedCountyData() )
    
    d = selectedCountyData() 
    
    # ensure no missing values:  set NA to 0 
    d$value[ is.na( d$value) ] = 0
    
      if ( input$modelYN ){
        
      print( 'model data')  
      
      if ( input$model %in% 'STL' ){
        
        md = model()  %>%
          components() %>%  
          mutate( value = ifelse( .fitted < 1 , .fitted , .fitted ) )
        
      }
      
      if ( input$model %in% 'Spline' ){ 
        
        md = model()
        
          print( 'spline m' ) ; glimpse( md ) ; saveRDS( md, 'spline_m.rds')
          
          tssa =  map_df( md$ss , augment ) %>% 
          rename( value = .fitted )
          # mutate( value = ifelse( .fitted < 1 , 0 , .fitted ) )
            
          print( 'tssa' ) ; glimpse( tssa ) ; saveRDS( tssa, 'tssa.rds')
          
          md =
            bind_cols( d %>% select( state, county , fips, date, name ) ,
                      tssa ) %>%
            arrange( state, county , fips , name , date ) %>%
            as_tsibble( index = date, key = c( county, state, fips, name ) ) 
      }
      
      if ( ! input$model %in% c('Spline', 'STL') ){ 
          
      md = model()  %>%
        augment %>%  
        mutate( value = ifelse( .fitted < 1 , .fitted , .fitted ) )
      
      }
        
      # print( input$model )
      # glimpse( md )
      

    } else { md = NA }
    
    return( md )
  })

  # FORECASTING ####
    forecastData = reactive({

    if ( input$modelYN & ( input$forecastYN | input$precastYN ) ){ 
      
      print( 'forecasting...')
      
      d = selectedCountyData() %>%
        # pivot_longer( cols = starts_with( input$variable  ) ) %>%
        mutate( value = ifelse( is.na( value ) | is.nan( value ) , 0 , value ) ) 
      
      maxDate = max( d$date )
      days_previous = 14 
      model_period = '7 days'
      
      
      if ( input$model %in% 'NNETAR' ){
        
         print( 'forecast: input model in NNETAR' )
       
        if ( input$forecastYN ){ 
          
          m = model()
          
          f = m %>% forecast( h = '2 weeks' , times = 10 ) 
          
        } else { f = NULL }
        
        if ( input$precastYN ){
          
          d.pre = d %>% filter( date < ( maxDate - days(days_previous)) )
          
          m.pre = d.pre %>%
            model( nnetar = NNETAR( log( value + 1 ) , period = model_period ) )
          
          f.pre = m.pre %>% forecast( h = '2 weeks' , times = 10 ,
                                        new_data = d %>% 
                                          filter( date >= (maxDate - days(days_previous)) )
            )
            
          } else { f.pre = NULL } 
        
        } else { return() }

    return( list( forecast = f , precast = f.pre ) )
      
    } else { return() }
  })
    
  # Source information ####
   sourceText = reactive({
     if ( is.null( data_file() ) ){
     # sourceText = "* Default data is from NYT https://github.com/nytimes/covid-19-data "
     sourceText = "Data source is USAFacts (https://data.usafacts.org)"
   
     } else { 
       sourceText = "" }
   })
   
   output$source = renderText( sourceText() )
   
   output$stateTextOutput = renderText( 
     if ( input$statePulldown %in% 'US' ){ 
       paste( "US (top " , input$top , "counties)" )
     } else { input$statePulldown }
   )
  
  # Load modules ####
  
   callModule( county_data , "countyDataModule" ,
               data  = reactive( selectedCountyData() ) ,
               model  = reactive( modelData() ) ,
               forecastData = reactive( forecastData()$forecast ) ,
               precastData = reactive( forecastData()$precast ) ,
               input_variables = reactive( input$variable ) ,
               movingAverageDays = reactive( input$movingAverage )
               )
   
}


# Run the application 
shinyApp(ui = ui, server = server)

