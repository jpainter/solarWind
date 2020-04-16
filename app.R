#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Libraries ####
# Function to test if package is installed

libraries = readLines( con = file( 'requirements.txt' ) , warn=FALSE )
libraries = gsub(" ", "" ,  libraries)

pkgTest <- function( package.list = libraries ){

  missing.packages = setdiff( package.list , rownames( installed.packages() ) )

  if ( length( missing.packages ) > 0 & nchar( missing.packages[1] ) ){
    print( missing.packages )

        install.packages( missing.packages
                          # , dependencies = TRUE ,
                          # , type="source" ,
                          # , repos = "https://cran.rstudio.com"
                          )
    }

}

# Test if packages installed
pkgTest( libraries )

# load the packages
suppressMessages(
  lapply( libraries , require  , character.only = TRUE)
)


# load modules ####
source( 'county_data.R' )
source( 'dataManagementFunctions.R' )

# Define UI #####
ui <- material_page(
    
    title = "Solar Wind and Coronal Hot Spots" ,
    nav_bar_fixed = TRUE ,
    useShinyjs(),
    include_fonts = T,
    nav_bar_color = "blue" ,
 
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
                        
      material_dropdown( "countyPulldown" , "County", 
                         choices = NULL , multiple = FALSE ) ,
    
      material_slider( 'top' , "Filter to top...(1-100)" , 
                     min_value = 1 , max_value = 100 , initial_value = 10 ) 
    
    ) ) ,
    
    br() ,
    
    material_row(       style="padding-left: 10px;" ,
                        
      material_dropdown( "variable" , "Variable", 
                         choices = c( "cumulativeCases" , 
                                      "cumulativeCaseIncidence" ,
                                      "dailyCases",
                                      "dailyCaseIncidence" ,
                                      "cumulativeDeaths" , 
                                      "cumulativeMortality" ,
                                      "dailyDeaths",
                                      "dailyMortality") ,
                         selected = "cumulativeCaseIncidence" ,
                         multiple = TRUE 
                         )
      ) ,

    material_row(       style="padding-left: 10px;" ,
                        
      material_column( "Transformations" , width = 12 , 
        
        material_checkbox( 'scale' , 'Scale data' , initial_value = FALSE )  ,       
                        
        material_slider( "movingAverage" , "Moving average (days)", 
                         min_value = 0 , max_value = 14 ,
                       initial_value = 0
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
                         choices = c( "ARIMA" ,"ETS" , "STL" , 
                                      # "TSLM" , 
                                      "NNETAR" , "Spline") ,
                         selected = "NNETAR"
                         )
      )
     )   ,
  
   # MAIN window
    material_row( align = 'center' , 
        material_file_input( 'dataFile' , 'Select data file*' ) ,
        textOutput( "source" ) ,
        h5( textOutput( "stateTextOutput" ) )
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
   
         if ( is.null( data_file() ) ){
            # source: https://github.com/nytimes/covid-19-data
            url =  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
            data = read_csv( url ) 
  
         } else { 
       
           d <- read_excel( data_file() , sheet = "Case Count" ) 
          
           if ( "new_county_name" %in% names( d ) ){
  
             data  = tidyCipher( d )
             print( paste( 'tidyCipher d ', nrow(data)) )
             
           } else {
             
             return( NULL )
           }
            
           # print( paste( 'data rows' , nrow( data )) )
           return( data )
         }
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
                                 value = states()[1]
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
   
  selectedCountyData = reactive({
    
      req( input$countyPulldown  )
      req( input$variable  )
     
     print( input$countyPulldown  )
     
     # if needed, add Lat/Long and pop
     if ( !all( c('lat', 'long') %in% names( allCountyData()  ) ) ){
      
      geoFIPS = readRDS( 'geoFIPS.rds') %>% select( fips, lat, long , pop )
      d = allCountyData()  %>% left_join( geoFIPS , by = "fips" ) 
      
    } else {
      
      d = allCountyData()
    }
     
     if ( input$statePulldown != 'US' ){
       
          d = filter( d , state %in% input$statePulldown  ) 
     
     } 
     
     if ( input$countyPulldown != 'ALL' ){
       
          d = filter( d,  county %in% input$countyPulldown )
     }
     
     d = d %>% 
       group_by( state, county, fips ) %>%
       arrange( state, county, date ) %>%
       mutate( cumulativeCases = cases ,
               cumulativeCaseIncidence = cases * 1e5 / pop ,
               dailyCases = difference( cumulativeCases ) ,
               dailyCaseIncidence = dailyCases * 1e5 / pop 
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
     vars = rlang::syms( input$variable ) 
     print( 'vars'); print( input$variable )
     
     # d = d %>%
     #   ungroup() %>%
     #   mutate( cases = !!rlang::sym( input$variable ) )
     
     # if US, pick top 100 spots
     if ( input$countyPulldown %in% 'ALL' ){
       
        top = d %>% 
         group_by( state, fips ) %>%
          # Max value
          # summarise( cases = max( cases , na.rm = TRUE ) ) %>%
          # latest value
         arrange( desc( date ) ) %>% filter( row_number() == 1 ) %>%
         ungroup %>%
         arrange( desc( cases ) ) %>%
         select( state, fips ) %>% 
         filter( row_number() <= input$top ) # top 100 
        
        d = semi_join( d, top , by = c('state' , 'fips') ) %>%
          ungroup() %>%
          arrange( state, county , fips , date ) %>%
          as_tsibble( key = c(state, county, fips ) , index = date ) 
     }
     
     # print( paste( 'moving average:' , input$movingAverage ))
     
    # Moving average
    d = d %>%
       group_by( state, county , fips )  %>%
       mutate_at( vars( !!! vars ) ,
         ~ slider::slide_dbl( .x, mean, na.rm = TRUE ,
                                         .before = input$movingAverage )
               ) %>% ungroup

         
     # Scale
     if (input$scale ){
           d = d %>%
             group_by( state, county , fips )  %>%
             mutate_at( vars( !!! vars ) , scale ) %>% ungroup
     }
    
    print( 'd' )
    glimpse( d )

     return( d )
   })
   
  
  modelData = reactive({
    req( selectedCountyData() )
    # req( input$modelYN )
    
    d = selectedCountyData() %>% 
      pivot_longer( cols = starts_with( input$variable  ) ) %>%
      arrange( state, county , fips , name, date ) %>%
      as_tsibble( key = c(state, county , fips , name ) , index = date )
    
    print( 'model d') ; glimpse( d )
    
    # ensure no missing values:  set NA to 0 
    d$value[ is.na( d$value) ] = 0

    # if ( input$modelYN & input$countyPulldown != 'ALL' ){
    if ( input$modelYN & input$top <= 5  ){
 
      # ETS
      if ( input$model %in% 'ETS' ){ 
        m = d %>%
        model( ets = ETS( value ) )  %>%
        augment %>%
        mutate( value = ifelse( .fitted < 1 , 0 , .fitted ) )
      }
      
      # STL
      if ( input$model %in% 'STL' ){ 
        m = d %>%
        model( stl = STL( value  ~ trend( window = 7 )) )  %>%
        components() %>%
        mutate( value = ifelse( trend < 1 , 0 , exp( trend ) + 1 ) )
      }
      
      # ARIMA
      if ( input$model %in% 'ARIMA' ){ 
        m = d %>%
        model( arima = ARIMA( value  ) )  %>%
        augment %>%
        mutate( value = ifelse( .fitted < 1 , 0 , .fitted ) )
      }
      
      # NNETAR
      if ( input$model %in% 'NNETAR' ){ 
        m = d %>%
        model( nnetar = NNETAR( value, period = '1 week' ) )  %>%
        augment %>%
        mutate( value = ifelse( .fitted < 1 , 0 , .fitted ) )
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
                    ~smooth.spline( x = data[[1]]$date , y = data[[1]]$value , spar = .5 ) )
                    # ~smooth.spline( x = .x$date , y = .x$value , spar = .5 ) )
                    # ~smooth.spline( x = date , y = value , spar = .5 ) )
) 
        # print( 'tss' ) ; glimpse( tss ) ; saveRDS( tss, 'tss.rds')
        
        tssa =  map_df( tss$ss , augment ) %>%
           mutate( value = ifelse( .fitted < 1 , 0 , .fitted ) )
          
        # print( 'tssa' ) ; glimpse( tssa ) ; saveRDS( tssa, 'tssa.rds')
        
        m = bind_cols( d , tssa ) %>%
          arrange( state, county , fips , name , date ) %>%
          as_tsibble( index = date, key = c( county, state, fips, name ) ) 
        }

    } else { m = NA }
    
    print('model') ; print( input$model )
    glimpse( m )
    
    return( m )
  })

  # Source information ####
   sourceText = reactive({
     if ( is.null( data_file() ) ){
     sourceText = "* Default data is from NYT https://github.com/nytimes/covid-19-data "
   } else { 
       sourceText = "" }
   })
   
   output$source = renderText( sourceText() )
   
   output$stateTextOutput = renderText( 
     if ( input$statePulldown %in% 'US' ){ 
       "US (top 100 counties)"
     } else { input$statePulldown }
   )
  
  # Load modules ####
  
   callModule( county_data , "countyDataModule" ,
               data  = reactive( selectedCountyData() ) ,
               model  = reactive( modelData() ) ,
               input_variables = reactive( input$variable )
               )
   
}


# Run the application 
shinyApp(ui = ui, server = server)

