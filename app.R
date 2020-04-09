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
        # , "Place Holder 2" = "place_holder_2"
      ),
      
      icons = c("insert_chart")
    ) ,
    
    br() , br() ,
    
    material_row( 
      material_dropdown( "statePulldown" , "State", 
                         choices = NULL , multiple = FALSE )
      ) ,
    br() , 
    material_row( 
      material_dropdown( "countyPulldown" , "County", 
                         choices = NULL , multiple = FALSE )
      ) ,
    br() ,
    
    material_row( 
      material_dropdown( "variable" , "Variable", 
                         choices = c( "cumulativeCases" , 
                                      "cumulativeIncidence" ,
                                      "dailyCases",
                                      "dailyIncidence") ,
                         selected = "cumulativeCases"
                         )
      ) 
    )  ,
  
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
    #   side_nav_tab_id = "place_holder_2",
    #   tags$h1("wait for it")
    # ) 
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
                               choices =  states() ,
                               value = states()[1]
                               )
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
     
    if ( !all( c('lat', 'long') %in% names( allCountyData()  ) ) ){
      
      geoFIPS = readRDS( 'geoFIPS.rds') %>% select( fips, lat, long , pop )
      d = allCountyData()  %>% left_join( geoFIPS , by = "fips" ) 
      
    } else {
      
      d = allCountyData()
    }
     
     d = d %>% 
       filter( state %in% input$statePulldown  ) %>% 
       group_by( state, county ) %>%
       arrange( state, county, date ) %>%
       mutate( cumulativeCases = cases ,
               cumulativeIncidence = cases * 1e5 / pop ,
               dailyCases = difference( cumulativeCases ) ,
               dailyIncidence = dailyCases * 1e5 / pop 
          ) %>%
       mutate( cases = !!rlang::sym( input$variable ) )
     
     if ( input$countyPulldown != 'ALL' ){
       
          d = filter( d,  county %in% input$countyPulldown )
     }
     
     return( d )
   })
   

   # Source information ####
   sourceText = reactive({
     if ( is.null( data_file() ) ){
     sourceText = "* Default data is from NYT https://github.com/nytimes/covid-19-data "
   } else { 
       sourceText = "" }
   })
   
   output$source = renderText( sourceText() )
   
   output$stateTextOutput = renderText( input$statePulldown )
  
  # Load modules ####
  
   callModule( county_data , "countyDataModule" ,
               data  = reactive( selectedCountyData() )
               )
   
}


# Run the application 
shinyApp(ui = ui, server = server)

