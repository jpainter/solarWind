#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Load libraries ####
library( shiny )
library( shinyjs )
library( shinymaterial )

library( tmap )
library( leaflet )
library( mapview )
library( sf )
library( rmapshaper )

library( RColorBrewer )
library( plotly )
library( tidyverse )
library( fabletools ) 
library( scales )
library( knitr )
library( rlang )
library( stringi )
library( tidyselect )
library( knitrProgressBar )
library( DT )
library( readxl )
library( openxlsx  )
library(readr)
library( anytime )
library( lubridate )


# load modules ####
source( 'county_data.R' )
source( 'dataManagementFunctions.R' )

# Define UI #####
ui <- material_page(

    title = "Coronal Spread" ,
    nav_bar_fixed = TRUE ,
    useShinyjs(),
    include_fonts = T,
    nav_bar_color = "blue" ,
 
  # SIDEBAR 
  material_side_nav(
    # image_source = "side_nav.jpeg",  
    fixed = TRUE,
    
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
      material_dropdown( "state" , "State", 
                         choices = c('CT', 'AZ', 'NY' ) , multiple = TRUE )
      ) ,
    br() , br() ,
    
    )  ,
  
   # MAIN window
    material_row(
        material_file_input( 'dataFile' , 'Select data file*' ) ,
        textOutput("source")
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
    
   countyData = reactive({
 
       if ( is.null( data_file() ) ){
          # source: https://github.com/nytimes/covid-19-data
          url =  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
          nyt = read_csv( url ) 
          glimpse( nyt )
          return( nyt )
       }
     
        d <- read_excel( data_file() , sheet = "Case Count" ) # data_file() )
        data  = tidyCipher( d )
        glimpse( data )
        return( data )
     })
   
   states = reactive({ 
     req( countryData() )
     countryData() %>% pull( state ) %>% unique 
     })
   
   # observe( states() ,
   #   update_material_dropdown( 'states' , 
   #                             choices =  states(), 
   #                             value = states()[1]
   #                             )
   # )
   # 
   output$source = renderText( "* Default data from NYT, https://github.com/nytimes/covid-19-data")
   # Load modules ####
  
   callModule( county_data , "countyDataModule" ,
               data  = countyData() )
   
}


# Run the application 
shinyApp(ui = ui, server = server)

