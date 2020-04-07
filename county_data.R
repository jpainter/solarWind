# county_data_module

# Helper functions

# User Interface ####
county_data_UI <- function( id ) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # tagList(
  # 
  #   tabsetPanel(type = "tabs",
  #               
  #               tabPanel("", 
  #                        material_file_input( ns('dataFile'), ' Select county data file (.csv)' 
  #                                      ) 
  #                        ) , 
  #                       
                material_row(
                  material_column( width = 5, offset = 1 ,
                                   plotlyOutput( ns('chartTS') )
                                   )
                  )
                         
  #                        ) 
  # )
}

# Server function ####
county_data <- function( input, output, session, data ) {

  countyData = reactive({ data })
  
  countyCount = reactive({
    
    req( countyData() )

    cd = count( countyData() , state   )
    
    return( cd )

  }) 
  
  countyTS = reactive({
    req( countyData() )

    ts = data_ts( countyData() )
  })
  
  # ggplot of time-series
  ggplotTS = reactive({
     req( countyTS )
    
      countyTS() %>% 
      filter( 
        # testing
        row_number() < 100 
        # state %in% 'CT'
        ) %>%
      autoplot( cases  ) +
      theme_minimal() 
  })

    
### output count of county by state  ####

  output$chartTS = renderPlotly(  ggplotly( ggplotTS() ) )
    
  output$variables = renderTable(

    iris %>% colnames ,
  )
  
  output$countyCount =  renderTable(
    
    if( nrow( countyCount() ) > 0 )  countyCount()
  )
  
  return( list(  ) )
    
}

