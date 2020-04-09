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
county_data <- function( input, output, session, data 
                         , states
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

    dataTS() %>%
    autoplot( cases ) +
    scale_x_date(date_labels = "%m/%d") +
    theme_minimal() +
    labs( x = "" , y = "count") +
    guides( color = FALSE )
      
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
    ggplotTS()
    ggplotly( ggplotTS() )  %>% hide_legend()
    })

   output$map = renderLeaflet({ 
      req( tmapData() ) 
      tm = tm_shape( tmapData() ) + 
        tm_dots( size = 'cases' , col = 'cases' , alpha = .5 )
      tmap_leaflet( tm )
    })
   
  output$countyCount =  renderTable({
    
    req( countyCount() )
    countyCount()
  })
  
  return( list(  ) )
    
}

