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
                                   leafletOutput( ns('map') )
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

    dataTS() %>%
    autoplot( cases ) +
    scale_x_date(date_labels = "%m/%d") +
    theme_minimal() +
    labs( x = "" ) +
    guides( color = FALSE )
      
  })

  tmapData = reactive({ 
    req( data() )
    print( paste( 'data has row' , nrow( data() ) ) )
    if ( nrow( data() ) == 0 ) return( NULL )
      
    # if missing lat/lonfg, add in
    if ( !all( c('lat', 'long') %in% names( data() ) ) ){
      geoFIPS = readRDS( 'geoFIPS.rds') %>% select( fips, lat, long  )
      d = data() %>% left_join( geoFIPS , by = "fips" ) 
      print( paste( 'd+geo' , nrow( data() ) ) )
      
    } else {
      print( paste( 'd' , nrow( data() ) ) )
      d = data()
      glimpse( d )
    }
    
    dcum = d %>% 
      group_by( fips , county ) %>% 
      summarise( 
        cumCases = max( cases, na.rm = T ) ,
        lat = max( lat ) ,
        long = max( long )
        ) %>%
      filter( !is.na( lat ) , !is.na( long ) ) 
  
    # convert to sf
    dsf = st_as_sf( dcum , coords = c( "long", "lat" ) ) 

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
        tm_dots( size = 'cumCases' , col = 'cumCases' , alpha = .5 )
      tmap_leaflet(tm)
    })
   
  output$countyCount =  renderTable({
    
    req( countyCount() )
    countyCount()
  })
  
  return( list(  ) )
    
}

