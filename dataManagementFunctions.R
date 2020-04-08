
# test
# d <- read_excel( "CDC_HotSpot_02APR20.xlsx" , sheet = "Case Count" )

# glimpse(d)

decipherDate = function( x ){ 
  x = ifelse( x %in% "2152020...31" , "2142020" , x )
  x = ifelse( x %in% "2152020...32" , "2152020" , x )

  year = str_sub( x , -4, -1 )
  day = str_sub( x , -6, -5 )
  month = str_sub( x , 1, nchar( x )-6 )
  month = ifelse( nchar( month ) == 1 , paste0( 0 , month ) , month )
  
  date = ymd( paste0( year , month, day  ) )
  return( date )
}

fivecharCountyFIPS = function( x ){
  ifelse( nchar(x) == 4 , paste0( "0" , x ) , x )
}

cipher_place = function( df ){
  # take raw data in wide form and data in long form
  # 1. Tibble with county names, location, population (d_place)
  # 2. Tibble with daily counts in long form (d_data)
  
  descriptive_cols = c( 'new_county_name' , 'new_state_abbreviation',
                        'geohash_lat' , 'geohash_long' , 
                        'new_respop72018' , 'stateFIPS' , 'countyFIPS' 
          )

  data_cols =  setdiff( names(df), descriptive_cols )
  
  # Tibble with county names, location, population
  d_place = df %>% select( !! descriptive_cols ) %>%
    rename( county = new_county_name ,
            state = new_state_abbreviation ,
            lat = geohash_lat ,
            long = geohash_long ,
            pop = new_respop72018 
            ) %>%
    mutate(
      countyFIPS = fivecharCountyFIPS( countyFIPS ) ,
    )
  return( d_place ) 
}

# test 
# cipher_place( d ) 

tidyCipher = function( df ){
  # take raw data in wide form and data in long form
  # 1. Tibble with county names, location, population (d_place)
  # 2. Tibble with daily counts in long form (d_data)
  
  descriptive_cols = c( 'new_county_name' , 'new_state_abbreviation',
                        'geohash_lat' , 'geohash_long' , 
                        'new_respop72018' , 'stateFIPS' , 'countyFIPS' 
          )

  data_cols =  setdiff( names(df), descriptive_cols )
  
  # Tibble with county names, location, population
  d_place = df %>% select( !! descriptive_cols ) %>%
    rename( county = new_county_name ,
            state = new_state_abbreviation ,
            lat = geohash_lat ,
            long = geohash_long ,
            pop = new_respop72018 
            ) %>%
    mutate(
      countyFIPS = fivecharCountyFIPS( countyFIPS ) 
    )
  
  # table with daily counts
  d_data = df %>% 
    select( - !! descriptive_cols ,  countyFIPS ) %>%
    pivot_longer( cols = -countyFIPS  , names_to = 'day' , values_to = 'cases' ) %>%
    mutate( date = decipherDate( day ) ,
            cases = as.integer( cases ) ,
            countyFIPS = fivecharCountyFIPS( countyFIPS ) 
            )
  
  data = inner_join( d_place , d_data , by = "countyFIPS" ) %>%
    rename( fips = countyFIPS )
  
  return( data ) 
}

# test
# data  = tidyCipher( d )
# glimpse( data )

data_ts = function( data ){
  # convert tidy data to time-series (tsibble)
  ts = data %>%
    arrange( state, county, date ) %>%
    as_tsibble( key = c(state, county ), index = date )
  return( ts )
}

# test
# ts  = data_ts( data )
# ts