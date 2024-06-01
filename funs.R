# This script prepares a set of general functions that are to be used in most reports

# list required packages
library(tidyverse)


# SUMMARIES  ----

# takes any number and prints it as a character with specified number of decimals
rprint <- function(x, dec = 2) sprintf( paste0( "%.", dec, "f" ), round( x , dec ) )

# takes any number and removes zero lead
decimalcz <- function(x) gsub( ".", ",", x, fixed = T )

# preparing descriptive tables
preptab <- function(var, lab, data, cond, type = "cont", dec = 2) {
  
  # summarise via central tendency and variance estimates
  sum_print <- function( x, t = "cont", d = 2 ) {
    
    # round it
    if( t != "cat" ) x <- rprint( x, d )
    
    # separator
    if( t == "cont") sep <- " ± "
    else if( t == "count" ) sep <- " ("
    else if( t == "cat" ) sep <- "/"
    
    # collapse the data
    out <- paste( x , collapse = sep )
    if( t == "count" ) out <- paste0( out, ")" ) # for count data add bracket at the end
    
    # return the output
    return(out)
    
  }
  
  # extract categories for categorical variables
  cats <-
    if ( is.numeric(data[ , var ]) ) min( data[ , var ], na.rm = T ):max( data[ , var ], na.rm = T )
    else sort( unique( data[ , var ] ) )
  
  # loop through all number of classes cleared by participants
  sapply(
    
    sort( na.omit( unique( data[ , cond ] ) ) ), # need to sort it
    function(x)
      
      # glue 'mean ± sd' or 'median (IQR)' or 'freq1/freq2/ ...' for continuous, count and categorical variables respectively
      if( type %in% c("cont","count") ) sapply(
        
        # glue continuous/count variables
        if( type == "cont" ) c("mean","sd") else if( type == "count" ) c("median","IQR"),
        function(f)
          do.call( f, list( data[ data[ , cond ] == x, var ], na.rm = T ) )
        
        #
      ) %>% sum_print( . , type, dec ) else if(type == "cat" ) sapply(
        
        # glue frequencies
        cats,
        function(i) table( data[ data[ , cond ] == x, var ] )[ as.character(i) ] %>% ifelse( is.na(.), "0", . )
        
      ) %>% sum_print( ., type )
  
  ) %>%
    
    # finishing touches
    t() %>%
    as.data.frame() %>%
    `dimnames<-`( list( x = lab, y = levels( data[ , cond ] ) ) ) %>%
    return()
  
}

# pull single variable tables together
printab <- function(vars, data, cond, dec = 2) {
  
  lapply( 1:nrow(vars),
          function(i)
            preptab( vars[i,1], vars[i,2], data, cond, vars[i,3], dec )
        ) %>%
    # join by the number of classes undergone
    do.call( rbind.data.frame, . ) %>%
    return()
  
}