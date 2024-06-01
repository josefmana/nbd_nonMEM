# This is a script used to calculate models estimating group differences in nonverbal memory test conditional on scoring method.

# clean environment
rm( list = ls() )
gc()

# list required packages into a character object
pkgs <- c( "here", "dplyr", "brms" )

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# read raw data
d0 <- read.csv( here("_data","nonverbal_mem","wide_df.csv"), sep = "," )
