# This script is supposed to pre-process data of non-verbal memory test from NBD.

rm( list = ls() ) # clean environment

# load required packages
library(here)
library(tidyverse)
library(purrr)


# IN-HOUSE FUNCTIONS ----

# extract numbers from character
char2num <- function(x) as.numeric( gsub("\\D", "", x) )

# adding demographic variables
demo_join <- function( d = d1, f = "1_zena", m = "0_muz", a = "vek_roky", s = "gender" ) {
  
  # list variables to keep
  v <- c( names(d[[ds]]), "vek_roky", "sex" )
  
  # prepare data
  dO <- d %>%
    
    # add relevant data from respective anamnesis files
    left_join(
      read.csv( here("_data",ds,"0_anamneza.csv"), sep = "\t" )[ ,c("kod_ditete",a,s) ],
      by = "kod_ditete"
    ) %>%
    
    mutate( sex = case_when( get(s) == f ~ "f", get(s) == m ~ "m" ), vek_roky = get(a) ) %>%
    mutate( kod_ditete = case_when( paste0("CON",kod_ditete) ) ) %>%

    select( all_of(v) )
  
  # return the new better data set
  return(dO)
    
}


# DATA READ ----

# reading the data
d0 <- list(
  
  Validacni_studie =
    read.csv( here("_raw","3_neverbalni_pamet.csv"), sep = "\t" ) %>%
    mutate( source = "Validacni_studie"),
  
  Sadecka =
    left_join(
      read.csv( here("_raw","Testy.csv"), sep = "\t" ),
      read.csv( here("_raw","Anamnéza.csv"), sep = "\t" ),
      by = "kod_ditete"
    ) %>%
    filter( if_any( starts_with("pok"), ~ !is.na(.) ) ) %>%
    mutate( source = "Sadecka" )

)


# DATA PROCESS ----

emp <- c(1,3,5,7,10,11,13,16,19,22,24) # list empty cells for hs4

## long format data ----

# pivot longer each of the data sets
d1 <- 
  
  lapply(
    
    setNames( names(d0), names(d0) ),
    function(i)
      
      d0[[i]] %>%
      pivot_longer(
        cols = contains("pol"),
        names_to = c("trial", "cell"),  names_sep= "_",
        names_transform = function(x) ifelse( x == "odd", x, char2num(x) ),
        values_to = "score", values_transform = char2num
      )

  )

# extract variables of interest separately for each data set
# basics for normative group
d1$Validacni_studie <-
  
  d1$Validacni_studie %>%
  select( kod_ditete, source, trial, cell, score ) %>%
  left_join(
    read.csv( here("_raw","0_anamneza.csv"), sep = "\t" )[ ,c("kod_ditete","vek_roky","gender") ],
    by = "kod_ditete"
  ) %>%
  mutate( sex = case_when( gender == "1_zena" ~ "f", gender == "0_muz" ~ "m" ) ) %>%
  mutate( kod_ditete = paste0("CON",kod_ditete) ) %>%
  select( -gender )

# some more variables in the Sadecka data set
d1$Sadecka <-
  
  d1$Sadecka %>%
  
  # re-code sex
  mutate(
    sex = case_when( Pohlavi == 0 ~ "f", Pohlavi == 1 ~ "m" ),
    vek_roky = vek.eNAact
  ) %>%
  
  # select variables of interest
  select(
    kod_ditete, source, trial, cell, score, # basic nonverbal memory task scores
    Poradi.adm., Zadani, vek_roky, sex, # demographics
    ROCFT_Kopie, ROCFT_3 , ROCFT_30, #  Rey Osterrieth Complex Figure Test
    starts_with("CMS"), # Children’s Memory Scale
    Matrice_sum, BNT_spont, BNT_kategor, BNT_fonem, kateg_sum, ToM_sum # other tests 
  )

# pull all types of data sets into a single long data frame
d1 <- reduce( d1, bind_rows )
  

## wide data set ----

# prepare a wide data set with three types of sum scores
d2 <-
  
  d1 %>%
  pivot_wider( names_from = cell, names_prefix = "pol", values_from = score ) %>%
  mutate(
    
    # score no.1: two points for getting it totally right only
    hs1_summ = rowSums(
      across(
        contains("pol"),
        ~ ifelse( .x == 0, 2, 0 )
      )
    ),
    
    # score no.2: two points for getting it totally right + one point for correct location with rotated pattern
    hs2_sumrot = rowSums(
      across(
        contains("pol"),
        ~ case_when( .x == 0 ~ 2, .x == 1 ~ 1, .default = 0 )
      )
    ),
    
    # score no.3: combined correct/partially correct responses
    hs3_orig = rowSums(
      across(
        contains("pol"),
        ~ case_when( .x == 0 | .x == 1 ~ 2, .x == 2 | .x == 4 ~ 1, .x == 3 ~ 0)
      )
    ),
    
    # re-code empty cells for hs4
    across( paste0("pol",emp), ~ ifelse( .x == 3, 0, .x ) ),
    
    # score no.4: gestalt score
    hs4_gestalt = rowSums(
      across(
        contains("pol"),
        ~ case_when( .x == 0 | .x == 1 ~ 1, .default = 0 )
      )
    )

  ) %>%
  
  # some more pivoting
  pivot_wider(
    names_from = trial,
    values_from = starts_with("hs"), # keep only pre-processed scores
    id_cols = names(d1)[ !colnames(d1) %in% c("cell","score","trial") ]
  ) %>%
  
  # add sum scores
  mutate(
    hs1_summ_1234 = rowSums( across( starts_with("hs1") & !ends_with("odd") ) ),
    hs2_sumrot_1234 = rowSums( across( starts_with("hs2") & !ends_with("odd") ) ),
    hs3_orig_1234 = rowSums( across( starts_with("hs3") & !ends_with("odd") ) ),
    hs4_gestalt_1234 = rowSums( across( starts_with("hs4") & !ends_with("odd") ) )
  ) %>%
  
  # relocate sum scores
  relocate( hs1_summ_1234, .after = hs1_summ_4 ) %>%
  relocate( hs2_sumrot_1234, .after = hs2_sumrot_4 ) %>%
  relocate( hs3_orig_1234, .after = hs3_orig_4 ) %>%
  relocate( hs4_gestalt_1234, .after = hs4_gestalt_4 )

# extract variable names for other tests that will be used for validations
v <-
  
  read.csv( here("vars.csv"), sep = "\t" ) %>%
  filter( !( test %in% c("0_anamneza","3_neverbalni_pamet") ) ) %>% # get rid of variables already included
  filter( ( rownames(.) %in% c(5,10,14,17,45,80,82,86) ) ) %>% # manually select rows for analysis
  add_column( label = c("VPaU 1-4","VPaU 30 min","Příběhy ok.","Příběhy 30 min","Pracovní pam.","Z-P vnímání","Orientace v P.","ToM") ) %>%
  
  # add rows for nonverbal memory test scores
  add_row( variable = "hs1_summ_1234", type = "cont", test = NA, label = "NP1 Pokus 1-4", .before = 1 ) %>%
  add_row( variable = "hs1_summ_odd", type = "cont", test = NA, label = "NP1 Oddálené vybavení", .after = 1 ) %>%
  add_row( variable = "hs2_sumrot_1234", type = "cont", test = NA, label = "NP2 Pokus 1-4", .after = 2 ) %>%
  add_row( variable = "hs2_sumrot_odd", type = "cont", test = NA, label = "NP2 Oddálené vybavení", .after = 3 ) %>%
  add_row( variable = "hs3_orig_1234", type = "cont", test = NA, label = "NP3 Pokus 1-4", .after = 4 ) %>%
  add_row( variable = "hs3_orig_odd", type = "cont", test = NA, label = "NP3 Oddálené vybavení", .after = 5 ) %>%
  add_row( variable = "hs4_gestalt_1234", type = "cont", test = NA, label = "NP4 Pokus 1-4", .after = 6 ) %>%
  add_row( variable = "hs4_gestalt_odd", type = "cont", test = NA, label = "NP4 Oddálené vybavení", .after = 7 ) %>%
  
  # add rows for Sadecka data set
  add_row( variable = "ROCFT_Kopie", type = "cont", test = "conv", label = "ROCFT kopie" ) %>%
  add_row( variable = "ROCFT_3", type = "cont", test = "conv", label = "ROCFT 3" ) %>%
  add_row( variable = "ROCFT_30", type = "cont", test = "conv", label = "ROCFT 30" ) %>%
  add_row( variable = "CMS_1", type = "cont", test = "conv", label = "CMS 1" ) %>%
  add_row( variable = "CMS_2", type = "cont", test = "conv", label = "CMS 2" ) %>%
  add_row( variable = "CMS_3", type = "cont", test = "conv", label = "CMS 3" ) %>%
  add_row( variable = "CMS_sum123", type = "cont", test = "conv", label = "CMS 1-3" ) %>%
  add_row( variable = "CMS_5_pointer", type = "cont", test = "conv", label = "CMS 5" ) %>%
  add_row( variable = "CMS_sum1235", type = "cont", test = "conv", label = "CMS 1235" ) %>%
  add_row( variable = "CMS6_30min", type = "cont", test = "conv", label = "CMS 30" ) %>%
  add_row( variable = "BNT_spont", type = "cont", test = "div", label = "BNT spont." ) %>%
  add_row( variable = "BNT_kategor", type = "cont", test = "div", label = "BNT kategor." ) %>%
  add_row( variable = "BNT_fonem", type = "cont", test = "div", label = "BNT fonem." ) %>%
  add_row( variable = "kateg_sum", type = "cont", test = "div", label = "Kat. fluence" ) %>%
  add_row( variable = "ToM_sum", type = "cont", test = "div", label = "ToM" )

# add a column with name for the finished data set
v$name <- c( v$variable[1:10], "storiesNOW", "storiesLATE", "wm", "vs_perc", "orient","ToM_nbd", v$variable[17:30],"ToM_sum" )

# keep only relevant rows
d3 <- d2[ , c( 1:4, which( colnames(d2) %in% v$variable ) ) ]

# add rows for NBD variables of interest
for ( i in which( !( is.na(v$test) | v$test %in% c("conv","div") ) ) ) {
  
  d3 <-
    
    d3 %>%
    left_join(
      
      read.csv( here( "_raw", paste0( v[i,"test"],".csv" ) ), sep = "\t" ) %>%
        select( "kod_ditete", v[i,"variable"] ) %>%
        rename( !! sym(v[i,"name"]) := v[i,"variable"] ) %>%
        mutate( kod_ditete = paste0("CON",kod_ditete) ),
      
      by = "kod_ditete"
      
    )
    
  
}

# get rid of duplicates
for ( i in names( table(d3$kod_ditete)[ table(d3$kod_ditete) > 1 ] ) ) d3 <- d3[ -which(d3$kod_ditete == i)[1], ]

# change v's columns for printab() in the report
v$variable <- v$name
v$type <- "cont"
v <- v[ , c(1,4,2) ]
v <- rbind.data.frame( c("sex", "Pohlaví (dívky/chlapci)", "cat" ), c("vek_roky", "Věk (roky)", "cont" ), v )
v$data_set <- c( rep(NA,2), rep("both",8), rep("Data set #2",8), rep("Data set #1",15) )


# SAVE THE RESULTS ----

if (!dir.exists("_data") ) dir.create("_data") # prepare the folder

write.table( x = d1, file = here("_data","long_df.csv"), sep = ",", na = "NA", dec = ".", row.names = F, quote = F )
write.table( x = d2, file = here("_data","wide_df.csv"), sep = ",", na = "NA", dec = ".", row.names = F, quote = F )
write.table( x = d3, file = here("_data","main_df.csv"), sep = ",", na = "NA", dec = ".", row.names = F, quote = F )
write.table( x = v, file = here("_data","vars.csv"), sep = ",", na = "NA", dec = ".", row.names = F, quote = F )
