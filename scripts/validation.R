# This is a script used to calculate models estimating convergent & divergent validity metrics.

rm( list = ls() ) # clean environment

# set-up libraries
library(here)
library(tidyverse)
library(pwr)
library(psych)
library(corrplot)
library(lavaan)


d <- read.csv( here("_data","main_df.csv"), sep = "," ) # read raw data
v <- read.csv( here("_data","vars.csv"), sep = "," ) # helpers
source("funs.R") # read in-house functions

sapply( c("figs","tabs"), function(i) if( !dir.exists(i) ) dir.create(i) ) # prepare folders


# DATA DESCRIPTION ----

# re-format source variable which we will be conditioning on
d <- d %>%
  
  mutate(
    source = factor( source, levels = c("Sadecka","Validacni_studie"), labels = paste0("Data set #",1:2), ordered = T ),
    sex = factor( sex, levels = c("f","m"), ordered = T )
  )

# extract number of subjects
n <- sapply( levels(d$source), function(i) as.character( nrow( d[ d$source == i, ] ) ) )

# print it
tab1 <-
  
  printab( v, d, "source", 2 ) %>%
  
  # ToM shinaningans (because it is in both data sets)
  bind_rows( . , data.frame( .[ "ToM1", "Data set #1" ], .[ "ToM", "Data set #2" ] ) %>% `colnames<-`( paste0("Data set #",1:2) ) ) %>%
  filter( ! (rownames(.) %in% c("ToM","ToM1") ) ) %>%
  `rownames<-`( c( rownames(.)[1:(nrow(.)-1)], "ToM" ) ) %>%
  slice( 1:17, n(), 18:(n()-1) ) %>%
  
  # finishing touches
  mutate( across( where(is.character), ~ ifelse( grepl("NA",.x), "-", .x ) ) ) %>%
  mutate( across( everything(), decimalcz ) ) %>%
  rbind( N = n, . ) %>%
  rownames_to_column("Variable")

# save it
write.table( x = tab1, file = here("tabs","data_description.csv"), sep = ";", row.names = F, quote = F )


# CORRELATION ANALYSES ----

# 

# list variables to be included in the validation correlation matrixes
vars <- data.frame(

  nam = c("hs2_sumrot_1234", "hs2_sumrot_odd", "ROCFT_Kopie", "ROCFT_3", "ROCFT_30", "CMS_1", "CMS_2", "CMS_3", "CMS_sum123", "CMS_5_pointer", "CMS_sum1235", "CMS6_30min", "BNT_kategor", "kateg_sum", "ToM_sum"),
  lab = c("NP2 Pokus 1-4", "NP2 Odd. vyb.", "ROCFT kopie", "ROCFT 3", "ROCFT 30", "CMS 1", "CMS 2", "CMS 3", "CMS 1-3", "CMS 5", "CMS 1235", "CMS 30", "BNT kategor.", "Kat. fluence", "ToM")

)

# extract (partial) correlation matrices
pcorr <- lapply(
  
  1:2,
  function(i) list(
    
    r = partial.r(

      data = d[ d$source == paste0("Data set #",i), c(with( v , variable[ data_set %in% c( "both", paste0("Data set #",i) ) ] ), "vek_roky") ],
      x = with( v , variable[ data_set %in% c( "both", paste0("Data set #",i) ) ] ),
      y = "vek_roky",
      use = "pairwise",
      method = "pearson"

    )
    
  )
)

# add p-values
for (i in 1:2) pcorr[[i]]$p <- corr.p(
  
  r = pcorr[[i]]$r,
  n = corr.test(d[ d$source == paste0("Data set #",i), with( v , variable[ data_set %in% c( "both", paste0("Data set #",i) ) ] ) ] )$n - 1, # n - 1 for one covariate
  adjust = "none",
  alpha = .05
  
)$p


## ---- plot ----

# plot it
pcorr[[1]]$r[vars$nam, vars$nam] %>%
  
  `colnames<-`(vars$lab) %>%
  `rownames<-`(vars$lab) %>%
  
  corrplot(
    type = "lower",
    method = "color",
    p.mat = pcorr[[1]]$p[vars$nam, vars$nam] %>% `colnames<-`(vars$lab) %>% `rownames<-`(vars$lab),
    insig = "blank",
    tl.srt = 45,
    tl.col = "black",
    addCoef.col = "white",
    addgrid.col = "grey",
    col = COL1("YlGn"),
    diag = F
  )

# prepare a table with all the variables
t.corr <-
  
  lapply(
    
    1:2,
    function(i) {
      
      r <- na.omit( v[ v$data_set == paste0("Data set #",i), "variable" ] ) # extract row
      c <- na.omit( v[ v$data_set == "both", "variable" ] ) # extract columns
      
      # extract correlations and SEs
      cor <- corr[[i]]$r[r,c] %>% as.data.frame() %>% mutate( across( everything(), ~ decimalcz( rprint(.x, 3) ) ) )
      p <- corr[[i]]$p[r,c] %>% as.data.frame() %>% mutate( across( everything(), ~ ifelse( .x < .001, "<0,001", decimalcz( rprint(.x, 3) ) ) ) )
      
      # extract the table
      sapply( colnames(cor), function(j) paste0( cor[[j]], " (", p[[j]], ")") ) %>%
        as.data.frame() %>%
        `rownames<-`( rownames(cor) ) %>%
        `rownames<-`( v[ v$variable %in% rownames(.), "label" ] ) %>%
        `colnames<-`( v[ v$variable %in% colnames(.), "label" ] ) %>%
        rownames_to_column("test") %>%
        mutate( data_set = paste0("Data set #",i) )
      
    }
    
  ) %>%
  
  # glue the table
  do.call( rbind.data.frame, . )

# save it
write.table( x = t.corr, file = here("tabs","pearson_correlations.csv"), sep = ";", row.names = F, quote = F )

# prepare correlation plots
for ( i in 1:2 ) {
  
  jpeg( filename = here( "figs", paste0("corrplot_dataset_",i,".jpg") ), quality = 100, units = "in", width = 9, height = 9, res = 300 ) # prepare the file
  
  # plot it
  corrplot(
    corr[[i]]$r %>% `colnames<-`( v[ v$variable %in% colnames(corr[[i]]$r), "label" ] ) %>% `rownames<-`( v[ v$variable %in% rownames(corr[[i]]$r), "label" ] ),
    method = "circle",
    type = "lower",
    diag = F,
    tl.srt = 45,
    tl.cex = .85,
    cl.cex = 1,
    tl.col = "black",
    col = COL2("PRGn", 10)
  )
  
  # save it
  dev.off()
  
  
}


#


# FACTOR ANALYSES ----

# set-up models for each scoring type
mod <-
  
  list(
    
    `1` = '
    nonverbm =~ hs1_summ_1234 + hs1_summ_odd + ROCFT_Kopie + ROCFT_3 + ROCFT_30 + CMS_sum123 + CMS6_30min
    language =~ BNT_kategor + kateg_sum
    ToM =~ 1*ToM_sum
    ',
    
    `2` = '
    nonverbm =~ hs2_sumrot_1234 + hs2_sumrot_odd + ROCFT_Kopie + ROCFT_3 + ROCFT_30 + CMS_sum123 + CMS6_30min
    language =~ BNT_kategor + kateg_sum
    ToM =~ 1*ToM_sum
    ',
    
    `3` = '
    nonverbm =~ hs3_orig_1234 + hs3_orig_odd + ROCFT_Kopie + ROCFT_3 + ROCFT_30 + CMS_sum123 + CMS6_30min
    language =~ BNT_kategor + kateg_sum
    ToM =~ 1*ToM_sum
    ',
    
    `4` = '
    nonverbm =~ hs4_gestalt_1234 + hs4_gestalt_odd + ROCFT_Kopie + ROCFT_3 + ROCFT_30 + CMS_sum123 + CMS6_30min
    language =~ BNT_kategor + kateg_sum
    ToM =~ 1*ToM_sum
    '
    
  )

# compute the CFAs
CFA <- lapply( 1:length(mod), function(i) cfa( mod[[i]], data = d, estimator = "MLR" ) )
sapply( 1:length(CFA), function(i) summary(CFA[[i]])$header$optim.converged ) # check convergence

# write down RMSEAs spread
write.table(

  x = decimalcz( minmax( sapply( CFA, function(i) summary(i, fit.measures = T)[["fit"]][["rmsea"]] ) ) ),
  file = here("tabs","rmsea.txt"),
  col.names = F,
  row.names = F,
  quote = F

)

# compute EFAs as well
EFA <- lapply(
  
  1:4,
  function(i)
    efa(data = d[ , c( v[ grepl( paste0("hs",i), v$variable ), "variable"], v[ grepl(1,v$data_set), "variable" ] )[-c(8,10)] ], nfactors = 1:5)
  
)
