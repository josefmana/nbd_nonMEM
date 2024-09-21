# This is a script used to calculate models estimating convergent & divergent validity metrics.

rm( list = ls() ) # clean environment

# set-up libraries
library(here)
library(tidyverse)
library(pwr)
library(psych)
library(corrplot)
library(lavaan)

sapply( c("figs","tabs"), function(i) if( !dir.exists(i) ) dir.create(i) ) # prepare folders
v <- read.csv( here("_data","vars.csv"), sep = "," ) # helpers
source("funs.R") # read in-house functions

# read and format data
d <-
  read.csv( here("_data","main_df.csv"), sep = "," ) %>%
  mutate(
    source = factor( source, levels = c("Validacni_studie", "Sadecka"), labels = paste0("Data set #",1:2), ordered = T ),
    sex = factor( sex, levels = c("f","m"), ordered = T )
  )

# variables to use
v1 <- subset(v, analysis == "selection") # variables for the selection analysis
v2 <- v %>% mutate( analysis = if_else( grepl("NP2 ", label), "validation", analysis) ) %>% filter(analysis == "validation") # variables for the validation analysis


# extract number of subjects
n <- list(
  
  max = sapply( levels(d$source), function(i) as.character( nrow( d[ d$source == i, ] ) ) ),
  cor = lapply( 1:2, function(i) corr.test( d[ d$source == paste0("Data set #",i), get( paste0("v",i) )$variable ] )$n )
  
)


# DATA DESCRIPTION ----

# print it
tab1 <-
  
  printab( v, d, "source", 2 ) %>%
  
  # ToM shinaningans (because it is in both data sets)
  bind_rows( . , data.frame( .[ "ToM", "Data set #1" ], .[ "ToM1", "Data set #2" ] ) %>% `colnames<-`( paste0("Data set #",1:2) ) ) %>%
  filter( ! (rownames(.) %in% c("ToM","ToM1") ) ) %>%
  `rownames<-`( c( rownames(.)[1:(nrow(.)-1)], "ToM" ) ) %>%
  slice( 1:27, n(), 28:(n()-1) ) %>%
  
  # finishing touches
  mutate( across( where(is.character), ~ ifelse( grepl("NA",.x), "-", .x ) ) ) %>%
  mutate( across( everything(), decimalcz ) ) %>%
  rbind( N = n$max, . ) %>%
  rownames_to_column("Variable")

# save it
write.table(x = tab1, file = here("tabs","data_description.csv"), sep = ";", row.names = F, quote = F)

## ---- HISTOGRAM OF AGE ----

# prepare file for plotting
jpeg(filename = here("figs","age_distribution.jpg"), width = 7, height = 8, units = "in", quality = 100, res = 300)

# prepare grid
par( mfrow = c(2,1) )

# plot them
hist(x = subset(d, source == "Data set #1")$vek_roky, col = "lightblue", main = NULL, xlab = "Věk (roky)", ylab = "Počet", xlim = c(6,20), breaks = 14)
title("A", adj = 0) # Data set #1
hist(x = subset(d, source == "Data set #2")$vek_roky, col = "lightblue", main = NULL, xlab = "Věk (roky)", ylab = "Počet", xlim = c(6,20), breaks = 14)
title("B", adj = 0) # Data set #2

# finish it
dev.off()

# return grid to original
par( mfrow = c(1,1) )


# POWER ANALYSIS ----

# pre-calculate Bonferroni adjusted p-values for each analysis
pval <- list(
  
  unadjusted = c(.05, .05),
  adjusted = sapply( 1:2, function(i) .05 / (nrow( subset( get( paste0("v",i) ), !grepl("NP",label) ) ) * (-i+3) ) )
  # in the first case adjusting for 2 times number of comparisons because the decision is based on two measures
  # in the second case, adjusting for the number of comparisons only because it will be evaluated for each variable separately
  
)

# compute table of powers
pwr_tab <- lapply(
  
  set_names(nm = c("selection", "validation"), x = 1:2), # selection (1) vs validation (2)
  function(i)
    
    sapply(
      
      c("0.4","0.6","0.8"), # reasonably large correlations
      function(r)
        
        sapply(
          
          names(pval), # adjusted vs unadjusted significance threshold
          function(p)
            
            pwr.r.test(n = min(n$cor[[i]]), sig.level = pval[[p]][[i]], r = as.numeric(r), power = NULL )$power
          
        )
    ) %>%
    
    t() %>%
    as.data.frame() %>%
    rownames_to_column("rho")

) %>%
  
  do.call( rbind.data.frame, . ) %>%
  mutate( analysis = strsplit(rownames(.), ".", fixed = T)[[1]][1], .after = rho)

# save it
write.table(x = pwr_tab, file = here("tabs","power_analysis.csv"), sep = ",", row.names = F, quote = F)


# CORRELATION ANALYSES ----

# extract (partial) correlation matrices (for both analyses at the same time)
pcorr <- lapply(
  
  set_names(nm = c("selection", "validation"), x = 1:2), # selection (1) vs validation (2)
  function(i) list(
    
    r = partial.r(

      data = d[ d$source == paste0("Data set #",i), c(get( paste0("v",i) )$variable, "vek_roky") ],
      x = get( paste0("v",i) )$variable,
      y = "vek_roky",
      use = "pairwise",
      method = "pearson"

    )
    
  )
)

# add p-values
for (i in 1:2) pcorr[[i]]$p <- corr.p(
  
  r = pcorr[[i]]$r,
  n = n$cor[[i]] - 1, # n - 1 for one covariate
  adjust = "none",
  alpha = .05
  
)$p


## ---- SCORING SELECTION ----

### ---- table it ----

tab2 <- expand.grid(

  y = subset(v1, data_set == "both")$variable,
  x = subset(v1, data_set == "Data set #1")$variable

) %>%
  
  # prepare variables of interest
  mutate(
    
    # extract stats
    rho = usapply( 1:nrow(.), function(i) pcorr$selection$r[ as.character(y[i]) , as.character(x[i]) ] ),
    p = usapply( 1:nrow(.), function(i) pcorr$selection$p[ as.character(y[i]) , as.character(x[i]) ] ),
    
    # prepare outcomes
    corr = paste0( decimalcz( rprint(rho, 2) ), if_else(p < pval$adjusted[1], "*", "") ),
    across( all_of( c("x","y") ), ~ usapply(.x, function(i) subset(v1, variable == i)$label) )
    
  ) %>%
  
  # tidy it up
  select(-rho, -p) %>%
  pivot_wider(names_from = y, values_from = corr)

# save it
write.table(x = tab2, file = here("tabs","scoring_selection.csv"), sep = ";", row.names = F, quote = F)


### ---- plot it ----

# prepare file for plotting
jpeg(filename = here("figs","scoring_selection.jpg"), width = 8, height = 8, units = "in", quality = 100, res = 300)

# plotting proper
cp0 <- tab2 %>%
  
  column_to_rownames("x") %>%
  `colnames<-`( sub( " ", "\n",colnames(.) ) ) %>%
  mutate( across( everything(), ~ as.numeric( sub( ",", ".",sub("*","",.x, fixed = T) ) ) ) ) %>%
  as.matrix() %>%
  
  corrplot(
    is.corr = F,
    method = "color",
    tl.srt = 90,
    tl.col = "black",
    col.lim = c(-1,1),
    col = COL2("PRGn", 10),
    addCoef.col = "grey42",
    number.digits = 2,
    cl.pos = "b",
    p.mat = tab2 %>%
      column_to_rownames("x") %>%
      `colnames<-`( sub( " ", "\n",colnames(.) ) ) %>%
      mutate( across( everything(), ~ if_else( grepl("*", .x, fixed = T), 0, 1 ) ) ) %>%
      as.matrix(),
    insig = "blank",
  )

# add correlation estimates for p > .05 cases
with( subset(cp0$corrPos, p.value > .05), text( x, y, round(2*corr-1, 2), cex = 1, col = "grey42", font = 1 ) ) # need the 2*corr-1 because of internal re-scaling corrplot does if is.corr = F

# finish it
dev.off()


## ---- SCORING VALIDATION ----

### ---- table it ----

tab3 <- expand.grid(
  
  y = subset(v2, data_set == "both")$variable,
  x = subset(v2, data_set == "Data set #2")$variable
  
) %>%
  
  # prepare variables of interest
  mutate(
    
    # extract stats
    rho = usapply( 1:nrow(.), function(i) pcorr$validation$r[ as.character(y[i]) , as.character(x[i]) ] ),
    p = usapply( 1:nrow(.), function(i) pcorr$validation$p[ as.character(y[i]) , as.character(x[i]) ] ),
    
    # prepare outcomes
    corr = paste0( decimalcz( rprint(rho, 2) ), if_else(p < pval$unadjusted[2], "*", "") ),
    across( all_of( c("x","y") ), ~ usapply(.x, function(i) subset(v2, variable == i)$label) )
    
  ) %>%
  
  # tidy it up
  select(-rho, -p) %>%
  pivot_wider(names_from = y, values_from = corr)

# save it
write.table(x = tab3, file = here("tabs","scoring_validation.csv"), sep = ";", row.names = F, quote = F)


### ---- plot it ----

# prepare file for plotting
jpeg(filename = here("figs","scoring_validation.jpg"), width = 10.5, height = 10.5, units = "in", quality = 100, res = 300)

# plotting proper
cp1 <- pcorr[[2]]$r[v2$variable, v2$variable] %>%
  
  `colnames<-`(v2$label) %>%
  `rownames<-`(v2$label) %>%
  
  corrplot(
    type = "lower",
    method = "color",
    p.mat = pcorr[[2]]$p[v2$variable, v2$variable] %>% `colnames<-`(v2$label) %>% `rownames<-`(v2$label),
    insig = "blank",
    tl.srt = 45,
    tl.col = "black",
    number.digits = 2,
    addCoef.col = "white",
    addgrid.col = "grey",
    col = COL2("PRGn", 10),
    diag = F
  )

# add correlation estimates for p > .05 cases
with( subset(cp1$corrPos, p.value > .05), text( x, y, round(corr,2), cex = .8 ) )

# finish it
dev.off()


# (NON-)CEILING EFFECT ----

# prepare labels data frame
labs <-
  
  v2 %>%
  filter( grepl("NP2|CMS", label) ) %>%
  select(variable, label) %>%
  mutate(
    letter = c("B","D","F","H","B","D","A","C","E","A","G","E","C"),
    group = c( rep("trial",4), rep("sum",2), rep("trial",3), "sum", "trial", rep("sum",2) ),
    col = if_else( grepl("NP2", label), "deepskyblue", "violetred")
  ) %>%
  arrange(group, letter)


## ---- SUM SCORES ----

### ---- younger children ----

# prepare a file
jpeg(filename = here("figs","ceiling_sums_younger.jpg"), width = 8, height = 10, units = "in", quality = 100, res = 300)

# prepare a grid
par( mfrow = c(3,2) )

# plot it
for (i in which(labs$group == "sum") ) {
  
  # histogram
  hist(
    x = as.numeric( na.omit( subset(d, vek_roky < 9)[ , labs$variable[i] ] ) ),
    col = labs$col[i],
    breaks = 10,
    main = NULL,
    xlab = labs$label[i],
    ylab = "Počet"
  )
  
  # plot label
  title(labs$letter[i], adj = 0)
  
}

# save it
dev.off()


### ---- older children ----

# prepare a file
jpeg(filename = here("figs","ceiling_sums_older.jpg"), width = 8, height = 10, units = "in", quality = 100, res = 300)

# prepare a grid
par( mfrow = c(3,2) )

# plot it
for (i in which(labs$group == "sum") ) {
  
  # histogram
  hist(
    x = as.numeric( na.omit( subset(d, vek_roky >= 9)[ , labs$variable[i] ] ) ),
    col = labs$col[i],
    breaks = 10,
    main = NULL,
    xlab = labs$label[i],
    ylab = "Počet"
  )
  
  # plot label
  title(labs$letter[i], adj = 0)
  
}

# save it
dev.off()


## ---- TRIAL SCORES ----

### ---- younger children ----

# prepare a file
jpeg(filename = here("figs","ceiling_trials_younger.jpg"), width = 6, height = 10, units = "in", quality = 100, res = 300)

# prepare a grid
par( mfrow = c(4,2) )

# plot it
for (i in which(labs$group == "trial") ) {
  
  # histogram
  hist(
    x = as.numeric( na.omit( subset(d, vek_roky < 9)[ , labs$variable[i] ] ) ),
    col = labs$col[i],
    breaks = 10,
    main = NULL,
    xlab = labs$label[i],
    ylab = "Počet"
  )
  
  # plot label
  title(labs$letter[i], adj = 0)
  
}

# save it
dev.off()


### ---- older children ----

# prepare a file
jpeg(filename = here("figs","ceiling_trials_older.jpg"), width = 6, height = 10, units = "in", quality = 100, res = 300)

# prepare a grid
par( mfrow = c(4,2) )

# plot it
for (i in which(labs$group == "trial") ) {
  
  # histogram
  hist(
    x = as.numeric( na.omit( subset(d, vek_roky >= 9)[ , labs$variable[i] ] ) ),
    col = labs$col[i],
    breaks = 10,
    main = NULL,
    xlab = labs$label[i],
    ylab = "Počet"
  )
  
  # plot label
  title(labs$letter[i], adj = 0)
  
}

# save it
dev.off()


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
