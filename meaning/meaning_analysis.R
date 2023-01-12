rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(foreign)
library(labelled)
library(dplyr)
library(lme4)
library(lmerTest) # calculate p-values in summary()
library(ggplot2)
library(patchwork) # combine ggplots using "+"
library(survival) # coxph

setwd(sprintf("~%s/multimorbidity", setpath))
df <- readRDS("t0t1t2t3_data.rds")
dfwide <- readRDS("t0t1t2t3_data_wide.rds")

df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)

gen_table <- function(fit, adjusted_r2 = FALSE, show_p = FALSE, show_CI = FALSE, exponentiate = FALSE){ 
  require(lmerTest)
  table <- data.frame(matrix(ncol = 2,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  dep_var <- as.character(formula(fit)[2])
  
  if (!is.null(summary(fit)$isLmer) | class(fit)[1] %in% "glmerMod"){ # check if linear mixed model (lmer)
    n <- iferror(nobs(fit), NA)
    n_unique <- iferror(summary(fit)$ngrps, NA)
    table[row_count, 1] <- "N (unique)"
    table[row_count, col_count] <-  paste0(n, " (", n_unique, ")")
    row_count <- row_count + 1
    
    icc <- iferror(performance::icc(fit)[[1]], NA)
    icc <- round_format(icc, 3)
    table[row_count, 1] <- "ICC"
    table[row_count, col_count] <-  icc
    row_count <- row_count + 1
  } else if (class(fit)[1] %in% "glm") {
    n <- iferror(nobs(fit), NA)
    table[row_count, 1] <- "N"
    table[row_count, col_count] <-  n
    row_count <- row_count + 1
    
    if(adjusted_r2){
      adj_r2 <- iferror(1 - ((summary(fit)$deviance/-2)-(length(fit$coeff)-1)) / (summary(fit)$null.deviance/-2), NA)
      adj_r2 <- round_format(adj_r2, 3)
      table[row_count, 1] <- "Adjusted R2"
      table[row_count, col_count] <-  adj_r2
    } else {
      r2 <- iferror(1 - ((summary(fit)$deviance/-2)) / (summary(fit)$null.deviance/-2), NA)
      r2 <- round_format(r2, 3)
      table[row_count, 1] <- "R2"
      table[row_count, col_count] <-  r2
    }
    row_count <- row_count + 1
    
  } else if (class(fit)[1] %in% "coxph"){
    n <- iferror(summary(fit)$n, NA)
    table[row_count, 1] <- "N"
    table[row_count, col_count] <-  n
    row_count <- row_count + 1
    
    concordance <- iferror(round_format(summary(fit)$concordance[[1]], 3), NA)
    table[row_count, 1] <- "Concordance"
    table[row_count, col_count] <-  concordance
    
    row_count <- row_count + 1
    
  } else {
    n <- iferror(nobs(fit), NA)
    table[row_count, 1] <- "N"
    table[row_count, col_count] <-  n
    row_count <- row_count + 1
    
    if(adjusted_r2){
      adj_r2 <- iferror(round_format(summary(fit)$adj.r.squared, 3), NA)
      table[row_count, 1] <- "Adjusted R2"
      table[row_count, col_count] <-  adj_r2
    } else {
      r2 <- iferror(round_format(summary(fit)$r.squared, 3), NA)
      table[row_count, 1] <- "R2"
      table[row_count, col_count] <-  r2
    }
    row_count <- row_count + 1
  }
  
  for (var in row.names(summary(fit)$coef)){
    
    if (!(var %in% unlist(table[1]))){
      table[row_count, 1] <- var
      if (grepl("age_group.", var, fixed = TRUE)){
        var_name <- substr(var, nchar("age_group.")+2, nchar(var))
        table[row_count, 1] <- var_name
      } else if (grepl("age_group", var, fixed = TRUE)){
        var_name <- substr(var, nchar("age_group")+1, nchar(var))
        table[row_count, 1] <- var_name
      }
    } else {
      row_count <- match(var, unlist(table[1]))
    }
    
    print(paste(dep_var, var))
    
    colnames(table)[col_count] <- dep_var
    
    beta <- iferror(summary(fit)$coef[var, 1], NA)
    if (class(fit)[1] %in% "coxph"){
      se <-  iferror(summary(fit)$coef[var, 3], NA)
    } else {
      se <-  iferror(summary(fit)$coef[var, 2], NA)
    }
    lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
    upperCI <- iferror(beta + qnorm(0.975) * se, NA)
    p_value <- iferror(summary(fit)$coef[var, ncol(summary(fit)$coef)], NA)
    
    # table[row_count, col_count] <-  paste0(n, ", ", starred_p(p_value, 3, beta))
    if (show_p){
      table[row_count, col_count] <-  round_format(p_value, 4)
    } else if (class(fit)[1] %in% c("glm", "coxph") & exponentiate){
      table[row_count, col_count] <-  starred_p(p_value, 3, exp(beta))
      if (show_CI){
        table[row_count, col_count] <-  paste0(round_format(exp(lowerCI), 3), ", ", round_format(exp(upperCI), 3))
      }
    } else if (show_CI & !(class(fit)[1] %in% c("glm", "coxph"))){
      table[row_count, col_count] <-  paste0(round_format(lowerCI, 3), ", ", round_format(upperCI, 3))
    } else {
      table[row_count, col_count] <-  starred_p(p_value, 3, beta)
    }
    
    row_count <- row_count + 1
    
  }
  col_count <- col_count + 1
  return(table)
}

combine_tables <- function(table = NULL, ..., adjusted_r2 = FALSE, show_p = FALSE, show_CI = FALSE, exponentiate = TRUE){
  for (i in 1:length(list(...))){
    if (is.null(table) & i == 1){
      table <- gen_table(list(...)[[i]], adjusted_r2, show_p, show_CI, exponentiate) 
      next
    }
    new_table <- gen_table(list(...)[[i]], adjusted_r2, show_p, show_CI, exponentiate) 
    dep_vars <- names(table)
    dep_var <- names(new_table)[2]
    names(table) <- c("X1",rep(2:ncol(table)))
    table <- plyr::join(table, new_table, by=c("X1"), type="full")
    names(table) <- c(dep_vars, dep_var)
    names(table)[names(table) == "X1"] <- ""
  }
  return(table)
}

dfwide$sarc_f.10 <- dfwide$sarc_f.1 - dfwide$sarc_f.0
dfwide$sarc_f.21 <- dfwide$sarc_f.2 - dfwide$sarc_f.1
dfwide$moca.10 <- dfwide$moca.1 - dfwide$moca.0
dfwide$hgs.10 <- dfwide$hgs.1 - dfwide$hgs.0

dfwide$sarc_f_mean.10 <- rowMeans(cbind(dfwide$sarc_f.1, dfwide$sarc_f.0))
dfwide$sarc_f_mean.21 <- rowMeans(cbind(dfwide$sarc_f.2, dfwide$sarc_f.1))
dfwide$moca_mean.10 <- rowMeans(cbind(dfwide$moca.1,dfwide$moca.0))
dfwide$hgs_mean.10 <- rowMeans(cbind(dfwide$hgs.1,dfwide$hgs.0))

# number of chronic diseases & of medications ----
create_variables <- function(df){
  df$md_total <- rowSums(df %>% select(md1, md2, md3), na.rm=TRUE)
  df$md_total <- ifelse(df$md == 1 & df$md1 == 0 & df$md2 == 0 & df$md3 == 0, 1, df$md_total)
  df$cvd_total <- rowSums(df %>% select(cvd1, cvd2, cvd3), na.rm=TRUE)
  df$cvd_total <- ifelse(df$cvd == 1 & df$cvd1 == 0 & df$cvd2 == 0 & df$cvd3 == 0, 1, df$cvd_total)
  df$rd_total <- rowSums(df %>% select(rd1, rd2, rd3, rd4), na.rm=TRUE)
  df$rd_total <- ifelse(df$rd == 1 & df$rd1 == 0 & df$rd2 == 0 & df$rd3 == 0 & df$rd4 == 0, 1, df$rd_total)
  df$ld_total <- rowSums(df %>% select(ld1, ld2, ld3), na.rm=TRUE)
  df$ld_total <- ifelse(df$ld == 1 & df$ld1 == 0 & df$ld2 == 0 & df$ld3 == 0, 1, df$ld_total)
  df$dd_total <- rowSums(df %>% select(dd1, dd2, dd3, dd4, dd5), na.rm=TRUE)
  df$dd_total <- ifelse(df$dd == 1 & df$dd1 == 0 & df$dd2 == 0 & df$dd3 == 0 & df$dd4 & df$dd5 == 0, 1, df$dd_total)
  df$msd_total <- rowSums(df %>% select(msd1, msd2 ), na.rm=TRUE)
  df$msd_total <- ifelse(df$msd == 1 & df$msd1 == 0 & df$msd2 == 0, 1, df$msd_total)
  df$td_total <- rowSums(df %>% select(td1, td2), na.rm=TRUE)
  df$td_total <- ifelse(df$td == 1 & df$td1 == 0 & df$td2 == 0, 1, df$td_total)
  df$rrd_total <- rowSums(df %>% select(rrd1, rrd2, rrd3), na.rm=TRUE)
  df$rrd_total <- ifelse(df$rrd == 1 & df$rrd1 == 0 & df$rrd2 == 0 & df$rrd3 == 0, 1, df$rrd_total)
  df$ent_total <- rowSums(df %>% select(ent1, ent2), na.rm=TRUE)
  df$ent_total <- ifelse(df$ent == 1 & df$ent1 == 0 & df$ent2 == 0, 1, df$ent_total)
  df$ed_total <- rowSums(df %>% select(ed1, ed2, ed3, ed4), na.rm=TRUE)
  df$ed_total <- ifelse(df$ed == 1 & df$ed1 == 0 & df$ed2 == 0 & df$ed3 == 0 & df$ed4 == 0, 1, df$ed_total)
  df$sd_total <- rowSums(df %>% select(sd1, sd2), na.rm=TRUE)
  df$sd_total <- ifelse(df$sd == 1 & df$sd1 == 0 & df$sd2 == 0, 1, df$sd_total)
  df$nsd_total <- rowSums(df %>% select(nsd1, nsd2, nsd3, nsd4), na.rm=TRUE)
  df$nsd_total <- ifelse(df$nsd == 1 & df$nsd1 == 0 & df$nsd2 == 0 & df$nsd3 == 0 & df$nsd4 == 0, 1, df$nsd_total)
  df$mi_total <- rowSums(df %>% select(mi1, mi2, mi3, mi4), na.rm=TRUE)
  df$mi_total <- ifelse(df$mi == 1 & df$mi1 == 0 & df$mi2 == 0 & df$mi3 == 0 & df$mi4 == 0, 1, df$mi_total)
  
  df$cd <- rowSums(df %>% select(md_total, ca, cvd_total, rd_total , ld_total, dd_total, 
                                 msd_total, td_total, rrd_total, ent_total, ed_total, sd_total, 
                                 bd1, nsd_total, mi_total, od), na.rm=TRUE)
  df$cd <- ifelse(df$time > 0, NA, df$cd)
  var_label(df$cd) <- "number of disease, by 43+ diseases (total diseases plus od-other disease)"
  
  
  df$htm <- rowSums(df %>% select(htm1, htm2, htm3), na.rm=TRUE)
  df$dmm <- rowSums(df %>% select(dmm1, dmm2, dmm3), na.rm=TRUE)
  df$dlm <- rowSums(df %>% select(dlm1, dlm2, dlm3), na.rm=TRUE)
  df$mim <- rowSums(df %>% select(mim1, mim2, mim3), na.rm=TRUE)
  df$painm <- rowSums(df %>% select(painm1, painm2, painm3), na.rm=TRUE)
  
  df$medication <- rowSums(df %>% select(htm, hdm, dmm, dlm, mim, painm), na.rm=TRUE)
  df$medication <- ifelse(df$time > 0, NA, df$medication)
  var_label(df$medication) <- "total number of medication intake"
  
  return(subset(df, select = c(cd, medication)))
}

df <- cbind(df, create_variables(df))

# HGS asymmetry ----
df$hgs_l <- pmax(df$hgs1, df$hgs2) # better hand-grip strength of left hand
df$hgs_r <-  pmax(df$hgs3, df$hgs4) # better hand-grip strength of right hand
df$hgs_l <- ifelse(df$hgs_l == 0, NA, df$hgs_l) # set to NA if zero
df$hgs_r <- ifelse(df$hgs_r == 0, NA, df$hgs_r)# set to NA if zero

df$hgs_ratio <- pmax(df$hgs_l, df$hgs_r)/pmin(df$hgs_l, df$hgs_r)
df$hgs_asym <- ifelse(df$hgs_ratio > 1.1, 1, 0)
df$hgs_l <- NULL
df$hgs_r <- NULL

# handgrip AWGS 2019 ----
df$hgs_2 <- ifelse(is.na(df$hgs) | is.na(df$gender), NA, 
                   ifelse(df$hgs >= 28 & df$gender == "M" , 1, 
                          ifelse(df$hgs >= 18 & df$gender == "F" , 1, 0)))
# from long to wide ----
dfwide <- reshape(df,
                  idvar = c("sopd"), # this line is to keep variables
                  sep = ".", 
                  timevar = "time",
                  direction = "wide")

dfwide$sar.10 <- dfwide$sar.1 - dfwide$sar.0
dfwide$sar.21 <- dfwide$sar.2 - dfwide$sar.1
dfwide$moca.10 <- dfwide$moca.1 - dfwide$moca.0
dfwide$hgs.10 <- dfwide$hgs.1 - dfwide$hgs.0

dfwide$sar_mean.10 <- rowMeans(cbind(dfwide$sar.1, dfwide$sar.0))
dfwide$sar_mean.21 <- rowMeans(cbind(dfwide$sar.2, dfwide$sar.1))
dfwide$moca_mean.10 <- rowMeans(cbind(dfwide$moca.1,dfwide$moca.0))
dfwide$hgs_mean.10 <- rowMeans(cbind(dfwide$hgs.1,dfwide$hgs.0))

# crate variables ----
dfwide$work.0 <- car::recode(dfwide$work.0,"
1 = 'retiree';
2 = 'housewife';
3 = 'employer';
4 = 'employee';
5 = 'self-employed';
6 = 'unemployed'
")
dfwide$work.0 <- ifelse(dfwide$work.0 %in% c("employer","self-employed"), "employer/self-employed", dfwide$work.0 )

dfwide$marriage.0 <- car::recode(dfwide$marriage.0,"
1 = 'single';
2 = 'married';
3 = 'divorced';
4 = 'separated';
5 = 'widowed'
")

dfwide$ht2.0 <- ifelse(dfwide$md.0 == 0, 0, # md1 is a nested question on chronic pain for those with metabolic disease 
                       ifelse(dfwide$md1.0 >= 1, 1, 0)) 
dfwide$dyslip.0 <- ifelse(dfwide$md.0 == 0, 0, # md2 is a nested question on chronic pain for those with metabolic disease 
                          ifelse(dfwide$md2.0 >= 1, 1, 0)) 
dfwide$dm.0 <- ifelse(dfwide$md.0 == 0, 0, # md3 is a nested question on chronic pain for those with metabolic disease 
                      ifelse(dfwide$md3.0 >= 1, 1, 0)) 
dfwide$chronicpain.0 <- ifelse(dfwide$msd.0 == 0, 0, # msd1 is a nested question on chronic pain for those with musculoskeletal and connective tissue diseases 
                               ifelse(dfwide$msd1.0 >= 1, 1, 0)) 
dfwide$skeletal.0 <- ifelse(dfwide$msd.0 == 0, 0, # msd2 is a nested question on "inflammatory conditions" for those with musculoskeletal and connective tissue diseases 
                            ifelse(dfwide$msd.0 >= 1, 1, 0)) 

dfwide$female <- ifelse(dfwide$gender.0 == "F", 1,
                        ifelse(dfwide$gender.0 == "M", 0, NA))

# reorder factor levels by decreasing frequency 
xLev = names(table(dfwide$work.0))[order(table(dfwide$work.0), decreasing = TRUE)]
dfwide$work.0 = factor(dfwide$work.0, levels=xLev)
xLev = names(table(dfwide$marriage.0))[order(table(dfwide$marriage.0), decreasing = TRUE)]
dfwide$marriage.0 = factor(dfwide$marriage.0, levels=xLev)


# # restrict sample and code dropouts ----
# dfwide$dropout_t1 <- ifelse(!(is.na(dfwide$gender.0) |  # 477 at T1
#                                 is.na(dfwide$age.0) |
#                                 is.na(dfwide$cd.0) |
#                                 is.na(dfwide$edu.0) |
#                                 is.na(dfwide$sar.0) |
#                                 is.na(dfwide$sar.1) |
#                                 is.na(dfwide$moca.0) |
#                                 is.na(dfwide$moca.1) |
#                                 is.na(dfwide$hgs.0) |
#                                 is.na(dfwide$hgs.1)), 0, 
#                             ifelse(!(is.na(dfwide$gender.0) | # 730 at T0
#                                        is.na(dfwide$age.0) |
#                                        is.na(dfwide$cd.0) |
#                                        is.na(dfwide$edu.0) |
#                                        is.na(dfwide$sar.0) |
#                                        is.na(dfwide$moca.0) |
#                                        is.na(dfwide$hgs.0)) &
#                                      (is.na(dfwide$date.1)  
#                                      ), 1, NA))
# 
# dfwide$dropout_t0 <- ifelse( is.na(dfwide$gender.0) |
#                                is.na(dfwide$age.0) |
#                                is.na(dfwide$cd.0) |
#                                is.na(dfwide$edu.0) |
#                                is.na(dfwide$sar.0) |
#                                is.na(dfwide$moca.0) |
#                                is.na(dfwide$hgs.0), 1, 0)
# 
# dfwide[is.na(dfwide$moca.0), ] %>% nrow() # 298 missing MOCA at T0
# dfwide[is.na(dfwide$sar.0) & !is.na(dfwide$moca.0), ] %>% nrow() # 64 missing SARC-F at T0
# dfwide[is.na(dfwide$hgs.0) & !is.na(dfwide$sar.0) & !is.na(dfwide$moca.0), ] %>% nrow() # 1 missing handgrip strength at T0
# dfwide[is.na(dfwide$date.1) & dfwide$dropout_t0 == 0 , ] %>% nrow() # 239 non-responders at T1 (aka lost to follow-up)
# 
# dfwide$dropout_t0t1 <- ifelse( is.na(dfwide$gender.0) |  # 477 at T1
#                                  is.na(dfwide$age.0) |
#                                  is.na(dfwide$cd.0) |
#                                  is.na(dfwide$edu.0) |
#                                  is.na(dfwide$sar.0) |
#                                  is.na(dfwide$sar.1) |
#                                  is.na(dfwide$moca.0) |
#                                  is.na(dfwide$moca.1) |
#                                  is.na(dfwide$hgs.0) |
#                                  is.na(dfwide$hgs.1), 1, 0)
# 
# dfwide0 <- dfwide
# dfwide <- dfwide %>% filter(!(is.na(gender.0) |
#                                 is.na(age.0) |
#                                 is.na(cd.0) |
#                                 is.na(edu.0) |
#                                 is.na(sar.0) |
#                                 is.na(sar.1) |
#                                 is.na(moca.0) |
#                                 is.na(moca.1) |
#                                 is.na(hgs.0) |
#                                 is.na(hgs.1)
# ))
# 
# descriptive statistics ----

dfwide$meaning0_NA <- ifelse(dfwide$meaning.0 %in% NA, 1, 0)

allVars <- c("age.0", "gender.0", "cd.0", "ht2.0", "dyslip.0", "dm.0", "skeletal.0", "chronicpain.0", "medication.0", "edu.0", "work.0", "marriage.0")
catVars <- c("gender.0",
             # "ht2.0", "dyslip.0", "chronicpain.0", "dm.0", "skeletal.0",
             "work.0", "marriage.0")
tableone::CreateTableOne(data =  dfwide, 
                         strata = c("meaning0_NA"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("age.0", "gender.0", "cd.0", "ht2.0", "dyslip.0", "chronicpain.0", "dm.0", "skeletal.0", "medication.0", "edu.0", "work.0", "marriage.0",
             "sar.0", "hgs.0", "moca.0")
catVars <- c("gender.0", "work.0", "marriage.0")
tableone::CreateTableOne(data =  dfwide0, 
                         strata = c("dropout_t0t1"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("sar.0", "sar.1", "sar_.0", "sar_.1", "moca.0", "moca.1", "mci.0", "mci.1", "hgs.0", "hgs.1")
catVars <- c("sar_.0", "sar_.1", "mci.0", "mci.1")
tableone::CreateTableOne(data =  dfwide, 
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

dftemp <- reshape(as.data.frame(dfwide),
                  direction = "long",
                  idvar = "case_id",
                  varying = list(c("sar.0", "sar.1"),
                                 c("sar_.0", "sar_.1"),
                                 c("moca.0", "moca.1"),
                                 c("mci.0", "mci.1"),
                                 c("hgs.0", "hgs.1")
                  ),
                  v.names=c("sar", 
                            "sar_", 
                            "moca", 
                            "mci", 
                            "hgs"),
                  timevar="time",
                  times=c("0", "1"))

allVars <- c("sar", "sar_", "moca", "mci", "hgs")
tableone::CreateTableOne(data =  dftemp, 
                         strata = c("time"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
rm(dftemp)
