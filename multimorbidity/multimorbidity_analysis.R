rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(foreign)
library(dplyr)
library(lme4)
library(lmerTest) # calculate p-values in summary()
library(ggplot2)
library(ggpubr) # ggerrorplot
library(labelled) # to_factor

setwd(sprintf("~%s/multimorbidity", setpath))
df <- readRDS("t0t1t2t3_data.rds")
dfwide <- readRDS("t0t1t2t3_data_wide.rds")

# generate results ----
gen_table <- function(df, vars, ordinalVars, medianVars, both_tests = FALSE, paired = TRUE, member_id = "member_id", group = "time", 
                      pre_name = "Baseline" , post_name = "Follow-up"){ 
  table <- data.frame(matrix(ncol = 10,  nrow = 0))
  
  row_count <- 1
  col_label <- 5
  col_bl <- 6
  col_bl2 <- 7 # matched baseline 
  col_f1 <- 8 # matched follow-up 
  col_dif <- 9 # difference
  col_pval <- 10 # p-value
  col_bl_n <- 2 # n of all baseline
  col_bl2_n <- 3 # n of matched baseline
  col_f1_n <- 4 # n of matched follow-up
  
  pre <- unique(df$time)[1]
  post <- unique(df$time)[2]
  
  df <- df[df$time %in% c(pre,post) & df$date %!in% NA,] # restrict to only T0 & T1 data
  df2 <- df %>% add_count(get(member_id)) %>% filter(n==2) # keep only those with both T0 & T1
  
  dfwide <- reshape(data=df, idvar= c(member_id),
                    timevar = group,
                    direction="wide")
  
  dfwide2 <- reshape(data=df2, idvar= c(member_id),
                     timevar = group,
                     direction="wide")
  
  df_ <- df # duplicate df
  dfwide2_ <- dfwide2 # duplicate df
  df2_ <- df2 # duplicate df
  
  colnames(table)[1] <- ""
  colnames(table)[col_label] <- ""
  colnames(table)[col_bl] <- paste0(pre_name," (all)")
  colnames(table)[col_bl2] <- paste0(pre_name," (paired)")
  colnames(table)[col_f1] <- paste0(post_name," (paired)")
  colnames(table)[col_dif] <- "Difference"
  colnames(table)[col_pval] <- "p-value"
  colnames(table)[col_bl_n] <-  paste0("N ", pre_name," (all)")
  colnames(table)[col_bl2_n] <- paste0("N ", pre_name," (paired)")
  colnames(table)[col_f1_n] <- paste0("N ", post_name," (paired)")
  
  
  table[row_count, 1] <- "N"
  table[row_count, col_label] <- ""
  table[row_count, col_bl] <-  nrow(df[df[[group]] == pre,])
  table[row_count, col_bl2] <-  nrow(dfwide2)
  table[row_count, col_f1] <-  nrow(dfwide2)
  
  row_count <- row_count + 1
  
  get_mean_sd <- function(x){
    mean <- mean(x, na.rm = TRUE) %>% round_format(decimal_places = 2)
    sd <- sd(x, na.rm = TRUE) %>% round_format(decimal_places = 2)
    return(paste0(mean, " (", sd, ")"))
  }
  
  get_median_iqr <- function(x){
    median <- median(x, na.rm = TRUE) %>% round_format(decimal_places = 0)
    q1 <- quantile(x, probs =  0.25) %>% round_format(decimal_places = 0)
    q3 <- quantile(x, probs =  0.75) %>% round_format(decimal_places = 0)
    
    return(paste0(median, " (", q1, "-", q3, ")"))
  }
  
  for (var in vars){
    print(var)
    df <- df_ # reset df
    if (class(df[[var]]) == "character") next()
    if (all(df[[var]][which(df[[group]] == pre)] %in% NA) | all(df[[var]][which(df[[group]] == post)] %in% NA)) next()
    
    dfwide2 <- dfwide2_[!is.na(dfwide2_[[paste0(var, ".0")]]) & !is.na(dfwide2_[[paste0(var, ".1")]]), ] # filter from duplicated df 
    df2 <- df2_[(df2_[[group]] == pre & !is.na(df2_[[var]])) |
                  (df2_[[group]] == post & !is.na(df2_[[var]])),] # filter from duplicated df 
    df <- df_[!is.na(df_[[var]]),] # filter from duplicated df 
    
    table[row_count, col_bl_n] <-  nrow(df[df[[group]] == pre & !is.na(df[[var]]),])
    matched_n <- min(nrow(dfwide2[!is.na(dfwide2[[paste0(var, ".0")]]),]), nrow(dfwide2[!is.na(dfwide2[[paste0(var, ".1")]]),]))
    table[row_count, col_bl2_n] <-  matched_n
    table[row_count, col_f1_n] <-  matched_n
    
    if (var %in% ordinalVars & both_tests) {
      table[row_count, 1] <- var
      
      table[row_count, col_label] <- "mean (sd)"
      table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df[[group]] == pre)])
      table[row_count, col_bl2] <- get_mean_sd(dfwide2[[paste0(var, ".0")]])
      table[row_count, col_f1] <-  get_mean_sd(dfwide2[[paste0(var, ".1")]])
      
      t_test <-  
        t.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
      table[row_count, col_dif] <-  (mean(df2[[var]][which(df2[[group]] == post)], na.rm = TRUE) - mean(df2[[var]][which(df2[[group]] == pre)], na.rm = TRUE)) %>% round(digits = 2)
      if (t_test$p.value < 0.001){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- t_test$p.value %>% round(digits = 3)
      }
      
      row_count <- row_count + 1
    }
    
    if (var %in% ordinalVars){
      table[row_count, 1] <- var
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
      if (wilcox_test$p.value < 0.001){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- wilcox_test$p.value %>% round(digits = 3)
      }
      
      for (val in unique(df[[var]])[order(unique(df[[var]]))]){
        table[row_count, col_label] <- val
        
        if (val %in% NA){
          table[row_count, col_label] <- "N/A"
        }
        
        table[row_count, col_bl] <- 
          table(df[[var]][which(df[[group]] == pre)],useNA = "ifany") %>% 
          prop.table() %>% as.data.frame() %>% 
          .[which(.$Var1 %in% val),"Freq"] 
        
        if (val %in% unique(df2[[var]][which(df2[[group]] == pre)])){
          table[row_count, col_bl2] <- iferror(
            table(df2[[var]][which(df2[[group]] == pre)],useNA = "ifany") %>%
              prop.table() %>% as.data.frame() %>%
              .[which(.$Var1 %in% val),"Freq"], NA)
        } else {
          table[row_count, col_bl2]  <- 0
        }
        
        if (val %in% unique(df2[[var]][which(df2[[group]] == post)])){
          table[row_count, col_f1] <-
            table(df2[[var]][which(df2[[group]] == post)],useNA = "ifany") %>%
            prop.table() %>% as.data.frame() %>%
            .[which(.$Var1 %in% val),"Freq"]
        } else {
          table[row_count, col_f1]  <- 0 
        }
        
        table[row_count, col_dif] <- as.numeric(table[row_count, col_f1]) - as.numeric(table[row_count, col_bl2])
        table[row_count, col_bl] <- table[row_count, col_bl] %>% as.numeric() %>% scales::percent(accuracy = 0.1)
        table[row_count, col_bl2] <- table[row_count, col_bl2] %>% as.numeric() %>% scales::percent(accuracy = 0.1)
        table[row_count, col_f1] <- table[row_count, col_f1] %>% as.numeric() %>% scales::percent(accuracy = 0.1)
        table[row_count, col_dif] <- table[row_count, col_dif] %>% as.numeric() %>% scales::percent(accuracy = 0.1)
        
        row_count <- row_count + 1
      }
    } else {
      table[row_count, 1] <- var
      
      table[row_count, col_label] <- "mean (sd)"
      table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df[[group]] == pre)])
      table[row_count, col_bl2] <- get_mean_sd(dfwide2[[paste0(var, ".0")]])
      table[row_count, col_f1] <-  get_mean_sd(dfwide2[[paste0(var, ".1")]])
      
      t_test <-  
        t.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
      table[row_count, col_dif] <-  (mean(df2[[var]][which(df2[[group]] == post)], na.rm = TRUE) - mean(df2[[var]][which(df2[[group]] == pre)], na.rm = TRUE)) %>% round(digits = 2)
      if (t_test$p.value < 0.001){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- t_test$p.value %>% round(digits = 3)
      }
      
      row_count <- row_count + 1
    }
    
    if (var %in% medianVars){
      table[row_count, col_label] <- "median (Q1-Q3)"
      table[row_count, col_bl] <-  get_median_iqr(df[[var]])
      table[row_count, col_bl2] <- get_median_iqr(dfwide2[[paste0(var, ".0")]])
      table[row_count, col_f1] <-  get_median_iqr(dfwide2[[paste0(var, ".1")]])
      
      table[row_count, col_dif] <- (median(df2[[var]][which(df2[[group]] == post)], na.rm = TRUE) - median(df2[[var]][which(df2[[group]] == pre)], na.rm = TRUE)) 
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
      if (wilcox_test$p.value < 0.001){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- wilcox_test$p.value %>% round(digits = 3)
      }
      
      row_count <- row_count + 1
    }
  }
  return(table)
}

vars <- c("bmi", "bmi2", "height", "waist", "hypertension", "hgs", "hgs_",  "sar", "sar_", "bpi_s", "bpi_i", "iadl", "pain", "srs",
          "phq", "phq_", "phq2t", "phq2t_", "gad", "gad_", "gad2t", "gad2t_", 
          "ls", "ls_e", "ls_s", 
          "sleep", "isi", "isit", "meaning", "eq5d", "eq5d6", "moca", "mci", "smu1", "health",
          "efs4", "efs5", "efs6", 
          "hgs_m", "hgs_f", "pain", "ls_", "oral", "efs1_", "hcu2", "hcu3", "hcu4", "hcu1", "efs1", "efs", "efs14", "moca_"
          )

vars_except <- c("sopd", "time", "date", "time1", "time2", sprintf("note%s", 1:5))

vars_ordinal <- c("bmi2", "hypertension", "hgs_", "srs", "sar_", "phq_", "phq2t_", "gad_", "gad2t_", "isit", "sleep", "mci", "meaning", 
                  "smu1", "health", "efs4", "hgs_m", "hgs_f",
                  "pain", "ls_",  "oral", "efs1_", "hcu2", "hcu3",  "efs5", "efs6", "hcu4", "efs1", "efs14", "moca_")

gen_table(df[df$time %in% c(0,1),], vars = vars, ordinalVars = vars_ordinal, medianVars = NULL, 
          both_tests = TRUE, member_id = "sopd", pre_name = "T0" , post_name = "T1") %>% clipr::write_clip()

vars <- c("age", "gender", "live_", "bmi", "bmi2", "md3", "md1", "pain", 
          "sar", "sar_", "fs", "fs_", "phq", "phq_", "gad", "gad_", "moca", "mci", "efs4", "ls", "ls_", "smu3_", "pase",
          "hgs", "hgs_", "hgs_m", "hgs_f", "eq5d", "eq5d6", "moca_")
vars_ordinal <- c("gender", "live_", "bmi2",  "md3", "md1", "pain", "sar_",  "fs_", "phq_", "gad_", "mci", "efs4", "ls_", "smu3_", "hgs_", "hgs_m", "hgs_f", "moca_")

tableone::CreateTableOne(data =  df[df$time %in% 0,],
                         vars = vars, factorVars = vars_ordinal) %>%
  print(showAllLevels = TRUE) %>% clipr::write_clip()

# activity data analysis ----
table <- combine_tables(NULL, 
                        dep_var = paste0("bmi", "f1 - ", "bmi", "f0"),
                        lm(I(bmif1-bmif0) ~ 1 + exclass_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + exclass_numf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + diseaseclass_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + diseaseclass_numf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + socialclass_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + socialclass_numf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + pychoclass_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + pychoclass_numf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + mci_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + talk_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + activity_attf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + activity_numf1, data = dfwide),
                        lm(I(bmif1-bmif0) ~ 1 + activity_catf1, data = dfwide)
)

vars <- c("height", "waist", "hypertension", "hgs", "hgs_",  "sar", "sar_", "bpi_s", "bpi_i", "iadl", "pain", "srs",
          "phq2t", "gad2t", 
          "ls", "ls_e", "ls_s", "ls_", 
          "sleep", "isi", "meaning", "eq5d", "eq5d6", "moca", "mci", "smu1", "health",
          "efs4", "efs5", "efs6", 
          "hgs_m", "hgs_f", "pain", "oral", "efs1_", "hcu2", "hcu3", "hcu4", "hcu1", "efs"
)

for (var in vars){
  temp <- combine_tables(NULL, 
                         dep_var = paste0(var, "f1 - ", var, "f0"),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + exclass_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + exclass_numf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + diseaseclass_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + diseaseclass_numf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + socialclass_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + socialclass_numf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + pychoclass_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + pychoclass_numf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + mci_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + talk_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + activity_attf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + activity_numf1, data = dfwide),
                                                 lm(I(get_(var, "f1") - get_(var, "f0")) ~ 1 + activity_catf1, data = dfwide)
                                                 )
  colnames <- c(names(table), names(temp)[2:ncol(temp)])
  table <- cbind(table, temp[2:ncol(temp)])
  names(table) <- colnames
  # rm(temp)
}
