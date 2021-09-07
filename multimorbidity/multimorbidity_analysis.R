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

# creat categorical variables ----
categorize <- function(df){
  df$bps <- (df$bp1s + df$bp2s)/2 # average systolic BP
  df$bps <- ifelse(is.na(df$bps), df$bp1s, df$bps)
  df$bps <- ifelse(is.na(df$bps), df$bp2s, df$bps)
  
  df$bpd <- (df$bp1d + df$bp2d)/2 # average systolic BP
  df$bpd <- ifelse(is.na(df$bpd), df$bp1d, df$bpd)
  df$bpd <- ifelse(is.na(df$bpd), df$bp2d, df$bpd)
  
  df$hypertension <- ifelse(df$bps >= 140 | df$bp2d >= 90, 1, 0)
  
  df %>% select(starts_with(sprintf("bpi%s", 3:6))) %>% names() -> q_bpi_s # BPI pain severity
  df %>% select(starts_with(sprintf("bpi%s", 10:16))) %>% names() -> q_bpi_i # BPI pain interference
  df %>% select(starts_with(sprintf("iadl%s", 1:5))) %>% names() -> q_iadl # BPI pain interference
  
  # reverse code IADL
  df[q_iadl] <- sapply(df[q_iadl], function(x) ifelse(x == 7, NA, x))
  df[q_iadl] <- sapply(df[q_iadl], function(x) (x-4)*-1) # from 1:3 to 3:1
  
  df <- df %>%
    mutate(
      bpi_s = rowSums(.[q_bpi_s], na.rm = FALSE)/4,
      bpi_i = rowSums(.[q_bpi_i], na.rm = FALSE)/7,
      iadl = rowSums(.[q_iadl], na.rm = FALSE)/5,
    )
  
  df$hgs_l <- pmax(df$hgs1, df$hgs2) # better hand-grip strength of left hand
  df$hgs_r <-  pmax(df$hgs3, df$hgs4) # better hand-grip strength of right hand
  df$hgs <- pmax(df$hgs_l, df$hgs_r) # average hand-grip strength
  # df$gender <- to_factor(df$gender)
  
  df$hgs_ <- ifelse(is.na(df$hgs) | is.na(df$gender), NA, 
                    ifelse(df$hgs >= 26 & df$gender == "M" , 1, 
                           ifelse(df$hgs >= 18 & df$gender == "F" , 1, 0)))
  
  # df$sar_ <- ifelse(df$sar >= 4, "SARc-F positive (≥4)", "SARc-F negative (<4)")
  df$sar_ <- ifelse(df$sar >= 4, 1, 0)
  
  # df$phq_ <- car::recode(df$phq, "
  # 0:4 = 'Normal (<5)';
  # 5:9 = 'Mild (5-9)';
  # 10:14 = 'Moderate (10-14)';
  # 15:19 = 'Moderately severe (15-19)';
  # 20:hi = 'Severe (20+)'
  # ")
  df$phq_ <- car::recode(df$phq, "
  0:4 = 1;
  5:9 = 2;
  10:14 = 3;
  15:19 = 4;
  20:hi = 5
  ")
  
  # df$phq2t_ <- ifelse(df$phq2t >= 3, "≥3", "<3")
  df$phq2t_ <- ifelse(df$phq2t >= 3, 1, 0)
  
  # df$gad_ <- car::recode(df$gad, "
  # 0:4 = 'Very mild (<5)';
  # 5:9 = 'Mild (5-9)';
  # 10:14 = 'Moderate (10-14)';
  # 15:hi = 'Severe (15+)'
  # ")
  df$gad_ <- car::recode(df$gad, "
  0:4 = 1;
  5:9 = 2;
  10:14 = 3;
  15:hi = 4
  ")
  
  # df$gad2t_ <- ifelse(df$gad2t >= 3, "≥3", "<3")
  df$gad2t_ <- ifelse(df$gad2t >= 3, 1, 0)
  
  # df$moca_ <- ifelse(df$moca >= 22, "Normal", "MCI (<22)")
  df$mci <- ifelse(df$moca >= 22, 0, 1)
  
  pase_freq <- function(a, b){
    if (length(a)!=length(b)){
      stop("Lengths of a and b must be the same!")
    }
    var <- c()
    for (i in 1:length(a)){
      if (a[i] %in% 0){
        var <- c(var, 0)
      } else if (a[i] %in% 1){
        var <- c(var, c(0.11,0.32,0.64,1.07)[b[i]])
      } else if (a[i] %in% 2){
        var <- c(var, c(0.25,0.75,1.50,2.50)[b[i]])
      } else if (a[i] %in% 3){
        var <- c(var,c(0.43,1.29,2.57,4.29)[b[i]])
      } else if (a[i] %in% NA){
        var <- c(var, NA)
      } else {
        stop("Range exceeds 0-3, check first param!")
      }
    }
    return(var)
  }
  
  pase10 <- ifelse(df$pase10 == 0 | df$pase10b == 0, 0, df$pase10a/7)
  df$pase <- (20* pase_freq(df$pase2, df$pase2b)  + 21 * pase_freq(df$pase3, df$pase3b)  
              + 23* (pase_freq(df$pase4, df$pase4b) + pase_freq(df$pase5, df$pase5b)) + 30*pase_freq(df$pase6, df$pase6b)
              + 25*(df$pase7 + df$pase8) + 30*df$pase9a + 36*df$pase9b + 20*df$pase9c + 35*df$pase9d + 21*pase10)
  
  #                                           
  # df$efs1_ <- ifelse((df$efs1 %in% (1:2) | df$hcuhsp1a %in% 1 | df$hcuhsp1b %in% 1) & !(df$hcuhsp1a %in% 1 & df$hcuhsp1b %in% 1), 1,
  #                    ifelse(df$efs1 > 2 | df$hcuhsp1a %in% 2 | df$hcuhsp1b %in% 2 | (df$hcuhsp1a %in% 1 & df$hcuhsp1b %in% 1), 2,
  #                           ifelse(df$efs1 %in% 0 | df$hcuhsp1a %in% 0 | df$hcuhsp1b %in% 0, 0, NA)))
  
  return(subset(df, select = c(hypertension, bpi_s, bpi_i, hgs, hgs_, sar_, phq_, phq2t_, gad_, gad2t_, mci, iadl, pase)))
}

df <- cbind(df, categorize(df))


df$health <- (df$efs2-2)*-1 # reverse code self-rated health

df$hgs_m <- ifelse(df$gender=="M", df$hgs_, NA)
df$hgs_f <- ifelse(df$gender=="F", df$hgs_, NA)

df$ls_ <- ifelse(df$ls >= 3, 1, 0)

df$hcu2 <- ifelse(is.na(df$hcu2), pmin(df$hcu2a, 1), df$hcu2) # SOPC
df$hcu3 <- ifelse(is.na(df$hcu3), pmin(df$hcu3a, 1), df$hcu3) # GOPC

df$hcu1 <- ifelse(df$hcu4 >= 14, df$hcu4, df$hcu1)
df$hcu4 <- ifelse(df$hcu4 >= 14 & df$time == 0, NA,
                  ifelse(df$hcu4 >= 1 & df$time == 1, 1, df$hcu4))

# EFS (Edmonton Frail Scale)
df$hcuhsp1 <- ifelse(df$hcuhsp1a %in% 2 | df$hcuhsp1b %in% 2, 2,
                     ifelse(df$hcuhsp1a %in% 1 & df$hcuhsp1b %in% 1, 2,
                            ifelse(df$hcuhsp1a %in% 1 | df$hcuhsp1b %in% 1, 1,
                                   ifelse(df$hcuhsp1a %in% 0 | df$hcuhsp1b %in% 0, 0, NA)))) 
df$efs1_ <- ifelse(is.na(df$efs1), df$hcuhsp1, pmin(df$efs1, 2)) 

q_efs <- c("efs1_", sprintf("efs%s", c(2:13)))

df <- df %>%
  mutate(
    efs = rowSums(.[q_efs], na.rm = FALSE)
  )

df$efs14 <- car::recode(df$efs, "
0:5 = 1;
6:7 = 2;
8:9 = 3;
10:11 = 4;
12:hi = 5
")

df$moca_ <- car::recode(df$moca, "
lo:18 = 1;
19:25 = 2;
26:hi = 3
")

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
  df2 <- df %>% add_count(sopd) %>% filter(n==2) # keep only those with both T0 & T1
  
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


df$live_ <- ifelse(df$live2 == 1, 2,
                  ifelse(df$live1 == 1, 1, 
                         ifelse(df$live4 == 1, 3, NA)))

df$fs_ <- car::recode(df$fs, "
  0 = 0;
  1:2 = 1;
  3:hi = 2
  ")

df$smu3_ <- ifelse(df$smu3 == 5, 0, 1)

vars <- c("age", "gender", "live_", "bmi", "bmi2", "md3", "md1", "pain", 
          "sar", "sar_", "fs", "fs_", "phq", "phq_", "gad", "gad_", "moca", "mci", "efs4", "ls", "ls_", "smu3_", "pase",
          "hgs", "hgs_", "hgs_m", "hgs_f", "eq5d", "eq5d6", "moca_")
vars_ordinal <- c("gender", "live_", "bmi2",  "md3", "md1", "pain", "sar_",  "fs_", "phq_", "gad_", "mci", "efs4", "ls_", "smu3_", "hgs_", "hgs_m", "hgs_f", "moca_")

tableone::CreateTableOne(data =  df[df$time %in% 0,],
                         vars = vars, factorVars = vars_ordinal) %>%
  print(showAllLevels = TRUE) %>% clipr::write_clip()
