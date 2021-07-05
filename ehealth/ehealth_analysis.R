rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(labelled)
library(eq5d)
library(lubridate) # interval() & duration()
library(ggplot2)
library(lme4)
library(lmerTest)

# import cleaned data
setwd(sprintf("~%s/ehealth", setpath))
df <- readRDS("ehealth_data.rds")
wbs <- readRDS("wbs_data.rds")

df %>% select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
df %>% select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
df %>% select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
df %>% select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
df %>% select(starts_with("diet_")) %>% colnames(.) -> diet_
df %>% select(starts_with("eq5d")) %>% colnames(.) -> eq5d_

# restrict sample to age >= 60 ----
df <- df[as.Date(df$ehealth_eval_timestamp) <= as.Date('2021-07-04'),]
# df <- df[(df$ehealth_eval_timestamp) <= ('2021-06-28 10:00:00 HKT'),]
df <- df[which(df$age >= 60),]

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
")

# pre-post results for powerpoint ----

to_character_df <- function(df, vars){
  for (var in vars) {
    df[[var]] <- to_character(df[[var]])
  }
  return(df)
}

to_English <- function(df){  
  for (var in self_efficacy_){
    df[[var]] <- to_labelled(to_factor(df[[var]]))
    valLabels <- val_labels(df[[var]])
    label_order <- c(which(names(valLabels) == "完全正確"),
                     which(names(valLabels) == "多數正確"),
                     which(names(valLabels) == "尚算正確"),
                     which(names(valLabels) == "完全不正確"))
    names(valLabels)[names(valLabels) == "完全正確"] <- "Absolutely true"
    names(valLabels)[names(valLabels) == "多數正確"] <- "Mostly true"
    names(valLabels)[names(valLabels) == "尚算正確"] <- "Partly true"
    names(valLabels)[names(valLabels) == "完全不正確"] <-  "Absolutely not true"
    val_labels(df[[var]]) <- valLabels[label_order]
    df[[var]] <- to_factor(df[[var]])
  }
  var <- "pase_c_1"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  names(valLabels)[names(valLabels) == "冇"] <- "None"
  names(valLabels)[names(valLabels) == "好少 (1至2日)"] <- "Rarely (1-2 days)"
  names(valLabels)[names(valLabels) == "有時 (3至4日)"] <- "Sometimes (3-4 days)"
  names(valLabels)[names(valLabels) == "經常 (5至7日)"] <-  "Often (5-7 days)"
  val_labels(df[[var]]) <- valLabels
  df[[var]] <- to_factor(df[[var]])
  
  var <- "pase_c_1_2"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "少過1個鐘"),
                   which(names(valLabels) == "1至2個鐘"),
                   which(names(valLabels) == "2至4個鐘"),
                   which(names(valLabels) == "多過4個鐘"))
  names(valLabels)[names(valLabels) == "少過1個鐘"] <- "Less than 1 hour"
  names(valLabels)[names(valLabels) == "1至2個鐘"] <- "1-2 hours"
  names(valLabels)[names(valLabels) == "2至4個鐘"] <- "2-4 hours"
  names(valLabels)[names(valLabels) == "多過4個鐘"] <-  "More than 4 hours"
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "pase_c_11"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "有"),
                   which(names(valLabels) == "冇"))
  names(valLabels)[names(valLabels) == "冇"] <- "No"
  names(valLabels)[names(valLabels) == "有"] <- "Yes"
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "pase_c_12_1"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "Yes"),
                   which(names(valLabels) == "No"))
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  for (var in matrix_diet_){
    df[[var]] <- to_labelled(to_factor(df[[var]]))
    valLabels <- val_labels(df[[var]])
    # label_order <- c(which(names(valLabels) == "0次"),
    #                  which(names(valLabels) == "1至2次"),
    #                  which(names(valLabels) == "3至4次"),
    #                  which(names(valLabels) == "5至6次"),
    #                  which(names(valLabels) == "7次或以上"))
    names(valLabels)[names(valLabels) == "0次"] <- "None"
    names(valLabels)[names(valLabels) == "1至2次"] <- "1-2 times"
    names(valLabels)[names(valLabels) == "3至4次"] <- "3-4 times"
    names(valLabels)[names(valLabels) == "5至6次"] <-  "5-6 times"
    names(valLabels)[names(valLabels) == "7次或以上"] <-  "7 times or more"
    val_labels(df[[var]]) <- valLabels
    df[[var]] <- to_factor(df[[var]])
  }
  
  var <- "diet_dp1"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "每餐0碗"),
                   which(names(valLabels) == "每餐約小半碗或以下"),
                   which(names(valLabels) == "每餐約半碗"),
                   which(names(valLabels) == "每餐約大半碗至1碗"),
                   which(names(valLabels) == "每餐多於1碗"))
  
  names(valLabels)[names(valLabels) == "每餐多於1碗"] <- "> 1 bowl"
  names(valLabels)[names(valLabels) == "每餐約大半碗至1碗"] <- "0.5-1 bowl"
  names(valLabels)[names(valLabels) == "每餐約小半碗或以下"] <- "< 0.5 bowl"
  names(valLabels)[names(valLabels) == "每餐約半碗"] <- "0.5 bowl"
  names(valLabels)[names(valLabels) == "每餐0碗"] <- "None"
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  
  var <- "diet_dp3"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "每天0碗"),
                   which(names(valLabels) == "每天約小半碗或以下"),
                   which(names(valLabels) == "每天約半碗"),
                   which(names(valLabels) == "每天約1碗"),
                   which(names(valLabels) == "每天多於1碗"))
  names(valLabels)[names(valLabels) == "每天0碗"] <- "None"
  names(valLabels)[names(valLabels) == "每天多於1碗"] <- "> 1 bowl"
  names(valLabels)[names(valLabels) == "每天約1碗"] <- "1 bowl"
  names(valLabels)[names(valLabels) == "每天約小半碗或以下"] <- "< 0.5 bowl"
  names(valLabels)[names(valLabels) == "每天約半碗"] <- "0.5 bowl"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "diet_dp4"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "每天0份"),
                   which(names(valLabels) == "每天約少於1份﹙即少於半碗切粒水果或少於1個中型水果或少於1湯匙乾果或少於半條香蕉或"),
                   which(names(valLabels) == "每天約1份 ﹙即半碗切粒水果或1個中型水果或1湯匙乾果或半條香蕉或大半杯鮮榨果汁﹚"),
                   which(names(valLabels) == "每天約2份 ﹙即1碗切粒水果或2個中型水果或2湯匙乾果或1條香蕉或1.5杯鮮榨果汁﹚"),
                   which(names(valLabels) == "每天多於2份 ﹙即多於1碗切粒水果或多於2個中型水果或多於2湯匙乾果或多於1條香蕉或多於"))
  names(valLabels)[names(valLabels) == "每天0份"] <- "None"
  names(valLabels)[names(valLabels) == "每天多於2份 ﹙即多於1碗切粒水果或多於2個中型水果或多於2湯匙乾果或多於1條香蕉或多於"] <- "> 2 servings"
  names(valLabels)[names(valLabels) == "每天約1份 ﹙即半碗切粒水果或1個中型水果或1湯匙乾果或半條香蕉或大半杯鮮榨果汁﹚"] <- "1 serving"
  names(valLabels)[names(valLabels) == "每天約2份 ﹙即1碗切粒水果或2個中型水果或2湯匙乾果或1條香蕉或1.5杯鮮榨果汁﹚"] <- "2 servings"
  names(valLabels)[names(valLabels) == "每天約少於1份﹙即少於半碗切粒水果或少於1個中型水果或少於1湯匙乾果或少於半條香蕉或"] <- "< 1 serving"
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "diet_dp5"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "每天0份"),
                   which(names(valLabels) == "每天約少於1份"),
                   which(names(valLabels) == "每天約1份"),
                   which(names(valLabels) == "每天約2至3份"),
                   which(names(valLabels) == "每天多於3份"))
  names(valLabels)[names(valLabels) == "每天0份"] <- "None"
  names(valLabels)[names(valLabels) == "每天多於3份"] <- "> 3 servings"
  names(valLabels)[names(valLabels) == "每天約1份"] <- "1 serving"
  names(valLabels)[names(valLabels) == "每天約2至3份"] <- "2-3 servings"
  names(valLabels)[names(valLabels) == "每天約少於1份"] <- "< 1 serving"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "eq5d_mobility"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  
  label_order <- c(which(names(valLabels) == "我無法行動。"),
                   which(names(valLabels) == "我的行動有嚴重問題。"),
                   which(names(valLabels) == "我的行動有中度問題。"),
                   which(names(valLabels) == "我的行動有輕微問題。"),
                   which(names(valLabels) == "我可以四處走動，沒有任何問題。"))
  names(valLabels)[names(valLabels) == "我無法行動。"] <- "Unable to walk about"
  names(valLabels)[names(valLabels) == "我的行動有嚴重問題。"] <- "Severe problems"
  names(valLabels)[names(valLabels) == "我的行動有中度問題。"] <- "Moderate problems"
  names(valLabels)[names(valLabels) == "我的行動有輕微問題。"] <- "Slight problems"
  names(valLabels)[names(valLabels) == "我可以四處走動，沒有任何問題。"] <- "No problems"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "eq5d_self_care"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "我無法進行平常活動。"),
                   which(names(valLabels) == "我在洗澡或穿衣服方面有嚴重問題。"),
                   which(names(valLabels) == "我在洗澡或穿衣服方面有中度問題。"),
                   which(names(valLabels) == "我在洗澡或穿衣服方面有輕微問題。"),
                   which(names(valLabels) == "我在洗澡或穿衣服方面沒有任何問題。"))
  names(valLabels)[names(valLabels) == "我無法進行平常活動。"] <- "Unable to wash or dress oneself"
  names(valLabels)[names(valLabels) == "我在洗澡或穿衣服方面有嚴重問題。"] <- "Severe problems"
  names(valLabels)[names(valLabels) == "我在洗澡或穿衣服方面有中度問題。"] <- "Moderate problems"
  names(valLabels)[names(valLabels) == "我在洗澡或穿衣服方面有輕微問題。"] <- "Slight problems"
  names(valLabels)[names(valLabels) == "我在洗澡或穿衣服方面沒有任何問題。"] <- "No problems"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "eq5d_usual_activity"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "我無法進行日常活動。"),
                   which(names(valLabels) == "我在進行平常活動方面有嚴重問題。"),
                   which(names(valLabels) == "我在進行平常活動方面有中度問題。"),
                   which(names(valLabels) == "我在進行平常活動方面有輕微問題。"),
                   which(names(valLabels) == "我能進行平常活動，沒有任何問題。"))
  names(valLabels)[names(valLabels) == "我無法進行日常活動。"] <- "Unable to do usual activities"
  names(valLabels)[names(valLabels) == "我在進行平常活動方面有嚴重問題。"] <- "Severe problems"
  names(valLabels)[names(valLabels) == "我在進行平常活動方面有中度問題。"] <- "Moderate problems"
  names(valLabels)[names(valLabels) == "我在進行平常活動方面有輕微問題。"] <- "Slight problems"
  names(valLabels)[names(valLabels) == "我能進行平常活動，沒有任何問題。"] <- "No problems"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "eq5d_pain_discomfort"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "我覺得極度疼痛或不舒服。"),
                   which(names(valLabels) == "我覺得嚴重疼痛或不舒服。"),
                   which(names(valLabels) == "我覺得中度疼痛或不舒服。"),
                   which(names(valLabels) == "我覺得輕微疼痛或不舒服。"),
                   which(names(valLabels) == "我沒有任何疼痛或不舒服。"))
  names(valLabels)[names(valLabels) == "我覺得極度疼痛或不舒服。"] <- "Extreme pain or discomfort"
  names(valLabels)[names(valLabels) == "我覺得嚴重疼痛或不舒服。"] <- "Severe pain or discomfort"
  names(valLabels)[names(valLabels) == "我覺得中度疼痛或不舒服。"] <- "Moderate pain or discomfort"
  names(valLabels)[names(valLabels) == "我覺得輕微疼痛或不舒服。"] <- "Slight pain or discomfort"
  names(valLabels)[names(valLabels) == "我沒有任何疼痛或不舒服。"] <- "No pain or discomfort"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])
  
  var <- "eq5d_anxiety_depression"
  df[[var]] <- to_labelled(to_factor(df[[var]]))
  valLabels <- val_labels(df[[var]])
  label_order <- c(which(names(valLabels) == "我覺得極度焦慮或沮喪。"),
                   which(names(valLabels) == "我覺得嚴重焦慮或沮喪。"),
                   which(names(valLabels) == "我覺得中度焦慮或沮喪。"),
                   which(names(valLabels) == "我覺得輕微焦慮或沮喪。"),
                   which(names(valLabels) == "我不覺得焦慮或沮喪。"))
  names(valLabels)[names(valLabels) == "我覺得極度焦慮或沮喪。"] <- "Extremely anxious or depressed"
  names(valLabels)[names(valLabels) == "我覺得嚴重焦慮或沮喪。"] <- "Severely anxious or depressed"
  names(valLabels)[names(valLabels) == "我覺得中度焦慮或沮喪。"] <- "Moderately anxious or depressed"
  names(valLabels)[names(valLabels) == "我覺得輕微焦慮或沮喪。"] <- "Slightly anxious or depressed"
  names(valLabels)[names(valLabels) == "我不覺得焦慮或沮喪。"] <- "Not anxious or depressed"
  
  val_labels(df[[var]]) <- valLabels[label_order]
  df[[var]] <- to_factor(df[[var]])

  return(df)
}

allVars <- c("use_health_service_8", "amic", "amic_sum", 
             "self_efficacy", "self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5",
             "eq5d", "eq5d_mobility", "eq5d_self_care", "eq5d_usual_activity", "eq5d_pain_discomfort", "eq5d_anxiety_depression", "eq5d_health", 
             "satisfaction_1", "satisfaction_2", 
             "pase_c", "pase_c_1", "pase_c_1_2", "pase_c_11", "pase_c_11_1", "pase_c_12_1", "pase_c_12",
             "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
             "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5")

ordinalVars <- c("amic",
                 "self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5", 
                 "eq5d_mobility", "eq5d_self_care", "eq5d_usual_activity", "eq5d_pain_discomfort", "eq5d_anxiety_depression",
                 "pase_c_1", "pase_c_1_2", "pase_c_11", "pase_c_12_1", 
                 "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
                 "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5")

gen_table <- function(df, vars, ordinalVars, medianVars){ 
  table <- data.frame(matrix(ncol = 7,  nrow = 0))
  row_count <- 1
  col_bl <- 3
  col_bl2 <- 4 # matched baseline 
  col_f1 <- 5 # matched baseline 
  col_dif <- 6 # difference
  col_pval <- 7 # p-value
  
  df2 <- df %>% add_count(member_id) %>% filter(n == 2) # keep only those with both T0 & T1
  
  dfwide <- reshape(data=df, idvar= c("member_id"),
                     timevar = "time",
                     direction="wide")
  
  dfwide2 <- reshape(data=df2, idvar= c("member_id"),
                     timevar = "time",
                     direction="wide")
  
    df_en <- to_English(to_character_df(df, ordinalVars))
  df2_en <- to_English(to_character_df(df2, ordinalVars))
  
  colnames(table)[1] <- ""
  colnames(table)[2] <- ""
  colnames(table)[col_bl] <- "Baseline (all)"
  colnames(table)[col_bl2] <- "Baseline (paired)"
  colnames(table)[col_f1] <- "6 months (paired)"
  colnames(table)[col_dif] <- "Difference"
  colnames(table)[col_pval] <- "p-value"
  
  table[row_count, 1] <- "N"
  table[row_count, 2] <- ""
    table[row_count, col_bl] <-  nrow(dfwide)
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
    if (var %in% ordinalVars){
      table[row_count, 1] <- var
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = TRUE) 
      if (wilcox_test$p.value < 0.001){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- wilcox_test$p.value %>% round(digits = 3)
      }

      for (val in unique(df_en[[var]])[order(unique(df_en[[var]]))]){
        table[row_count, 2] <- val
        
        if (val %in% NA){
          table[row_count, 2] <- "N/A"
        }
        
        table[row_count, col_bl] <- 
          table(df_en[[var]],useNA = "ifany") %>% 
          prop.table() %>% as.data.frame() %>% 
          .[which(.$Var1 %in% val),"Freq"] 
        
        if (val %in% unique(df2_en[[var]][which(df2$time == 0)])){
          table[row_count, col_bl2] <- iferror(
            table(df2_en[[var]][which(df2$time == 0)],useNA = "ifany") %>%
            prop.table() %>% as.data.frame() %>%
            .[which(.$Var1 %in% val),"Freq"], NA)
        } else {
          table[row_count, col_bl2]  <- 0
        }
        
        if (val %in% unique(df2_en[[var]][which(df2$time == 1)])){
          table[row_count, col_f1] <-
            table(df2_en[[var]][which(df2$time == 1)],useNA = "ifany") %>%
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
      
      table[row_count, 2] <- "mean (sd)"
      table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df$time == 0)])
      table[row_count, col_bl2] <- get_mean_sd(df2[[var]][which(df2$time == 0)])
      table[row_count, col_f1] <-  get_mean_sd(df2[[var]][which(df2$time == 1)])
      
      t_test <-  
        t.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = TRUE) 
      table[row_count, col_dif] <- t_test$estimate %>% round(digits = 1)*-1
      if (t_test$p.value < 0.001){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- t_test$p.value %>% round(digits = 3)
      }
      
      row_count <- row_count + 1
    }
    
    if (var %in% medianVars){
      table[row_count, 2] <- "median (Q1-Q3)"
      table[row_count, col_bl] <-  get_median_iqr(df[[var]])
      table[row_count, col_bl2] <- get_median_iqr(df2[[var]][which(df2$time == 0)])
      table[row_count, col_f1] <-  get_median_iqr(df2[[var]][which(df2$time == 1)])
      
      table[row_count, col_dif] <- (median(df2[[var]][which(df2$time == 1)], na.rm = TRUE) - median(df2[[var]][which(df2$time == 0)], na.rm = TRUE)) 
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = TRUE) 
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

medianVars <- c("use_health_service_8")

Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(df, allVars[], ordinalVars, medianVars) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 
# trend plots ----
df2 <- df %>% add_count(member_id) %>% filter(n == 2) # keep only those with both T0 & T1

df <- df[order(df$ehealth_eval_timestamp), ] # order by timestamp
df2 <- df2[order(df2$ehealth_eval_timestamp), ] # order by timestamp

dfwide <- reshape(data=df, idvar= c("member_id"),
                  timevar = "time",
                  direction="wide")

dfwide2 <- reshape(data=df2, idvar= c("member_id"),
                   timevar = "time",
                   direction="wide")

dfwide <- dfwide[order(dfwide$ehealth_eval_timestamp.0), ] # order by timestamp
dfwide2 <- dfwide2[order(dfwide2$ehealth_eval_timestamp.0), ] # order by timestamp
dfwide3 <- dfwide2
dfwide3 <- dfwide3[order(dfwide3$ehealth_eval_timestamp.1), ] # order by timestamp

var_names <- t(array(c(c("use_health_service_8", "Out-of-pocket payments"),  
                       c("amic", "Proportion of AMIC >= 3"),  
                       c("amic_sum", "AMIC score"), 
                       c("self_efficacy", "Self-efficacy - Total"), 
                       c("self_efficacy_1", "Self-efficacy - Physical health"),
                       c("self_efficacy_2", "Self-efficacy - Mental health"),
                       c("self_efficacy_3", "Self-efficacy - Social support"),
                       c("self_efficacy_4", "Self-efficacy - Healthy diet"),
                       c("self_efficacy_5", "Self-efficacy - Exercise"),
                       c("eq5d", "Health related quality of life (EQ-5D-5L)"),
                       c("eq5d_mobility", "EQ-5D-5L Mobility"),
                       c("eq5d_self_care", "EQ-5D-5L Self care"),
                       c("eq5d_usual_activity", "EQ-5D-5L Usual activity"),
                       c("eq5d_pain_discomfort", "EQ-5D-5L Pain/discomfort"),
                       c("eq5d_anxiety_depression", "EQ-5D-5L Anxiety/depression"),
                       c("eq5d_health", "Health related quality of life (EQ VAS)"),
                       c("satisfaction_1", "Satisfaction with all health care professionals"),  
                       c("satisfaction_2", "Satisfaction with all health care professionals"),  
                       c("pase_c", "Physical activity	(PASE-C)"),  
                       c("pase_c_1", "Sitting per week (categorical)"),  
                       c("pase_c_1_2", "Sitting per day (categorical)"),  
                       c("pase_c_11", "Proportion of walking as exercise daily"),  
                       c("pase_c_11_1", "Number of streets walked as exercise"),  
                       c("pase_c_12_1", "Proportion of walking in general daily"),  
                       c("pase_c_12", "Number of streets walked (excl. exercise)"),  
                       c("matrix_diet_dh3", "High-sugar/high-fat snacks per week (categorical)"),  
                       c("matrix_diet_dh4", "Processed/canned foods (categorical)"),  
                       c("matrix_diet_dh7", "Three main meals daily (categorical)"),  
                       c("matrix_diet_dh8", "Three meals at regular hours (categorical)"),  
                       c("diet_dp1", "Amount of grains in a meal (categorical)"),  
                       c("diet_dp3", "Amount of vegetables per day (categorical)"),  
                       c("diet_dp4", "Amount of fruit per day (categorical)"),  
                       c("diet_dp5", "Amount of meat/poultry/fish/egg per day (categorical)")), dim = c(2,33)))

setwd(sprintf("~%s/ehealth/slides", setpath))
pdf("plot_trends.pdf", height = 28/2.54/1.6, width = 50/2.54/1.6)
for (var in allVars){
  # var <- "use_health_service_8"
  func <- "mean"
  test <- rbind(
    data.frame(y = zoo::rollapplyr(dfwide[[paste0(var, ".0")]], seq_along(dfwide[[paste0(var, ".0")]]), get(func), na.rm = TRUE), timestamp = dfwide$ehealth_eval_timestamp.0, time = "Baseline (all)"),
    data.frame(y = zoo::rollapplyr(dfwide2[[paste0(var, ".0")]], seq_along(dfwide2[[paste0(var, ".0")]]), get(func), na.rm = TRUE), timestamp = dfwide2$ehealth_eval_timestamp.0, time = "Baseline (paired)"),
    data.frame(y = zoo::rollapplyr(dfwide3[[paste0(var, ".1")]], seq_along(dfwide3[[paste0(var, ".1")]]), get(func), na.rm = TRUE), timestamp = dfwide3$ehealth_eval_timestamp.1, time = "Six months (paired)"))
  test$timestamp <- test$timestamp %>% as.Date
  plot <- ggplot(data=test, aes(x=timestamp, y=y, group = time, colour = factor(time))) +
    labs(x = "Time", y = var_names[which(var_names==var),2], color = "Time point") +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels="%b %Y") +
    theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1)) +
    ylim(min(as.numeric(df[[var]]), na.rm=TRUE),max(test$y, na.rm=TRUE))
  print(plot)
}
dev.off()

# plot data collection dates ----
# restrict to those with both T0 & T1
temp <- df %>%
  add_count(member_id) %>%
  filter(n == 2) 
# temp <- df
temp$time <- as.factor(temp$time)

ggplot(temp, aes(x=as.Date(ehealth_eval_timestamp), color = time)) + 
  geom_histogram(bins = 100, alpha=0.2,position="identity") +
  # geom_jitter(height = 0.25) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels="%b %Y") +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1))
