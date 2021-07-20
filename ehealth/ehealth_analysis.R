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
library(ordinal) # clm & clmm
library(multcomp) # glht

# import cleaned data
setwd(sprintf("~%s/ehealth", setpath))
df <- readRDS("ehealth_data.rds")
wbs <- readRDS("wbs_data.rds")

df %>% dplyr::select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
df %>% dplyr::select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
df %>% dplyr::select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
df %>% dplyr::select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
df %>% dplyr::select(starts_with("diet_")) %>% colnames(.) -> diet_
df %>% dplyr::select(starts_with("eq5d")) %>% colnames(.) -> eq5d_

# set variable names ----
var_names <- t(array(c(c("use_health_service_8", "Out-of-pocket payments (HK$)"),  
                       c("amic", "Memory symptoms, proportion of AMIC >= 3 (lower=better)"),  
                       c("amic_sum", "Memory symptoms score (AMIC, lower=better)"), 
                       c("self_efficacy", "Self-efficacy - Total"), 
                       c("self_efficacy_1", "Self-efficacy - Physical health"),
                       c("self_efficacy_2", "Self-efficacy - Mental health"),
                       c("self_efficacy_3", "Self-efficacy - Social support"),
                       c("self_efficacy_4", "Self-efficacy - Healthy diet"),
                       c("self_efficacy_5", "Self-efficacy - Exercise"),
                       c("eq5d", "Health related quality of life (EQ-5D-5L)"),
                       c("eq5d_mobility", "Health related quality of life - Mobility"),
                       c("eq5d_self_care", "Health related quality of life - Self care"),
                       c("eq5d_usual_activity", "Health related quality of life - Usual activity"),
                       c("eq5d_pain_discomfort", "Health related quality of life - Pain/discomfort (higher=better)"),
                       c("eq5d_anxiety_depression", "Health related quality of life - Anxiety/depression"),
                       c("eq5d_health", "Self-rated Health (EQ VAS)"),
                       c("satisfaction_1", "Satisfaction with all health care professionals"),  
                       c("satisfaction_2", "Satisfaction with eHealth health care and centre staff"),  
                       c("pase_c", "Physical activity score	(PASE-C)"),  
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

# restrict sample to age >= 60 ----
# df <- df[as.Date(df$ehealth_eval_timestamp) <= as.Date('2021-07-04'),]
# df <- df[(df$ehealth_eval_timestamp) <= ('2021-06-28 10:00:00 HKT'),]
df <- df[which(df$age >= 60),]

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
") # (T0 = baseline, T1 = 6mth, T2 = 12mth, T3 = = 18mth)

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

medianVars <- c("use_health_service_8")

gen_table <- function(df, vars, ordinalVars, medianVars, paired = TRUE, group = "time"){ 
  table <- data.frame(matrix(ncol = 7,  nrow = 0))
  row_count <- 1
  col_bl <- 3
  col_bl2 <- 4 # matched baseline 
  col_f1 <- 5 # matched baseline 
  col_dif <- 6 # difference
  col_pval <- 7 # p-value
  
  df2 <- df %>% add_count(member_id) %>% filter(n == 2) # keep only those with both T0 & T1
  
  dfwide <- reshape(data=df, idvar= c("member_id"),
                     timevar = group,
                     direction="wide")
  
  dfwide2 <- reshape(data=df2, idvar= c("member_id"),
                     timevar = group,
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
        wilcox.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = paired) 
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
        
        if (val %in% unique(df2_en[[var]][which(df2[[group]] == 0)])){
          table[row_count, col_bl2] <- iferror(
            table(df2_en[[var]][which(df2[[group]] == 0)],useNA = "ifany") %>%
            prop.table() %>% as.data.frame() %>%
            .[which(.$Var1 %in% val),"Freq"], NA)
        } else {
          table[row_count, col_bl2]  <- 0
        }
        
        if (val %in% unique(df2_en[[var]][which(df2[[group]] == 1)])){
          table[row_count, col_f1] <-
            table(df2_en[[var]][which(df2[[group]] == 1)],useNA = "ifany") %>%
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
      table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df[[group]] == 0)])
      table[row_count, col_bl2] <- get_mean_sd(df2[[var]][which(df2[[group]] == 0)])
      table[row_count, col_f1] <-  get_mean_sd(df2[[var]][which(df2[[group]] == 1)])
      
      t_test <-  
        t.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = paired) 
      table[row_count, col_dif] <-  (mean(df2[[var]][which(df2[[group]] == 1)], na.rm = TRUE) - mean(df2[[var]][which(df2[[group]] == 0)], na.rm = TRUE)) %>% round(digits = 2)
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
      table[row_count, col_bl2] <- get_median_iqr(df2[[var]][which(df2[[group]] == 0)])
      table[row_count, col_f1] <-  get_median_iqr(df2[[var]][which(df2[[group]] == 1)])
      
      table[row_count, col_dif] <- (median(df2[[var]][which(df2[[group]] == 1)], na.rm = TRUE) - median(df2[[var]][which(df2[[group]] == 0)], na.rm = TRUE)) 
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = paired) 
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

gen_table2 <- function(df, vars, ordinalVars, medianVars, paired = TRUE, group = "time"){ 
  table <- data.frame(matrix(ncol = 7,  nrow = 0))
  row_count <- 1
  col_bl <- 3
  col_bl2 <- 4 # matched baseline 
  col_f1 <- 5 # matched baseline 
  col_dif <- 6 # difference
  col_pval <- 7 # p-value
  
  df2 <- df %>% add_count(member_id) %>% filter(n == 2) # keep only those with both T0 & T1
  
  dfwide <- reshape(data=df, idvar= c("member_id"),
                    timevar = group,
                    direction="wide")
  
  dfwide2 <- reshape(data=df2, idvar= c("member_id"),
                     timevar = group,
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
    print(var)
    if (var %in% ordinalVars){
      table[row_count, 1] <- var
      
      count <- 2 # starting at 2+ due to 1 sometimes would give erroneous highly significant p-value results
      clmm_model <- 
        clmm(as.factor(get(var)) ~ time + (1 | member_id), data = df2, link="logit", Hess=TRUE, na.action=na.omit, nAGQ=count)
      trycatchNA(clmm_p <- summary(clmm_model)$coef["time",4])
      # print(summary(clmm_model))
      while( is.na(table[row_count, col_pval]) & count <= 27){
        print(count)
        clmm_model <- update(clmm_model, nAGQ = count)
        # print(summary(clmm_model))
        trycatchNA(clmm_or <- exp(summary(clmm_model)$coef["time",1]))
        trycatchNA(clmm_p <- summary(clmm_model)$coef["time",4])
        table[row_count, col_pval] <- ifelse(clmm_p %in% c("NA", "NaN"), NA, clmm_p)
        
        count <- max(count+1, floor((count^2)/3)) # fast growing count
      }   
      table[row_count, col_pval] <- ifelse(clmm_p < 0.001, "<0.001", round(table[row_count, col_pval], 3))

      for (val in unique(df_en[[var]])[order(unique(df_en[[var]]))]){
        table[row_count, 2] <- val
        
        if (val %in% NA){
          table[row_count, 2] <- "N/A"
        }
        
        table[row_count, col_bl] <- 
          table(df_en[[var]],useNA = "ifany") %>% 
          prop.table() %>% as.data.frame() %>% 
          .[which(.$Var1 %in% val),"Freq"] 
        
        if (val %in% unique(df2_en[[var]][which(df2[[group]] == 0)])){
          table[row_count, col_bl2] <- iferror(
            table(df2_en[[var]][which(df2[[group]] == 0)],useNA = "ifany") %>%
              prop.table() %>% as.data.frame() %>%
              .[which(.$Var1 %in% val),"Freq"], NA)
        } else {
          table[row_count, col_bl2]  <- 0
        }
        
        if (val %in% unique(df2_en[[var]][which(df2[[group]] == 1)])){
          table[row_count, col_f1] <-
            table(df2_en[[var]][which(df2[[group]] == 1)],useNA = "ifany") %>%
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
      table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df[[group]] == 0)])
      table[row_count, col_bl2] <- get_mean_sd(df2[[var]][which(df2[[group]] == 0)])
      table[row_count, col_f1] <-  get_mean_sd(df2[[var]][which(df2[[group]] == 1)])
      
      t_test <-  
        t.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = paired) 
      table[row_count, col_dif] <-  (mean(df2[[var]][which(df2[[group]] == 1)], na.rm = TRUE) - mean(df2[[var]][which(df2[[group]] == 0)], na.rm = TRUE)) %>% round(digits = 2)
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
      table[row_count, col_bl2] <- get_median_iqr(df2[[var]][which(df2[[group]] == 0)])
      table[row_count, col_f1] <-  get_median_iqr(df2[[var]][which(df2[[group]] == 1)])
      
      table[row_count, col_dif] <- (median(df2[[var]][which(df2[[group]] == 1)], na.rm = TRUE) - median(df2[[var]][which(df2[[group]] == 0)], na.rm = TRUE)) 
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".0")]], dfwide2[[paste0(var, ".1")]], paired = paired) 
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

Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(df, allVars[], ordinalVars, medianVars) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 

# baseline & follow-up in same period ----
df <- df[, c(1:105)]
temp <- df[as.Date(df$ehealth_eval_timestamp) >= as.Date('2021-04-07') & df$time == 1 |
               (as.Date(df$ehealth_eval_timestamp) <= as.Date('2020-11-20') | 
                  as.Date(df$ehealth_eval_timestamp) >= as.Date('2021-04-07'))  & df$time == 0 , ]

temp1 <- temp %>% add_count(member_id) %>% filter(n == 2)
temp2 <- temp %>% add_count(member_id) %>% filter(as.Date(ehealth_eval_timestamp) >= as.Date('2021-04-07') & n == 1 & 
                                                    member_id %!in% unique(temp1$member_id) & time == 0)
temp <- rbind(temp1, temp2) 
rm(temp1, temp2)
# temp <- df[as.Date(df$ehealth_eval_timestamp) >= as.Date('2021-03-28')
#            # & as.Date(df$ehealth_eval_timestamp) <= as.Date('2021-05-15')
#            ,]
# temp <- df[as.Date(df$ehealth_eval_timestamp) > as.Date('2021-05-15')
#            ,]
# temp$time <- (temp$time-1)*-1

vars <- c("member_id", "gender", "dob", "wbs_survey_date",
          "marital", "educ", "living_status", "housing_type",
          "risk_score", "risk_level", "digital")

get_descstat <- function(df){
  for (var in vars){
    if (is.character(df[[var]]) |
        is.labelled(df[[var]])){
      df[[var]] <- to_character(df[[var]])

      if (!(var %in% c("educ"))){ # exceptions
        xLabels = names(table(df[[var]]))[order(table(df[[var]]), decreasing = TRUE)] # order factor levels in descending order
        df[[var]] = factor(df[[var]], levels=xLabels)

      }
    }
  }

  allVars <- c("age", "age_group", "gender", "marital", "educ", "living_status", "housing_type", "risk_score")
  catVars <- c("age_group", "gender", "marital", "educ", "living_status", "housing_type")
  tableone::CreateTableOne(data =  df,
                           strata = c("time"),
                           vars = allVars, factorVars = catVars) %>%
    print(showAllLevels = TRUE) %>% clipr::write_clip()
}
# get_descstat(temp)

library(MatchIt)
library(optmatch) # for optimal matching
library(Matching) # for genetic matchin
library(rgenoud) # for genetic matchin

select_vars <-  c("time", "gender", "age", "living_status",
                  # "marital", "educ", "housing_type",
                  "risk_score")
temp <- temp[complete.cases(temp[, select_vars]),]
temp$marital <- to_factor(temp$marital)
temp$educ <- to_factor(temp$educ)
temp$living_status <- to_factor(temp$living_status)
temp$housing_type <- to_factor(temp$housing_type)
# matched.out <- matchit(time ~ gender + age + marital + educ + living_status + housing_type + risk_score,
#                  data = temp, method = "optimal"
#                  # , distance='mahalanobis'
#                  )
matched.out <- matchit(time ~ gender + age + housing_type + risk_score,
                 data = temp, method = "optimal"
                 # , caliper = 0.25
                 # , distance='mahalanobis'
                 )

# temp$time <- (temp$time-1)*-1

temp_matched <- match.data(matched.out, data = temp) 
temp_matched$member_id_old <- temp_matched$member_id
temp_matched$member_id <- temp_matched$subclass
Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(temp_matched, allVars[], ordinalVars, medianVars, paired =  TRUE) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 
plot(summary(matched.out)
     , xlim=c(0,0.25)
     )

# # check robustness of the matching method for T0 between matched and random subset of those with both T0 & T1
# temp <- df
# temp <- temp %>% add_count(member_id) # create variable n 
# member_ids_both <- unique(temp$member_id[temp$n == 2 & as.Date(temp$ehealth_eval_timestamp) >= as.Date('2021-02-01')])
# temp$n <- NULL
# member_ids_both <- sample(member_ids_both, length(member_ids_both)*2/3)
# temp$test <- ifelse(as.Date(temp$ehealth_eval_timestamp) < as.Date('2020-11-01') &
#                       temp$member_id %in% member_ids_both, 1, 0)
# 
# temp$int_t0 <- ifelse(as.Date(temp$ehealth_eval_timestamp) < as.Date('2020-11-01') &
#                         temp$member_id %in% member_ids_both, 1,
#                       ifelse(as.Date(temp$ehealth_eval_timestamp) < as.Date('2020-11-01') &
#                                temp$member_id %!in% member_ids_both, 0, NA))
# temp$housing_type <- to_factor(temp$housing_type)
# select_vars <-  c("int_t0", "gender", "age", "living_status",
#                   # "marital", "educ", "housing_type",
#                   "risk_score")
# temp <- temp[complete.cases(temp[, select_vars]),]
# matched.out <- matchit(int_t0 ~ gender + age + housing_type + risk_score,
#                        data = temp, method = "optimal"
#                        # , caliper = 0.25
#                        # , distance='mahalanobis'
# )
# temp_matched <- match.data(matched.out, data= temp) 
# temp_matched$member_id <- temp_matched$subclass
# Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
# gen_table(temp_matched, allVars[], ordinalVars, medianVars, paired =  TRUE, group = "int_t0") %>% clipr::write_clip()
# Sys.setlocale(locale =  "eng") 
# plot(summary(matched.out)
#      , xlim=c(0,0.25)
# ) 

# generate a table of results ----
temp_matched$int <- temp_matched$time
temp_matched$member_id <- temp_matched$member_id_old
df_matched <- merge(df, temp_matched[c("member_id", "int", "subclass")], 
            by=c("member_id"), all.x = TRUE)

df_matched$covid <- ifelse(df_matched$int == 0 & df_matched$time == 0, 1,
                        ifelse(df_matched$int == 1 & df_matched$time == 0, 0, 
                               ifelse(df_matched$int == 1 & df_matched$time == 1, 1, NA)))
df_matched$int <- ifelse(df_matched$covid == 0 & df_matched$int == 1, 0, df_matched$int) # set int group at baseline to be pre-covid baseline

# table <- combine_tables(NULL, 
#                         exponentiate = FALSE,
#                         # show_CI = 0.83,
#                         lmer(use_health_service_8~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(amic~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         # glmer(amic~ 1+covid+int+ (1| subclass), family = binomial, data = df_matched),
#                         lmer(amic_sum~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         # clmm(as.factor(amic_sum) ~ covid+int + (1 | subclass), data = df_matched, link="logit", Hess=TRUE, na.action=na.omit, nAGQ=5),
#                         lmer(self_efficacy~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(self_efficacy_1~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(self_efficacy_2~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(self_efficacy_3~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(self_efficacy_4~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(self_efficacy_5~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
# 
#                         lmer(eq5d~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(eq5d_mobility~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(eq5d_self_care~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(eq5d_usual_activity~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(eq5d_pain_discomfort~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(eq5d_anxiety_depression~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(eq5d_health~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
# 
#                         lmer(satisfaction_1~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(satisfaction_2~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         
#                         lmer(pase_c~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(pase_c_1~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(pase_c_1_2~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(pase_c_11~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         # glmer(pase_c_11~ 1+covid+int+ (1| subclass), family = binomial, data = df_matched),
#                         lmer(pase_c_11_1~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(pase_c_12_1~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         # glmer(pase_c_12_1~ 1+covid+int+ (1| subclass), family = binomial, data = df_matched),
#                         lmer(pase_c_12~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
# 
#                         lmer(matrix_diet_dh3~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(matrix_diet_dh4~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(matrix_diet_dh7~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(matrix_diet_dh8~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(diet_dp1~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(diet_dp3~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(diet_dp4~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched),
#                         lmer(diet_dp5~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched)
# )
# table %>% clipr::write_clip()

# plot covid & intervention effects ----
gen_coef <- function(model, coef_name, CI = 0.83){
  int_T0 <- summary(glht(model, linfct = "(Intercept) = 0"))
  int_T0_est <- summary(model)$coef["(Intercept)", 1]
  int_T0_se <- int_T0$test$sigma[[1]]
  
  ctrl_T1 <- summary(glht(model, linfct = "covid = 0")) 
  ctrl_T1_est <- int_T0_est + summary(model)$coef["covid", 1]
  ctrl_T1_se <- summary(glht(model, linfct = "(Intercept) + covid = 0"))$test$sigma[[1]]
  
  int_ <- summary(glht(model, linfct = "int = 0"))
  
  int_T1 <- summary(glht(model, linfct = "covid + int = 0")) 
  int_T1_est <- int_T0_est + summary(model)$coef["covid", 1] + summary(model)$coef["int", 1]
  # vcov_A <- vcov(model)["covid", "covid"]
  # vcov_B <- vcov(model)["int", "int"]
  # vcov_AB <- vcov(model)["covid", "int"]
  # int_T1_se <-  sqrt(vcov_A + vcov_B + 2* vcov_AB)
  int_T1_se <- summary(glht(model, linfct = "(Intercept) + covid + int = 0"))$test$sigma[[1]]
  
  alpha <- (1-CI)
  
  results0 <- data.frame(coef_name = coef_name,
                         estimate = NA,
                         se = NA,
                         lowerCI = NA,
                         upperCI = NA,
                         p_value = NA,
                         covid = 0,
                         int = 1,
                         legend = "Baseline/Control",
                         coef = NA, 
                         group = "")
  results1 <- data.frame(coef_name = coef_name,
                         estimate = int_T0_est,
                         se = int_T0_se,
                         lowerCI = int_T0_est + qnorm(alpha/2) * int_T0_se,
                         upperCI = int_T0_est + qnorm(1-(alpha/2)) * int_T0_se,
                         p_value = round_format(int_T0$test$pvalues[[1]], decimal_places = 4),
                         covid = 0,
                         int = 0,
                         legend = "Baseline/Control",
                         coef = NA, 
                         group = "Int T0")
  results2 <- data.frame(coef_name = coef_name,
                         estimate = ctrl_T1_est,
                         se = ctrl_T1_se,
                         lowerCI = ctrl_T1_est + qnorm(alpha/2) * ctrl_T1_se,
                         upperCI = ctrl_T1_est + qnorm(1-(alpha/2)) * ctrl_T1_se,
                         p_value = round_format(ctrl_T1$test$pvalues[[1]], decimal_places = 4),
                         covid = 1,
                         int = 0,
                         legend = "Baseline/Control",
                         coef = NA, 
                         group = "Ctrl T1")
  results3 <- data.frame(coef_name = coef_name,
                         estimate = (int_T0_est + ctrl_T1_est)/2,
                         se = NA,
                         lowerCI = min(int_T0_est, ctrl_T1_est),
                         upperCI = max(int_T0_est, ctrl_T1_est),
                         p_value = round_format(ctrl_T1$test$pvalues[[1]], decimal_places = 4),
                         covid = 1.3,
                         int = 0,
                         legend = NA,
                         coef = ctrl_T1$test$coefficients[[1]], 
                         group = paste0("COVID effect ", starred_p(ctrl_T1$test$pvalues[[1]], 3, ctrl_T1$test$coefficients[[1]])))
  results4 <- data.frame(coef_name = coef_name,
                         estimate = (ctrl_T1_est + int_T1_est)/2,
                         se = NA,
                         lowerCI = min(ctrl_T1_est, int_T1_est),
                         upperCI = max(ctrl_T1_est, int_T1_est),
                         p_value = round_format(int_$test$pvalues[[1]], decimal_places = 4),
                         covid = 1.3,
                         int = 1,
                         legend = NA,
                         coef = int_$test$coefficients[[1]], 
                         group = paste0("Intervention effect ", starred_p(int_$test$pvalues[[1]], 3, int_$test$coefficients[[1]])))
  results5 <- data.frame(coef_name = coef_name,
                         estimate = int_T1_est,
                         se = int_T1_se,
                         lowerCI = int_T1_est + qnorm(alpha/2) * int_T1_se,
                         upperCI = int_T1_est + qnorm(1-(alpha/2)) * int_T1_se,
                         p_value = round_format(int_T1$test$pvalues[[1]], decimal_places = 4),
                         covid = 1,
                         int = 1,
                         legend = "Follow-up",
                         coef = NA, 
                         group = "Int T1")
  results6 <- data.frame(coef_name = coef_name,
                         estimate = NA,
                         se = NA,
                         lowerCI = NA,
                         upperCI = NA,
                         p_value = NA,
                         covid = 0.5,
                         int = 0,
                         legend = NA,
                         coef = NA,
                         group = "COVID 4th wave")
  results <- rbind(results0, results1, results2, results3, results4, results5, results6)
  return(results)
}
    
results <- data.frame()
for (var in allVars){
  print(var)
  model <- lmer(get(var)~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched)
  results <- rbind(results, gen_coef(model, var, CI = 0.83))
}
results$group <- as.factor(results$group)

library(ggpubr)
library(pBrackets)
library(ggrepel) # geom_text_repel
library(ungeviz) # stat_confidence_density

results$direction <- ifelse(results$coef < 0, "first", "last") # first = down, last = up
results$angle <- ifelse(abs(results$coef) < 0.01, 40, 40)
results$length <- ifelse(abs(results$coef) < 0.01, 0.3, 0.3)
vars_reverse <- c("use_health_service_8", "amic", "amic_sum", "pase_c_1", "pase_c_1_2", "matrix_diet_dh3", "matrix_diet_dh4")
results$improve <- ifelse(results$coef_name %in% vars_reverse & results$coef < 0, 1, 
                          ifelse(results$coef_name %in% vars_reverse & results$coef >= 0, 0, 
                                 ifelse(results$coef_name %!in% vars_reverse & results$coef < 0, 0, 
                                        ifelse(results$coef_name %!in% vars_reverse & results$coef >= 0, 1, NA))))

red <- "#db6d63"
green <- "#4f9e78"
blue <- "#98a2da"
yellow <- "#ffe873"

setwd(sprintf("~%s/ehealth/slides", setpath))
pdf("plot_effects.pdf", height = 28/2.54/1.6, width = 50/2.54/1.6)
for (var in allVars){
  # var <- allVars[3]
  
  plot <- results[results$coef_name == var,] %>%
    ggplot(aes(x = as.factor(covid), y = estimate, group = int)) +
    labs(title=var_names[which(var_names==var), 2],
         x = "", y = var_names[which(var_names==var), 2], 
         caption = "* p<0.05; ** p<0.01; *** p<0.001",
         color = "") +
    expand_limits(x = factor(c(seq(0, 1.5, by = 0.5)))) +
    scale_x_discrete(labels = c("Before", "COVID-19 4th wave", "After", "", ""), breaks = c(0, 0, 1, 0, 0)) +
    geom_linerange(data = results[results$coef_name == var & results$covid != 1.3 & results$legend %in% c("Baseline/Control", "Follow-up"),], aes(ymin = lowerCI, ymax = upperCI, 
                                                                                                                                                  color = legend), size = 2.7, alpha = 0.9,
                   position = position_dodge(width = 0.4),
                   show.legend = TRUE)  +
    scale_color_manual(values=c("Baseline/Control" = blue, "Follow-up" = yellow)) +
    geom_text_repel(data = results[results$coef_name == var & results$covid == 1.3,], aes(label=group), hjust = -1, vjust = 0, 
                    position = position_dodge(width = 0.4),
                    color = c(red, green)[results$improve[results$coef_name == var
                                                          & results$covid == 1.3]+1],
                    segment.color = 'transparent', family = 'sans', size = 4.2
    ) +
    geom_point(data = results[results$coef_name == var & results$covid != 1.3,], size=5, position = position_dodge(width = 0.4), shape = 45) +
    # geom_errorbar(data = results[results$coef_name == var & results$covid == 1.3,], aes(ymin = lowerCI, ymax = upperCI), width = 0.05,
    #               position = position_dodge(width = 0.2))  +
    annotate("segment", x = 3.9, xend = 3.9, y = results$lowerCI[results$coef_name == var
                                                                 & results$covid == 1.3 & results$int == 0], yend = results$upperCI[results$coef_name == var
                                                                                                                                    & results$covid == 1.3 & results$int == 0],
             color = c(red, green)[results$improve[results$coef_name == var
                                                   & results$covid == 1.3 & results$int == 0]+1], 
             arrow = arrow(type = 'closed', ends = results$direction[results$coef_name == var & results$covid == 1.3 & results$int == 0],
                           angle =  results$angle[results$coef_name == var & results$covid == 1.3 & results$int == 0],
                           length = unit(results$length[results$coef_name == var & results$covid == 1.3 & results$int == 0],"cm"))) +
    annotate("segment", x = 4, xend = 4, y = results$lowerCI[results$coef_name == var
                                                                 & results$covid == 1.3 & results$int == 1], yend = results$upperCI[results$coef_name == var
                                                                                                                                    & results$covid == 1.3 & results$int == 1],
             color = c(red, green)[results$improve[results$coef_name == var
                                                   & results$covid == 1.3 & results$int == 1]+1], 
             arrow = arrow(type = 'closed', ends = results$direction[results$coef_name == var & results$covid == 1.3 & results$int == 1],
                           angle =  results$angle[results$coef_name == var & results$covid == 1.3 & results$int == 1],
                           length = unit(results$length[results$coef_name == var & results$covid == 1.3 & results$int == 1],"cm"))) +
    # stat_confidence_density(aes(moe = se, group = group), confidence = 0.83, fill = "#81A7D6") +
    geom_rect(inherit.aes=FALSE, aes(xmin=1.4, xmax=2.7,
                                     ymin=-Inf, ymax=Inf), color = "transparent", fill="#ebebeb", alpha=0.5, ) +
    geom_text(inherit.aes=FALSE, aes(x = 2.05,
                                     y = (min(estimate, na.rm = TRUE) +
                                            max(estimate, na.rm = TRUE))/2, label="COVID-19 4th wave"),
              family="sans", 
              # fontface= "italic", 
              color = "#888888", size = 5.3) +
    theme(text = element_text(size=rel(5)),
          strip.text.x = element_text(size=rel(3*0.85)),
          strip.text.y = element_text(size=rel(3*0.85))) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5)
                            # , legend.position="none"
    )   + 
    theme(legend.position = c(1, 0.9),legend.justification = c(2, 0))
  
  print(plot)
}
dev.off()


results_test <- data.frame()
var <- "eq5d_pain_discomfort"
# var <- "satisfaction_1"
# var <- "pase_c"
# var <- "matrix_diet_dh4"
model <- lmer(get(var)~ 1+covid+int+ (1| subclass) , REML = TRUE, data = df_matched)
results_test <- rbind(results_test, gen_coef(model, var, CI = 0.83))

plot_test <- function(results){
  results$direction <- ifelse(results$coef < 0, "first", "last") # first = down, last = up
  results$angle <- ifelse(abs(results$coef) < 0.01, 40, 40)
  results$length <- ifelse(abs(results$coef) < 0.01, 0.3, 0.3)
  # results$int_colour <- ifelse(results$int == 1, "#ffe873", "#98a2da")
  results$improve <- ifelse(results$coef_name %in% vars_reverse & results$coef < 0, 1, 
                            ifelse(results$coef_name %in% vars_reverse & results$coef >= 0, 0, 
                                   ifelse(results$coef_name %!in% vars_reverse & results$coef < 0, 0, 
                                          ifelse(results$coef_name %!in% vars_reverse & results$coef >= 0, 1, NA))))
  
  red <- "#db6d63"
  green <- "#4f9e78"
  blue <- "#98a2da"
  yellow <- "#ffe873"
  
  plot <- results[results$coef_name == var,] %>%
    ggplot(aes(x = as.factor(covid), y = estimate, group = int)) +
    labs(title=var_names[which(var_names==var), 2],
         x = "", y = var_names[which(var_names==var), 2], 
         caption = "* p<0.05; ** p<0.01; *** p<0.001",
         color = "") +
    expand_limits(x = factor(c(seq(0, 1.5, by = 0.5)))) +
    scale_x_discrete(labels = c("Before", "COVID-19 4th wave", "After", "", ""), breaks = c(0, 0, 1, 0, 0)) +
    geom_linerange(data = results[results$coef_name == var & results$covid != 1.3 & results$legend %in% c("Baseline/Control", "Follow-up"),], aes(ymin = lowerCI, ymax = upperCI, 
                                                                                         color = legend), size = 2.7, alpha = 0.9,
                   position = position_dodge(width = 0.4),
                   show.legend = TRUE)  +
    scale_color_manual(values=c("Baseline/Control" = blue, "Follow-up" = yellow)) +
    geom_text_repel(data = results[results$coef_name == var & results$covid == 1.3,], aes(label=group), hjust = -1, vjust = 0, 
                    position = position_dodge(width = 0.3),
                    color = c(red, green)[results$improve[results$coef_name == var
                                                              & results$covid == 1.3]+1],
                    segment.color = 'transparent', family = 'sans', size = 4.2
                    ) +
    geom_point(data = results[results$coef_name == var & results$covid != 1.3,], size=5, position = position_dodge(width = 0.4), shape = 45) +
    # geom_errorbar(data = results[results$coef_name == var & results$covid == 1.3,], aes(ymin = lowerCI, ymax = upperCI), width = 0.05,
    #               position = position_dodge(width = 0.2))  +
    annotate("segment", x = 3.9, xend = 3.9, y = results$lowerCI[results$coef_name == var
                                                                 & results$covid == 1.3 & results$int == 0], yend = results$upperCI[results$coef_name == var
                                                                                                                                    & results$covid == 1.3 & results$int == 0],
             color = c(red, green)[results$improve[results$coef_name == var
                                                       & results$covid == 1.3 & results$int == 0]+1], 
             arrow = arrow(type = 'closed', ends = results$direction[results$coef_name == var & results$covid == 1.3 & results$int == 0],
                           angle =  results$angle[results$coef_name == var & results$covid == 1.3 & results$int == 0],
                           length = unit(results$length[results$coef_name == var & results$covid == 1.3 & results$int == 0],"cm"))) +
    annotate("segment", x = 5.2, xend = 5.2, y = results$lowerCI[results$coef_name == var
                                                                 & results$covid == 1.3 & results$int == 1], yend = results$upperCI[results$coef_name == var
                                                                                                                                    & results$covid == 1.3 & results$int == 1],
             color = c(red, green)[results$improve[results$coef_name == var
                                                       & results$covid == 1.3 & results$int == 1]+1], 
             arrow = arrow(type = 'closed', ends = results$direction[results$coef_name == var & results$covid == 1.3 & results$int == 1],
                           angle =  results$angle[results$coef_name == var & results$covid == 1.3 & results$int == 1],
                           length = unit(results$length[results$coef_name == var & results$covid == 1.3 & results$int == 1],"cm"))) +
    # stat_confidence_density(aes(moe = se, group = group), confidence = 0.83, fill = "#81A7D6") +
    geom_rect(inherit.aes=FALSE, aes(xmin=1.4, xmax=2.7,
                                     ymin=-Inf, ymax=Inf), color = "transparent", fill="#ebebeb", alpha=0.5, ) +
    geom_text(inherit.aes=FALSE, aes(x = 2.05,
                                     y = (min(estimate, na.rm = TRUE) +
                                            max(estimate, na.rm = TRUE))/2, label="COVID-19 4th wave"),
              family="sans", 
              fontface= "italic",
              color = "#888888", size = 5.3) +
    theme(text = element_text(size=rel(5)),
          strip.text.x = element_text(size=rel(3*0.85)),
          strip.text.y = element_text(size=rel(3*0.85))) +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5)
                            # , legend.position="none"
                            )   + 
    theme(legend.position = c(1, 0.9),legend.justification = c(2, 0))
  
  return(plot) 
}
plot_test(results_test)


# visualise pre-post changes
library(ggpubr)
temp_matched$time <- as.numeric(temp_matched$time)
ggline(temp_matched, x = "time", y = "amic_sum", 
       add = "mean_ci", add.params = list(ci = 0.834), ylim = c(NA, NA), 
       color = "darkblue", linetype = "blank") + 
  stat_compare_means(method = "t.test", paired = TRUE, label.x = 1.5, label.y = 3) 
ggboxplot(temp_matched, x = "time", y = "amic_sum", color = "time") + 
  stat_compare_means(method = "t.test", paired = TRUE)

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
temp <- df
temp$time <- as.factor(temp$time)
lab_bl_ctrl <- "Baseline"
# restrict to those with both T0 & T1
# lab_bl_ctrl <- "Baseline/Control"
# temp <- temp[as.Date(temp$ehealth_eval_timestamp) >= as.Date('2021-04-07') & temp$time == 1 |
#             (as.Date(temp$ehealth_eval_timestamp) <= as.Date('2020-11-20') |
#                as.Date(temp$ehealth_eval_timestamp) >= as.Date('2021-04-07'))  & temp$time == 0 , ]
# 
# 
# temp1 <- temp %>% add_count(member_id) %>% filter(n == 2)
# temp2 <- temp %>% add_count(member_id) %>% filter(as.Date(ehealth_eval_timestamp) >= as.Date('2021-04-07') & n == 1 &
#                                                     member_id %!in% unique(temp1$member_id) & time == 0)
# temp <- rbind(temp1, temp2)
# rm(temp1, temp2)

setwd(sprintf("~%s/ehealth", setpath))
covid <- readRDS("covid_data_hk.rds")

plot_ehealth <-
  ggplot(temp, aes(x=as.Date(ehealth_eval_timestamp), fill = time)) + labs(fill="") +
  scale_fill_manual(
    labels = c(lab_bl_ctrl, "Follow-up"), values = c("#4456bb", "#ffd500")
  ) +
  labs(title = "eHealth data collection of baseline & 6 months follow-up assessments",
       x = "", y = "") +
  geom_histogram(bins = floor(max(temp$ehealth_eval_timestamp)-min(temp$ehealth_eval_timestamp)), alpha=0.55,position="identity") +
  # geom_jitter(height = 0.25) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels="%b %Y") +
  theme_minimal() + theme(text = element_text(size=rel(5))) +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1), legend.text = element_text(size=rel(4)), plot.title = element_text(hjust = 0.5, size=rel(4))) +
  coord_cartesian(xlim = c(as.Date("2020-08-02"), NA)) 

plot_covid <-
  ggplot(covid, aes(x=as.Date(date), y = new_confirmed)) +
  labs(title = "COVID daily cases",
       x = "", y = "", color = "", fill = "") +
  geom_bar(aes(color = "Confirmed cases", fill = "Confirmed cases"), stat='identity', alpha = 0.2) +
  geom_bar(aes(x=as.Date(date), y = new_deaths, color = "Deaths", fill =  "Deaths"), stat='identity') +
  scale_color_manual(values = c("Confirmed cases" = "#BBBBBB", "Deaths" = "#888888")) +
  scale_fill_manual(values = c("Confirmed cases" = "#CCCCCC", "Deaths" = "#999999")) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels="%b %Y") +
  theme_minimal() + theme(text = element_text(size=rel(5))) +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1), legend.text = element_text(size=rel(4)), plot.title = element_text(hjust = 0.5, size=rel(4))) +
  coord_cartesian(xlim = c(as.Date("2020-08-02"), NA)) 

library(patchwork)

Sys.setlocale(locale =  "eng")
setwd(sprintf("~%s/ehealth/slides", setpath))
ggsave("T0 vs T1.png", plot = plot_ehealth/plot_covid, height =  42, width =  60, units = "cm", dpi = 300)
# ggsave("T0 vs T1 (paired).png", plot = plot_ehealth/plot_covid, height =  42, width =  60, units = "cm", dpi = 300)