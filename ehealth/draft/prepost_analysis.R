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

lo <- 1:15  # lower tertile (~28% telephone, ~26% WBS)
mi <- 16:19 # middle tertile (~39% telephone, ~39%, WBS)
hi <- 20:35 # upper tertile (~33% telephone, ~35% WBS)

cutoff_date <- as.Date('2022-07-31')
df <- df[as.Date(df$ehealth_eval_timestamp) <= cutoff_date,]
wbs <- wbs[as.Date(wbs$wbs_survey_date) <= cutoff_date,]

df %>% dplyr::select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
df %>% dplyr::select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
df %>% dplyr::select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
df %>% dplyr::select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
df %>% dplyr::select(starts_with("diet_")) %>% colnames(.) -> diet_
df %>% dplyr::select(starts_with("eq5d")) %>% colnames(.) -> eq5d_

df$pase_c_11_1 <- ifelse(df$pase_c_11 == 0 , 0, df$pase_c_11_1)
df$pase_c_11 <- ifelse(df$pase_c_11_1 == 0 , 0, df$pase_c_11)

df$yearmonth <- zoo::as.yearmon(df$ehealth_eval_timestamp)
df$yearmonth <- match(df$yearmonth, unique(df$yearmonth)[order(unique(df$yearmonth))])
df$yearmonth2 <- floor((df$yearmonth+2)/3) # every 3 months
# df$surveydate <- as.Date(df$ehealth_eval_timestamp)
# df$surveydate <- match(df$surveydate, unique(df$surveydate)[order(unique(df$surveydate))]) # this skips some empty days
df$surveydays <- as.Date(df$ehealth_eval_timestamp) - as.Date('2020-08-03') # days since first recorded survey date
# df$surveyquarters <- round((as.Date(df$ehealth_eval_timestamp) - as.Date('2020-08-03'))/60, 0) # quarters since first recorded survey date

gen_table <- function(df, vars, ordinalVars = NULL, nominalVars = NULL, show_levels = TRUE, medianVars = NULL, paired = TRUE, group = "time", id = "member_id", to_English = FALSE, decimal = 3, p_decimal = 3){ 
  table <- data.frame(matrix(ncol = 7,  nrow = 0))
  row_count <- 1
  col_bl <- 3
  col_bl2 <- 4 # matched baseline 
  col_f1 <- 5 # matched baseline 
  col_dif <- 6 # difference
  col_pval <- 7 # p-value
  
  pre <- unique(df[[group]])[order(unique(df[[group]]))][1]
  post <- unique(df[[group]])[order(unique(df[[group]]))][2]
  
  df2 <- df %>% add_count(get(id), name = "n") %>% filter(n == 2) # keep only those with both T0 & T1
  
  dfwide <- reshape(data=df, idvar= c(id),
                    timevar = group,
                    direction="wide")
  
  dfwide2 <- reshape(data=df2, idvar= c(id),
                     timevar = group,
                     direction="wide")
  
  if (to_English){
    df_en <- to_English(to_character_df(df, ordinalVars))
    df2_en <- to_English(to_character_df(df2, ordinalVars))
  } else {
    df_en <- df
    df2_en <- df2
  }
  
  colnames(table)[1] <- ""
  colnames(table)[2] <- ""
  colnames(table)[col_bl] <- "Baseline (all)"
  colnames(table)[col_bl2] <- "Pre-test (paired)"
  colnames(table)[col_f1] <- "Post-test (paired)"
  colnames(table)[col_dif] <- "Difference"
  colnames(table)[col_pval] <- "p-value"
  
  table[row_count, 1] <- "N"
  table[row_count, 2] <- ""
  table[row_count, col_bl] <-  nrow(df[df[[group]] %in% pre,])
  table[row_count, col_bl2] <- nrow(df[df[[group]] %in% post,])
  table[row_count, col_f1] <-  nrow(df[df[[group]] %in% post,])
  
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
    if (var %in% ordinalVars | var %in% nominalVars){
      table[row_count, 1] <- var
      
      if(var %in% ordinalVars){
        wilcox_test <-  
          wilcox.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
        if (wilcox_test$p.value < 0.001 & p_decimal == decimal){
          table[row_count, col_pval] <- "<0.001"
        } else {
          table[row_count, col_pval] <- wilcox_test$p.value %>% round(digits = p_decimal)
        }
      } else if (var %in% nominalVars){
        levels <- c(unique(unique(dfwide2[[paste0(var, ".", pre)]]), unique(dfwide2[[paste0(var, ".", post)]])))
        mcnemar_test <-  
          mcnemar.test(table(factor(dfwide2[[paste0(var, ".", pre)]], levels = levels), factor(dfwide2[[paste0(var, ".", post)]], levels = levels))) 
        if (mcnemar_test$p.value %!in% NaN && mcnemar_test$p.value < 0.001 & p_decimal == decimal){
          table[row_count, col_pval] <- "<0.001"
        } else {
          table[row_count, col_pval] <- mcnemar_test$p.value %>% round(digits = p_decimal)
        }
      }
      
      if (show_levels){
        for (val in unique(df_en[[var]])[order(unique(df_en[[var]]))]){
          
          
          table[row_count, 2] <- val
          
          if (val %in% NA){
            table[row_count, 2] <- "N/A"
          }
          
          table[row_count, col_bl] <- 
            table(df_en[[var]],useNA = "ifany") %>% 
            prop.table() %>% as.data.frame() %>% 
            .[which(.$Var1 %in% val),"Freq"] 
          
          if (val %in% unique(df2_en[[var]][which(df2[[group]] == pre)])){
            table[row_count, col_bl2] <- iferror(
              table(df2_en[[var]][which(df2[[group]] == pre)],useNA = "ifany") %>%
                prop.table() %>% as.data.frame() %>%
                .[which(.$Var1 %in% val),"Freq"], NA)
          } else {
            table[row_count, col_bl2]  <- 0
          }
          
          if (val %in% unique(df2_en[[var]][which(df2[[group]] == post)])){
            table[row_count, col_f1] <-
              table(df2_en[[var]][which(df2[[group]] == post)],useNA = "ifany") %>%
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
        
        table[row_count, 2] <- "mean (sd)"
        table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df[[group]] == pre)])
        table[row_count, col_bl2] <- get_mean_sd(df2[[var]][which(df2[[group]] == pre)])
        table[row_count, col_f1] <-  get_mean_sd(df2[[var]][which(df2[[group]] == post)])
        
        table[row_count, col_dif] <-  (mean(df2[[var]][which(df2[[group]] == post)], na.rm = TRUE) - mean(df2[[var]][which(df2[[group]] == pre)], na.rm = TRUE)) %>% round(digits = decimal-1)
        
      }
      
      row_count <- row_count + 1
      
      
    } else {
      table[row_count, 1] <- var
      
      table[row_count, 2] <- "mean (sd)"
      table[row_count, col_bl] <-  get_mean_sd(df[[var]][which(df[[group]] == pre)])
      table[row_count, col_bl2] <- get_mean_sd(df2[[var]][which(df2[[group]] == pre)])
      table[row_count, col_f1] <-  get_mean_sd(df2[[var]][which(df2[[group]] == post)])
      
      t_test <-  
        t.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
      table[row_count, col_dif] <-  (mean(df2[[var]][which(df2[[group]] == post)], na.rm = TRUE) - mean(df2[[var]][which(df2[[group]] == pre)], na.rm = TRUE)) %>% round(digits = decimal-1)
      if (t_test$p.value < 0.001 & p_decimal == decimal){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- t_test$p.value %>% round(digits = p_decimal)
      }
      
      row_count <- row_count + 1
    }
    
    if (var %in% medianVars){
      table[row_count, 2] <- "median (Q1-Q3)"
      table[row_count, col_bl] <-  get_median_iqr(df[[var]])
      table[row_count, col_bl2] <- get_median_iqr(df2[[var]][which(df2[[group]] == pre)])
      table[row_count, col_f1] <-  get_median_iqr(df2[[var]][which(df2[[group]] == post)])
      
      table[row_count, col_dif] <- (median(df2[[var]][which(df2[[group]] == post)], na.rm = TRUE) - median(df2[[var]][which(df2[[group]] == pre)], na.rm = TRUE)) 
      
      wilcox_test <-  
        wilcox.test(dfwide2[[paste0(var, ".", pre)]], dfwide2[[paste0(var, ".", post)]], paired = paired) 
      if (wilcox_test$p.value < 0.001 & p_decimal == decimal){
        table[row_count, col_pval] <- "<0.001"
      } else {
        table[row_count, col_pval] <- wilcox_test$p.value %>% round(digits = p_decimal)
      }
      
      row_count <- row_count + 1
    }
  }
  return(table)
}

# total score of diet items ----
scoring <- function(df){
  reverse_matrix_diet <- matrix_diet_[c(1, 2)] # high-sugar/high-fat snacks & processed/canned foods
  df[reverse_matrix_diet] <- (df[reverse_matrix_diet]-4)*-1 # reverse (0:4) to (4:0)
  
  diet_ <- c(matrix_diet_, diet_)
  df <- df %>% 
    mutate(diet_sum = rowSums(.[diet_], na.rm = FALSE) 
    )
  
  
  return(subset(df, select = c(diet_sum
  )))
}

df <- cbind(df, scoring(df))

# merge with WBS data----
wbsvars <- c("Survey_centre", "wbs_survey_date", "gender", "dob",
             "Phase1_member_self_report", "DH_centre_member", "Carer", "Hypertension", "Hypertension_HA",
             "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", "Heart_score", "Stroke", "Copd", "Renal", 
             "Disease_other", "Disease_other_indicate", "marital", "educ", "Income_oaa", "Income_oala", 
             "Income_ssa", "Income_work", "Income_saving", "Income_cssa", "Income_cssa_score", "Income_pension",
             "Income_child", "Income_other", "living_status", "housing_type", "Rent", "Own", "FS1", "FS2", "FS3",
             "FS4", "FS5", "FS_total", "FS_score", "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC",
             "AMIC_score", "Self_rated_health", "Self_rated_health_score", "Satisfaction", "Satisfaction_score",
             "Meaning_of_life", "Happiness", "Happiness_score", "Incontinence", "Hospital", "Hospital_day", 
             "Hospital_score", "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
             "Drug_use", "Drug_use_score", "risk_score", "risk_level", "digital", "Centre", "NGO")

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
") # (T0 = baseline, T1 = 6mth, T2 = 12mth, T3 = = 18mth)
val_labels(df$time) <- NULL

dfwide <-  reshape(data=df, idvar=  "member_id",
                   sep = ".t", 
                   timevar = "time",
                   direction="wide")

wbswide <-  reshape(data=wbs, idvar=  "member_id",
                    sep = ".r", 
                    timevar = "Round",
                    direction="wide")

df <- merge(df, wbswide[, c("member_id", paste0(wbsvars, ".r1"), paste0(wbsvars, ".r2"))], # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

df$age.r1 <- floor(interval(df$dob.r1, df$ehealth_eval_timestamp) / duration(num = 1, units = "years")) # https://stackoverflow.com/a/27363833

df$age_group.r1 <- recode_age(df$age.r1, age_labels = c("50-59", "60-69", "70-79", "80+"))
df$age_group.r1 <- relevel(as.factor(df$age_group.r1), ref = "60-69")

dfwide <- merge(dfwide, wbswide[, c("member_id", paste0(wbsvars, ".r1"), paste0(wbsvars, ".r2"))], # extract item matched by member ID
                by=c("member_id"), all.x = TRUE)

dfwide$age.r1 <- floor(interval(dfwide$dob.r1, dfwide$ehealth_eval_timestamp.t0) / duration(num = 1, units = "years")) # https://stackoverflow.com/a/27363833

dfwide$age_group.r1 <- recode_age(dfwide$age.r1, age_labels = c("60-69", "70-79", "80+"))
dfwide$age_group.r1 <- relevel(as.factor(dfwide$age_group.r1), ref = "60-69")

# create baseline risk score (not necessarily from first WBS round) ----
dfwide$risk_score.bl <- ifelse(dfwide$ehealth_eval_timestamp.t0 %in% NA, dfwide$risk_score.r1,
                               ifelse(dfwide$risk_score.r1 >= 14, dfwide$risk_score.r1, 
                                      ifelse(dfwide$ehealth_eval_timestamp.t0 <= dfwide$wbs_survey_date.r2, dfwide$risk_score.r1,
                                             ifelse(dfwide$ehealth_eval_timestamp.t0 > dfwide$wbs_survey_date.r2, dfwide$risk_score.r2, NA))))

df <- merge(df, dfwide[, c("member_id", "risk_score.bl")], # extract item matched by member ID
                by=c("member_id"), all.x = TRUE)

# dfwide[dfwide$risk_score.bl %in% NA, ] %>% # check for any NAs
#   select(member_id, ehealth_eval_timestamp.t0,  
#          wbs_survey_date.r1, wbs_survey_date.r2, risk_score.r1, risk_score.r2) %>% View

# set variable names ----
var_names <- t(array(c(c("use_health_service_8", "Out-of-pocket payments (lower=better)"),  
                       c("amic", "Memory symptoms, proportion of AMIC >= 3 (lower=better)"),  
                       c("amic_sum", "Memory symptoms score (AMIC, lower=better)"), 
                       c("self_efficacy", "Self-efficacy - Total"), 
                       c("self_efficacy_1", "Self-efficacy - Physical health"),
                       c("self_efficacy_2", "Self-efficacy - Mental health"),
                       c("self_efficacy_3", "Self-efficacy - Social support"),
                       c("self_efficacy_4", "Self-efficacy - Healthy diet"),
                       c("self_efficacy_5", "Self-efficacy - Exercise"),
                       c("eq5d", "Health-related quality of life (EQ-5D-5L)"),
                       c("eq5d_mobility", "Health-related quality of life - Mobility"),
                       c("eq5d_self_care", "Health-related quality of life - Self care"),
                       c("eq5d_usual_activity", "Health-related quality of life - Usual activity"),
                       c("eq5d_pain_discomfort", "Health-related quality of life - Pain/discomfort (higher=better)"),
                       c("eq5d_anxiety_depression", "Health-related quality of life - Anxiety/depression (higher=better)"),
                       c("eq5d_health", "Self-rated health (EQ VAS)"),
                       c("satisfaction_1", "Satisfaction with all health care professionals"),  
                       c("satisfaction_2", "Satisfaction with eHealth health care and centre staff"),  
                       c("pase_c", "Physical activity score	(PASE-C)"),  
                       c("pase_c_1", "Sitting per week"),  
                       c("pase_c_1_2", "Sitting per day"),  
                       c("pase_c_11", "Proportion of walking as exercise daily"),  
                       c("pase_c_11_1", "Number of streets walked as exercise"),  
                       c("pase_c_12_1", "Proportion of walking in general daily"),  
                       c("pase_c_12", "Number of streets walked in general"),  
                       c("matrix_diet_dh3", "Amount of high-sugar/high-fat snacks"),  
                       c("matrix_diet_dh4", "Amount of processed/canned foods"),  
                       c("matrix_diet_dh7", "Three main meals daily"),  
                       c("matrix_diet_dh8", "Three meals at regular hours"),  
                       c("diet_dp1", "Amount of grains in a meal"),  
                       c("diet_dp3", "Amount of vegetables per day"),  
                       c("diet_dp4", "Amount of fruit per day"),  
                       c("diet_dp5", "Amount of meat/poultry/fish/egg per day"), 
                       c("diet_sum", "Diet score")), dim = c(2,34)))

# restrict df sample to age >= 60 ----
# df <- df[(df$ehealth_eval_timestamp) <= ('2021-06-28 10:00:00 HKT'),]
df <- df[which(df$age.r1 >= 60),]
dfwide <- dfwide[which(dfwide$age.r1 >= 60),]
wbs <- wbs[which(wbs$Age >= 60),]
wbswide <- wbswide[which(wbswide$Age.r1 >= 60),]

# withdrawal statistics ----
setwd(sprintf("~%s/ehealth/wbs", setpath))
withdrawals <- xlsx::read.xlsx2("WithdrawalList_2022-08-13.xlsx", sheetName  = "Sheet 1")

names(withdrawals)[names(withdrawals)=="X3..會員編號"] <- "member_id"
names(withdrawals)[names(withdrawals)=="X4..退出原因"] <- "withdrawal_reason"
names(withdrawals)[names(withdrawals)=="X4.1.請注明退出原因"] <- "withdrawal_reason_"
names(withdrawals)[names(withdrawals)=="X5..退出日期"] <- "withdrawal_date"
withdrawals <-  merge(withdrawals, wbs[, c("member_id", "Age", "risk_level")], # extract item matched by member ID
                      by=c("member_id"), all.x = TRUE)

withdrawals$withdrawal_reason <- ifelse(grepl("搬屋", withdrawals$withdrawal_reason_), "搬遷|搬離坪洲", withdrawals$withdrawal_reason)
withdrawals$withdrawal_reason <- ifelse(grepl("時間", withdrawals$withdrawal_reason_) &
                                          !grepl("太遠", withdrawals$withdrawal_reason_), "沒有時間", withdrawals$withdrawal_reason)
withdrawals$withdrawal_reason <- ifelse(grepl("健康", withdrawals$withdrawal_reason_), "健康問題", withdrawals$withdrawal_reason)

withdrawals$withdrawal_reason <- car::recode(withdrawals$withdrawal_reason, "
c('其他','') = 'Others';
'已入院舍' = 'Residential care';
'失聯' = 'Loss of contact';
'行動不便' = 'Mobility problems';
'沒有時間' = 'Lack of time';
'沒有需要' = 'Unnecessary';
'私人理由' = 'Personal reasons';
'健康問題' = 'Health problems';
'患認知障礙' = 'Dementia';
c('搬遷', '搬遷|搬離坪洲') = 'Relocation';
'照顧家人' = 'Care for family';
'資料錯誤' = 'Wrong information';
'轉中心' = 'Change of centre';
'離世' = 'Death';
'長期離港' =  'Away from Hong Kong'
")

# nursing mode & freq data ----
setwd(sprintf("~%s/ehealth", setpath))
temp <- xlsx::read.xlsx2("No. of nurse session_As of 20220228.xlsx", sheetIndex  = 1
                         , header = TRUE
)
temp <- convert2NA(temp, "")
names(temp)[names(temp) == "UID"] <- "member_id"
names(temp)[names(temp) == "Start.date"] <- "nursing_start_date"
names(temp)[names(temp) == "Status"] <- "nursing_withdrawn"
names(temp)[names(temp) == "Refused.nursing.support..written.confirmed.by.centers."] <- "nursing_refused"
names(temp)[names(temp) == "No..of.Face.to.face.Session"] <- "f2f_num"
names(temp)[names(temp) == "No..of.Tele.mode.Session"] <- "telemode_num"
names(temp)[names(temp) == "Total"] <- "f2f_telemode_num"
temp$nursing_start_date <- as.Date(as.numeric(temp$nursing_start_date), origin = "1899-12-30")

temp$nursing_withdrawn <- ifelse(temp$nursing_withdrawn %in% "退出", 1, 0)
temp$nursing_refused <- ifelse(temp$nursing_refused %in% "Yes", 1, 0)

df <- merge(df, temp, # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

df$f2f_num <- as.numeric(df$f2f_num)
df$f2f_num <- ifelse(df$f2f_num %in% NA, 0, df$f2f_num)

df$telemode_num <- as.numeric(df$telemode_num)
df$telemode_num <- ifelse(df$telemode_num %in% NA, 0, df$telemode_num)

df$f2f_telemode_num <- as.numeric(df$f2f_telemode_num)
df$f2f_telemode_num <- df$f2f_num + df$telemode_num # seems some values in original total were miscalculated
df$f2f_pct <- df$f2f_num/df$f2f_telemode_num
df$telemode_pct <- df$telemode_num/df$f2f_telemode_num

temp <- df
# temp <- temp %>% add_count(member_id, name = "n") %>% filter((n == 1 & time == 0)| (n == 2 & time == 1) | (n >= 2 & time == 2))

allVars <- c("f2f_num", "telemode_num", 
             "f2f_telemode_num", "telemode_pct", "Exercise", "Cognitive", "Nutritional", "Mental", "Social", "Others", "Total")

# tableone::CreateTableOne(data =  temp,
#                          strata = c("time"),
#                          vars = allVars, test = FALSE) %>%
#   print(showAllLevels = TRUE, nonnormal = NULL) %>% clipr::write_clip()

# COVID waves 3-5 ----
# start defined as new cases >= 15 for 3 days, end defined as new cases < 15 for 3 days (roughly)
df$covid_3rd <- ifelse(df$ehealth_eval_timestamp >= as.Date('2020-07-01') & 
                         df$ehealth_eval_timestamp <= as.Date('2020-09-15'), 1, 0) # https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(21)00039-0/fulltext

df$covid_4th <- ifelse(df$ehealth_eval_timestamp >= as.Date('2020-11-01') & 
                         df$ehealth_eval_timestamp <= as.Date('2021-03-31'), 1, 0) # https://www.sciencedirect.com/science/article/pii/S2666606521001905

df$covid_5th <- ifelse(df$ehealth_eval_timestamp >= as.Date('2021-12-31') # https://www.tandfonline.com/doi/full/10.1080/22221751.2022.2060137
                       # & df$ehealth_eval_timestamp <= as.Date('2022-04-20') # gov social distancing measures easing from 21st April 2022
                       , 1, 0)

df$covid <- ifelse(df$covid_3rd %in% 1 | df$covid_4th %in% 1 | df$covid_5th %in% 1, 1, 0)

# start defined as new cases >= 15 for 3 days, end defined as new cases < 15 for 3 days (roughly)
wbs$covid_3rd <- ifelse(wbs$wbs_survey_date >= as.Date('2020-07-01') & 
                          wbs$wbs_survey_date <= as.Date('2020-09-15'), 1, 0) # https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(21)00039-0/fulltext

wbs$covid_4th <- ifelse(wbs$wbs_survey_date >= as.Date('2020-11-01') & 
                          wbs$wbs_survey_date <= as.Date('2021-03-31'), 1, 0) # https://www.sciencedirect.com/science/article/pii/S2666606521001905

wbs$covid_5th <- ifelse(wbs$wbs_survey_date >= as.Date('2021-12-31') # https://www.tandfonline.com/doi/full/10.1080/22221751.2022.2060137
                        # & wbs$wbs_survey_date <= as.Date('2022-04-20')
                        , 1, 0)

wbs$covid <- ifelse(wbs$covid_3rd %in% 1 | wbs$covid_4th %in% 1 | wbs$covid_5th %in% 1, 1, 0)


# descriptive statistics ----
allVars <- c("gender.r1", "age.r1", "age_group.r1", "educ.r1", "marital.r1", "living_status.r1", "housing_type.r1"
             # , "risk_score.bl"
             )
catVars <- c("gender.r1", "age_group.r1", "educ.r1", "marital.r1", "living_status.r1", "housing_type.r1")

# gen_desc(df, vars = allVars, nominalVars = catVars, medianVars = NULL
#          # , group = "HLCat3"
# ) %>% clipr::write_clip()

# telephone survey
df_temp <- df %>% filter(time %in% c(0,1)) %>% add_count(member_id) %>% filter(n == 2 & time == 0) # only T0 of those with both T0 & T1

tableone::CreateTableOne(data = df_temp[df_temp$time %in% c(0,1),], 
                         # strata = c("time"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

# WBS survey
wbs_temp <- wbs
wbs_temp <- wbs_temp[wbs_temp$member_id %in% wbs_temp$member_id[wbs_temp$Round == 1 & wbs_temp$risk_level == 3], ]
wbs_temp <- wbs_temp[wbs_temp$member_id %in% df$member_id, ]
# wbs_temp <- wbs_temp[(wbs_temp$member_id %in% df$member_id & wbs_temp$Round == 2) | wbs_temp$Round == 1, ]

wbs_temp <- merge(wbs_temp, dfwide[, c("member_id", "risk_score.r1")], # extract item matched by member ID
              by=c("member_id"), all.x = TRUE)
wbs_temp <- wbs_temp %>% filter(Round %in% c(1,2)) %>% add_count(member_id) %>% filter(n == 2 & Round == 1) 

wbs_temp$age_group <- recode_age(wbs_temp$Age, age_labels = c("50-59", "60-69", "70-79", "80+"))

allVars <- c("gender", "Age", "age_group", "educ", "marital", "living_status", "housing_type"
             # , "risk_score.bl"
)
catVars <- c("gender", "age_group", "educ", "marital", "living_status", "housing_type")


tableone::CreateTableOne(data = wbs_temp, 
                         strata = c("Round"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()


df_temp$survey <- "telephone"
wbs_temp$survey <- "wbs"
allVars_df <- c("gender.r1", "age.r1", "age_group.r1", "educ.r1", "marital.r1", "living_status.r1", "housing_type.r1"
                # , "risk_score.bl"
)
allVars_wbs <- c("gender", "Age", "age_group", "educ", "marital", "living_status", "housing_type"
             # , "risk_score.bl"
)

for (index in 1:length(allVars_df)){
  names(df_temp)[names(df_temp) %in% allVars_df[index]] <- allVars_wbs[index]
}

temp <- rbind(df_temp[,c(allVars_wbs, "survey", "member_id")], wbs_temp[,c(allVars_wbs, "survey", "member_id")])
df_temp[df_temp$member_id %in% wbs_temp$member_id,] %>% nrow
wbs_temp[wbs_temp$member_id %in% df_temp$member_id,] %>% nrow

## restrict to people in both telephone survey and WBS
# temp <- temp[temp$member_id %in% wbs_temp$member_id,]
# temp <- temp[temp$member_id %in% df_temp$member_id,]

tableone::CreateTableOne(data = temp, 
                         strata = c("survey"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

# telephone survey pre-post results ----
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

allVars <- c("amic", "amic_sum", 
             "amic1", "amic2", "amic3", "amic4", "amic5",
             "self_efficacy", "self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5",
             "eq5d", "eq5d_mobility", "eq5d_self_care", "eq5d_usual_activity", "eq5d_pain_discomfort", "eq5d_anxiety_depression", "eq5d_health", 
             "satisfaction_1", "satisfaction_2", 
             "pase_c", "pase_c_1", "pase_c_1_2", "pase_c_11", "pase_c_11_1", "pase_c_12_1", "pase_c_12",
             "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
             "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5", "diet_sum", "use_health_service_8")

ordinalVars <- c("self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5",
                 "eq5d_mobility", "eq5d_self_care", "eq5d_usual_activity", "eq5d_pain_discomfort", "eq5d_anxiety_depression",
                 "pase_c_1", "pase_c_1_2",
                 "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
                 "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5")

nominalVars <- c("amic", "pase_c_11", "pase_c_12_1")

medianVars <- c("use_health_service_8")

# temp <- df[df$time %in% c(0,1) & df$gender == "M" & df$age_group == "60-69", ]
temp <- df[df$time %in% c(1,2)
             # & df$covid %in% 0 # outside covid waves
           # & df$risk_score.bl %in% lo
             , ] 

# # remove withdrawals or sessions < 6 
# temp <- merge(temp, withdrawals[c("member_id", "withdrawal_date")], 
#               by=c("member_id"), all.x = TRUE)
# temp <- temp[(temp$withdrawal_date < temp$ehealth_eval_timestamp & temp$time %in% 1) | temp$withdrawal_date %in% NA | temp$time %in% 0,] # remove withdrawals before T1
# # temp <- temp[temp$member_id %!in% withdrawals$member_id,] # remove any withdrawals
# temp <- temp[(temp$f2f_telemode_num >= 6 & temp$time >= 1) | temp$time == 0,] # remove if nursing sessions < 6

# temp <- temp[temp$interviewer_name %in% c("Ashley Leung", "Eva Mak", "Susan To", "Tang Tsz Chung", "Lucas Li", "Vicky", "Chan Ka Wai, Katherine", "Carman Yeung", "Yan"), ] # restrict to interviewers with longer interview duration
gen_table(temp, to_English = TRUE, vars = allVars[], ordinalVars =  ordinalVars, nominalVars = nominalVars, medianVars = medianVars, show_levels = FALSE, p_decimal = 100) %>% clipr::write_clip()

# wbs variable lists and variable creation ----
vars <- c("Survey_centre", "wbs_survey_date", "gender", "dob",
          "Phase1_member_self_report", "DH_centre_member", "Carer", "Hypertension", "Hypertension_HA",
          "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", # "Heart_score", 
          "Stroke", "Copd", "Renal", "Disease_other", "Disease_other_indicate", "marital", "educ", "Income_oaa", "Income_oala", 
          "Income_ssa", "Income_work", "Income_saving", "Income_cssa", #"Income_cssa_score", 
          "Income_pension", "Income_child", "Income_other", "living_status", "housing_type", "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", "FS_total", # "FS_score", 
          "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC", # "AMIC_score", 
          "Self_rated_health", # "Self_rated_health_score", 
          "Satisfaction", # "Satisfaction_score",
          "Meaning_of_life", "Happiness", # "Happiness_score", 
          "Incontinence", "Hospital", "Hospital_day", # "Hospital_score", 
          "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
          "Drug_use", # "Drug_use_score", 
          "risk_score", "risk_level", "digital", "Centre", "NGO")

allVars <- c("Carer", "Hypertension", "Hypertension_HA",
             "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", # "Heart_score", 
             "Stroke", "Copd", "Renal", "Disease_other", # "married", 
             "educ", "Income_oaa", "Income_oala", 
             "Income_ssa", "Income_work", "Income_saving", "Income_cssa", # "Income_cssa_score", 
             "Income_pension", "Income_child", "Income_other", # "living_status", "housing_type", 
             "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", "FS_total", # "FS_score", 
             "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC", # "AMIC_score", 
             "Self_rated_health", # "Self_rated_health_score", 
             "Satisfaction", # "Satisfaction_score",
             "Meaning_of_life", "Happiness", # "Happiness_score", 
             "Incontinence", "Hospital", "Hospital_day", # "Hospital_score", 
             "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
             "Drug_use", # "Drug_use_score", 
             "risk_score")

wbs$housing_type <- ifelse(wbs$housing_type %in% c(4,5), 3, wbs$housing_type) # recode 4&5 to 3 (1=public rental housing, 2=subsidized-sale housing, 3=private housing, 4=Home for the Aged, 5=others)
wbs$married <- ifelse(wbs$marital %in% 2, 1, 0) # (1= single, 2=married, 3=widowed, 4=divorced/separated)

ordinalVars <- c("educ", "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5",
                 "Self_rated_health", "Satisfaction", 
                 "Meaning_of_life", "Happiness", 
                 "Incontinence", "Drug_use")

allVars <- c(allVars, 
             "prefrail", "sarcopenic", 
             "poor_health", "memory_complaints", 
             "incontinent", "polypharmacy", "low_wellbeing")

nominalVars <- c("marital", "living_status", "housing_type",
                 "Carer", "Hypertension", "Hypertension_HA",
                 "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", 
                 "Stroke", "Copd", "Renal", "Disease_other", # "married", 
                 "Income_ssa", "Income_work", "Income_saving", "Income_cssa", 
                 "Income_pension", "Income_child", "Income_other", 
                 "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", 
                 "Hospital", 
                 "Aeservices", "SOPD", "GOPD", "Clinic", 
                 "prefrail", "sarcopenic", 
                 "poor_health", "memory_complaints", 
                 "incontinent", "polypharmacy", "low_wellbeing"
)

wbs$prefrail <- ifelse(wbs$FS_total %in% c(1:5), 1, 0)
wbs$sarcopenic <- ifelse(wbs$SAR_total %in% c(4:10), 1, 0)
wbs$poor_health <- ifelse(wbs$Self_rated_health %in% 1, 1, 0)
wbs$memory_complaints <- ifelse(wbs$AMIC %in% c(1:2), 1, 0)
wbs$incontinent <- ifelse(wbs$Incontinence %in% c(1:2), 1, 0)
wbs$polypharmacy <- ifelse(wbs$Drug_use %in% 3, 1, 0)
wbs$low_wellbeing <- ifelse(wbs$Satisfaction %in% c(1:3) |
                              wbs$Meaning_of_life %in% c(1:3) |
                              wbs$Happiness %in% c(0:3), 1, 0)


varlist <- t(array(c(
  c("FS_total", "Fraility score (FRAIL)"), 
  c("prefrail", "Frail or pre-frail (FRAIL ≥ 1)"), 
  c("SAR_total", "Sarcopenia score (SARC-F)"), 
  c("sarcopenic", "Sarcopenic (SARC-F ≥ 4)"), 
  c("poor_health", "Poor self-rated health"), 
  c("low_wellbeing", "Low subjective wellbeing"), 
  c("memory_complaints", "Subjective memory complaint"), 
  c("incontinent", "Urinary/fecal incontinence (incl. occasional)"), 
  c("polypharmacy", "Polypharmacy"), 
  c("Hospital", "Hospitalized in past year"), 
  c("Hospital_day", "Number of times hospitalized"), 
  c("Aeservices", "Used A&E services in past year"), 
  c("Aeservices_day", "Number of times used A&E services"), 
  c("GOPD", "Used GOPD in past year"), 
  c("SOPD", "Used SOPD in past year"), 
  c("Clinic", "Used Private clinics/hospitals in past year"), 
  c("Hypertension", "Hypertension"), 
  c("Diabetes", "Diabetes"), 
  c("Cholesterol", "High cholesterol"), 
  c("Heart", "Heart diseases"), 
  c("Stroke", "Stroke"), 
  c("Copd", "Chronic obstructive pulmonary disease (COPD)"), 
  c("Renal", "Renal disease"), 
  c("Disease_other", "Other diseases")
),
dim = c(2,24))) %>% as.data.frame()

# wbs pre-post results ----
temp <- wbs

# high risk at 1st round WBS & did baseline
temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level == 3], ]
# temp <- temp[temp$member_id %in%
#                df$member_id
#                # dfwide$member_id[(as.Date(dfwide$ehealth_eval_timestamp.t0)+84) < dfwide$wbs_survey_date.r2]
#              ,]
temp <- merge(temp, dfwide[, c("member_id", "risk_score.r1")], # extract item matched by member ID
              by=c("member_id"), all.x = TRUE)
# temp <- temp[temp$risk_score.r1 %in% lo,]
# temp <- temp[temp$covid %in% 0, ]

# # low risk at 1st round WBS & not done telephone survey
# temp <- wbs
# temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2022-07-31'),]
# 
# temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level %in% c(1, 2)], ]
# temp <- temp[temp$member_id %!in%
#                df$member_id
#                # dfwide$member_id[as.Date(dfwide$ehealth_eval_timestamp.t0) < dfwide$wbs_survey_date.r2]
#                ,]
# temp <- merge(temp, wbswide[, c("member_id", "risk_score.r1")], # extract item matched by member ID
#               by=c("member_id"), all.x = TRUE)
# # temp <- temp[temp$risk_score.r1 %in% 13,]
# # temp <- temp[temp$covid %in% 0, ]

gen_table(temp, id = "member_id", group = "Round", to_English = FALSE, vars = allVars, 
          ordinalVars = ordinalVars,
          nominalVars =  nominalVars, show_levels = FALSE, p_decimal = 100) %>% clipr::write_clip()


# synthetic control ----
panelView::panelview(eq5d_score ~  time, data = df[1:1000,],  index = c("member_id", "yearmonth"), pre.post = TRUE)

get_plot <- function(df, x, y, xlab = NULL, ylab = NULL, legendtitle = NULL, jitter_w = 0, jitter_h = 0, 
                     yintercept = NULL, xintercept = NULL, text_size = 5.5, group = NULL, legend = TRUE, scatter = FALSE, method = "loess", colour =  "darkgray", showCI = TRUE){
  plot <- ggplot(df, aes_string(x=x, y=y, fill = group, colour = group) ) +
    xlab(xlab) + ylab(ylab) + guides(fill=guide_legend(title=legendtitle), color=guide_legend(title=legendtitle))  +
    theme(text = element_text(size=rel(text_size)),
          legend.text = element_text(size=rel(text_size)),
          strip.text.x = element_text(size=rel(text_size*0.85)),
          strip.text.y = element_text(size=rel(text_size*0.85)))
  if (!is.numeric(df[[x]])) {plot <- plot + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")}
  if (scatter) {plot <- plot + geom_jitter(width = jitter_w, height = jitter_h)}
  if (!legend) {plot <- plot + theme(legend.position="none")}
  if (is.null(group)) {plot <- plot + geom_smooth(method=method, formula = y ~ x, se=showCI, na.rm=TRUE, color = colour, fill = colour)} 
  else {plot <- plot + geom_smooth(method=method, formula = y ~ x, se=showCI, na.rm=TRUE)}
  return(plot)
}

df$time <- as.factor(df$time)
df$ehealth_eval_timestamp <- as.Date(df$ehealth_eval_timestamp)
get_plot(df[df$member_id %in% df$member_id[df$time %in% 2],], x = "ehealth_eval_timestamp", xlab = "Survey date", 
         y = "pase_c", group = "time", legendtitle = "Time point", legend = TRUE, showCI = FALSE, scatter = FALSE) + ggtitle('') &
  theme(axis.text.x = element_text(size=rel(6*1), angle=20, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=rel(6*1))) 

get_plot(df[df$member_id %in% df$member_id[df$time %in% 1] & df$time %in% 0:1,], x = "ehealth_eval_timestamp", xlab = "Survey date", 
         y = "pase_c", group = "time", legendtitle = "Time point", legend = TRUE, showCI = FALSE, scatter = FALSE) + ggtitle('') &
  theme(axis.text.x = element_text(size=rel(6*1), angle=20, vjust = 1, hjust = 1),
        axis.text.y = element_text(size=rel(6*1))) 

# ICC calculation
dfwide$cluster <- substr(dfwide$member_id, 0, 5) # 80 centres

cluster_icc <- function(data, var, cluster){
  summary_aov <- summary(aov(data[[var]] ~ data[[cluster]]))
  print(var)
  return(summary_aov[[1]][1,2]/sum(summary_aov[[1]][,2]))
}
cluster_icc(dfwide, "Meaning_of_life.r1", "cluster")
cluster_icc(dfwide, "Happiness.r1", "cluster")
cluster_icc(dfwide, "Satisfaction.r1", "cluster")
cluster_icc(dfwide, "SAR_total.r1", "cluster")
cluster_icc(dfwide, "FS_total.r1", "cluster")
cluster_icc(dfwide, "FS_total.r1", "cluster")

vars1 <- c("amic", "amic_sum", 
             "amic1", "amic2", "amic3", "amic4", "amic5",
             "self_efficacy", "self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5",
             "eq5d", "eq5d_mobility", "eq5d_self_care", "eq5d_usual_activity", "eq5d_pain_discomfort", "eq5d_anxiety_depression", "eq5d_health", 
             "satisfaction_1", "satisfaction_2", 
             "pase_c", "pase_c_1", "pase_c_1_2", "pase_c_11", "pase_c_11_1", "pase_c_12_1", "pase_c_12",
             "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
             "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5", "diet_sum", "use_health_service_8")
vars2 <- c("Hypertension", "Hypertension_HA",
             "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", # "Heart_score", 
             "Stroke", "Copd", "Renal", "Disease_other", # "married", 
             "educ", "Income_oaa", "Income_oala", 
             "Income_ssa", "Income_work", "Income_saving", "Income_cssa", # "Income_cssa_score", 
             "Income_pension", "Income_child", "Income_other", # "living_status", "housing_type", 
             "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", "FS_total", # "FS_score", 
             "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC", # "AMIC_score", 
             "Self_rated_health", # "Self_rated_health_score", 
             "Satisfaction", # "Satisfaction_score",
             "Meaning_of_life", "Happiness", # "Happiness_score", 
             "Incontinence", "Hospital", "Hospital_day", # "Hospital_score", 
             "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
             "Drug_use", # "Drug_use_score", 
             "risk_score")


for (var in vars1){
  print(cluster_icc(dfwide
                    [dfwide$cluster %in% sample(unique(dfwide$cluster), 30), ] 
                    %>% group_by(cluster) %>% slice_sample(n=9)
                    , paste0(var, ".t0"), "cluster"))
}
for (var in vars1){
  print(cluster_icc(dfwide, paste0(var, ".t1"), "cluster"))
}

for (var in vars2){
  print(cluster_icc(dfwide, paste0(var, ".r1"), "cluster"))
}
for (var in vars2){
  print(cluster_icc(dfwide, paste0(var, ".r2"), "cluster"))
}


