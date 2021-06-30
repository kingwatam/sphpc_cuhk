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

Sys.setlocale(locale =  "eng")
# import cleaned data
setwd(sprintf("~%s/ehealth", setpath))
df <- readRDS("ehealth_data.rds")
wbs <- readRDS("wbs_data.rds")

import_func("ehealth_analysis.R", encoding = "UTF-8") # import to_character_df() & to_English() functions

df %>% select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
df %>% select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
df %>% select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
df %>% select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
df %>% select(starts_with("diet_")) %>% colnames(.) -> diet_
df %>% select(starts_with("eq5d")) %>% colnames(.) -> eq5d_

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
")

# restrict sample to January 2021 & age >= 60 ----
df <- df[df$ehealth_eval_timestamp <= as.Date('2021-01-31'),]
df <- df[which(df$age >= 60),]

## descriptive statistics ----
df$self_efficacy_onetwo <- ifelse(df$self_efficacy_1 %in% 1:2 |
                                    df$self_efficacy_2 %in% 1:2 |
                                    df$self_efficacy_3 %in% 1:2 |
                                    df$self_efficacy_4 %in% 1:2 |
                                    df$self_efficacy_5 %in% 1:2, 1, 0)
df$self_efficacy_allone <- ifelse(df$self_efficacy_1 %in% 1 |
                                    df$self_efficacy_2 %in% 1 |
                                    df$self_efficacy_3 %in% 1 |
                                    df$self_efficacy_4 %in% 1 |
                                    df$self_efficacy_5 %in% 1, 1, 0)

# demographics ----
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
  
  allVars <- c("age", "age_group", "gender", "marital", "educ", "living_status", "housing_type")
  catVars <- c("age_group", "gender", "marital", "educ", "living_status", "housing_type")
  tableone::CreateTableOne(data =  df,
                           # strata = c(""),
                           vars = allVars, factorVars = catVars) %>%
    print(showAllLevels = TRUE) %>% clipr::write_clip()
}

get_descstat(df)

# baseline assessments ----
allVars <- c("use_health_service_8", "pase_c")
# catVars <- c("age_group", "gender", "marital", "educ", "living_status", "housing_type")
otherVars <- c("use_health_service_8", "pase_c")
tableone::CreateTableOne(data =  df, 
                         # strata = c(""),
                         vars = allVars
                         # , factorVars = catVars
) %>% 
  print(showAllLevels = TRUE, nonnormal = otherVars) %>% clipr::write_clip()

allVars <- c("use_health_service_8", "amic", "amic_sum", 
             "self_efficacy", "self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5",
             "eq5d", "eq5d_health", "satisfaction_1", "satisfaction_2", 
             "pase_c", "pase_c_1", "pase_c_1_2", "pase_c_11", "pase_c_11_1", "pase_c_12_1", "pase_c_12",
             "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
             "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5")
catVars <- c("amic",
             "self_efficacy_1", "self_efficacy_2", "self_efficacy_3", "self_efficacy_4", "self_efficacy_5", 
             "pase_c_1", "pase_c_1_2", "pase_c_11", "pase_c_12_1", 
             "matrix_diet_dh3", "matrix_diet_dh4", "matrix_diet_dh7", "matrix_diet_dh8",
             "diet_dp1", "diet_dp3", "diet_dp4", "diet_dp5")
tableone::CreateTableOne(data =  to_English(to_character_df(df, catVars)) %>% filter(time==0), 
                         # strata = c(""),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE)  %>% clipr::write_clip() # %>% writeClipboard(format = 13)

# plot data collection dates ----
Sys.setlocale("LC_ALL", "English")
ggplot2::ggplot(df, aes(x=ehealth_eval_timestamp, y=evaluation_event)) + 
  geom_point() + 
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels="%b/%Y") +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1))

# withdrawal statistics ----
setwd(sprintf("~%s/ehealth/fwweeklyehealthreport_20210515", setpath))
withdrawals <- xlsx::read.xlsx2("WithdrawalList_2021-05-15.xlsx", sheetName  = "Sheet 1")

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
'搬遷' = 'Relocation';
'照顧家人' = 'Care for family';
'資料錯誤' = 'Wrong information';
'轉中心' = 'Change of centre';
'離世' = 'Death'
")


withdrawals <- withdrawals[withdrawals$withdrawal_date <= as.Date('2021-01-31'),]

withdrawals$risk_level2 <- ifelse(withdrawals$risk_level %in% 1:2, "low-medium", 
                                  ifelse(withdrawals$risk_level == 3, "high", NA))
val_labels(withdrawals$risk_level) <- c(low = 1, medium = 2, high = 3)

allVars <- c("withdrawal_reason")
catVars <- c("withdrawal_reason")
tableone::CreateTableOne(data =  withdrawals[], 
                         strata = c("risk_level"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE)  %>% clipr::write_clip() # %>% writeClipboard(format = 13)

withdrawals <- withdrawals %>%
  group_by(risk_level, withdrawal_reason) %>%
  summarise(count = n()) %>%
  group_by(risk_level) %>%
  mutate(per=count/sum(count)) %>% 
  ungroup()

withdrawals$risk_level <- to_factor(withdrawals$risk_level)
ggplot(withdrawals[!is.na(withdrawals$risk_level),], aes(x= "", y = per, fill=withdrawal_reason)) + 
  geom_col() +
  facet_wrap(~risk_level)+
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(per*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title= "Risk levels", x="", y="") + 
  guides(fill = guide_legend(title = "Reasons for withdrawal")) +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # plot.title = element_text(hjust = 0.5, color = "#666666")
  )