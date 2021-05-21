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

Sys.setlocale(locale =  "cht") # set locale to traditional Chinese
setwd(sprintf("~%s/ehealth", setpath))
df <- foreign_to_labelled(haven::read_sav("EHealthIIEvaluation_DATA_NOHDRS_2021-05-18_0953.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

# # load Eva's data
# XLConnect::xlcFreeMemory() # rtools is also required to be installed to avoid error
# df2 <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210426.xlsx", sheetName  = "Raw data"
#                       , encoding = "UTF-8"
#                       , header = FALSE
#                       )
# df2 <- df2[2:nrow(df2),]
# names(df2) <- c(names(df), names(df2)[87:96])

# survey data cleaning ----
temp <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210517_duplicates.xlsx", sheetName  = "duplicate record"
                         , encoding = "UTF-8"
                         , header = TRUE
)
# obtain list of duplicates
dupes <- c(1:25,
          temp$Duplicate.empty.test.wrong.ID[2:nrow(temp)],
          temp$X.[temp$X. != ""],
          1324)
rm(temp)
df <- df[!(df$record_id %in% dupes),] # remove duplicates

# hardcode missing timestamps 
df$ehealth_eval_timestamp[df$record_id %in% df$record_id[df$ehealth_eval_timestamp %in% ""]] <-
  c("2020-10-15 15:01:00", "2020-10-15 15:45:00", "2020-10-15 16:20:00", "2020-10-16 10:00:00")

# fix problematic member IDs
gsub("\t", "", "\tSAG03M223")
df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)] <- 
  gsub("\t", "", df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)]) # remove "\t"

df$ehealth_eval_timestamp <- as.Date(df$ehealth_eval_timestamp)

# # examine duplicates
# wbs %>%
#   add_count(member_id) %>%
#   filter(n >= 2) %>% View()

# scoring <- function(df){
  df %>% select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
  df %>% select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
  df %>% select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
  df %>% select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
  df %>% select(starts_with("diet_")) %>% colnames(.) -> diet_
  df %>% select(starts_with("eq5d")) %>% colnames(.) -> eq5d_
  
  # replace certain labels with NAs
  df[] <- lapply(df[], function(x){
    if (is.labelled(x) %in% TRUE){
      na_value <- val_labels(x)[which(colnames(t(val_labels(x)))  %in% c("不適用/不願意作答", "不適用/不願意作答。", "88.不清楚/不好說"))]
      if (length(na_value) == 0){
        return(x)
      }
      na_value <- na_value[[1]]
      x <- labelled_spss(x, labels = val_labels(x), na_values = na_value)
      x <- user_na_to_na(x)
      # val_label(x, na_value) <- NA
      return(x)
    } else {
      return(x)
    }
  })
  
  df <- df %>%
    mutate(
           # amic = rowSums(.[amic_], na.rm = FALSE), # na.rm = FALSE means any NA in an item results in NA for the scale
           self_efficacy = rowSums(.[self_efficacy_], na.rm = FALSE),
           pase_c = rowSums(.[pase_c_], na.rm = TRUE)
    )
# return(subset(df, select = c(amic, self_efficacy, pase_c
# )))
# }

# df <- cbind(df, scoring(df))
  
# reverse EQ5D items
df[eq5d_[1:5]] <-  (df[eq5d_[1:5]]-6)*-1

df$eq5d_score <- ifelse(is.na(df$eq5d_mobility) | is.na(df$eq5d_self_care) | is.na(df$eq5d_usual_activity) | 
                          is.na(df$eq5d_pain_discomfort) | is.na(df$eq5d_anxiety_depression),
                          NA, paste0(df$eq5d_mobility, df$eq5d_self_care, df$eq5d_usual_activity, df$eq5d_pain_discomfort, df$eq5d_anxiety_depression))
df$eq5d =  eq5d_fast(scores=df$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)

# import WBS data ----
setwd(sprintf("~%s/ehealth/JCreport 20210409/data/Overall", setpath))
wbs <- foreign_to_labelled(haven::read_sav("EHealth_NEW_DATA_WBS_Complete_2021-01-31_Overall.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

setwd(sprintf("~%s/ehealth/fwweeklyehealthreport_20210515", setpath))
wbs2 <- xlsx::read.xlsx2("EHealth_NEW_DATA_WBS_Complete_SCHSA_2021-05-15.xlsx", sheetName  = "Raw data"
                         )
# names(wbs2)[names(wbs2)=="Memory"] <- "AMIC"
# names(wbs2)[names(wbs2)=="Memory_score"] <- "AMIC_score"
# 
# vars <- c("Uid", "Gender", "Birth_date", "Survey_date",
#           "Marital_status", "Education", "Overall_score", "Risk_level")
# 
# wbs2[,c("Marital_status", "Education")] <-
#   lapply(wbs2[, c("Marital_status", "Education")],type.convert,as.is=TRUE)
# 
# val_labels(wbs2$Gender) <- val_labels(wbs$Gender)
# val_labels(wbs2$Marital_status) <- val_labels(wbs$Marital_status)
# val_labels(wbs2$Education) <- val_labels(wbs$Education)
# 
# wbs2$Risk_level <- car::recode(wbs2$Risk_level, "
# 'L' = 1; 
# 'M' = 2;
# 'H' = 3
# ")
# val_labels(wbs2$Risk_level) <- c("L" = 1, "M" = 2, "H" = 3)
# wbs2$Birth_date <- as.Date(as.numeric(wbs2$Birth_date), origin = "1899-12-30")

for (i in c("wbs"
            , "wbs2"
            )){ # change variable names for both dfs
  df.tmp <- get(i)
  names(df.tmp)[names(df.tmp)=="Uid"] <- "member_id"
  names(df.tmp)[names(df.tmp)=="Gender"] <- "gender"
  names(df.tmp)[names(df.tmp)=="Birth_date"] <- "dob"
  names(df.tmp)[names(df.tmp)=="Survey_date"] <- "wbs_survey_date"
  names(df.tmp)[names(df.tmp)=="Marital_status"] <- "marital"
  names(df.tmp)[names(df.tmp)=="Education"] <- "educ"
  names(df.tmp)[names(df.tmp)=="Overall_score"] <- "risk_score"
  names(df.tmp)[names(df.tmp)=="Risk_level"] <- "risk_level"
  names(df.tmp)[names(df.tmp)=="Digital"] <- "digital"
  names(df.tmp)[names(df.tmp)=="Living_status"] <- "living_status"
  names(df.tmp)[names(df.tmp)=="Housing_type"] <- "housing_type"
  assign(i, df.tmp)
  rm(df.tmp)
}

vars <- c("member_id", "gender", "dob", "wbs_survey_date",
          "marital", "educ", "living_status", "housing_type",
          "risk_score", "risk_level", "digital")

# df1 <- df
# wbs2t0 <- wbs2[wbs2$Questionnaire_code=="Q1",] # baseline only
# df1 <- merge(df1, wbs2t0[, vars], # extract item matched by member ID
#              by=c("member_id"), all.x = TRUE)

wbst0 <- wbs[wbs$Questionnaire_code=="Q1",] # baseline only
df <- merge(df, wbst0[, vars], # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

df$age <- floor(interval(df$dob, df$ehealth_eval_timestamp) / duration(num = 1, units = "years")) # https://stackoverflow.com/a/27363833

df$age_group <- recode_age(df$age, age_labels = c("60-69", "70-79", "80+"))
df$age_group <- relevel(as.factor(df$age_group), ref = "60-69")

# restrict sample to January 2021 & age >= 60 ----
df <- df[df$ehealth_eval_timestamp <= as.Date('2021-01-31'),]
df <- df[which(df$age >= 60),]

## descriptive statistics ----
df$amic <- ifelse(df$amic_sum >= 3, 1, 0)
val_labels(df$amic) <- c("AMIC >= 3" = 1, "AMIC < 3" = 0)
df$pase_c_12_1 <- ifelse(df$pase_c_12 > 0, 1, 0) # Walking in general as binary
val_labels(df$pase_c_12_1) <- c(Yes=1, No=0)

# demographics ----
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
  label_order <- c(which(names(valLabels) == "每餐約小半碗或以下"),
                   which(names(valLabels) == "每餐約半碗"),
                   which(names(valLabels) == "每餐約大半碗至1碗"),
                   which(names(valLabels) == "每餐多於1碗"))
  names(valLabels)[names(valLabels) == "每餐多於1碗"] <- "> 1 bowl"
  names(valLabels)[names(valLabels) == "每餐約大半碗至1碗"] <- "0.5-1 bowl"
  names(valLabels)[names(valLabels) == "每餐約小半碗或以下"] <- "< 0.5 bowl"
  names(valLabels)[names(valLabels) == "每餐約半碗"] <- "0.5 bowl"
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
  
  
  return(df)
}

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
tableone::CreateTableOne(data =  to_English(to_character_df(df, catVars)), 
                         # strata = c(""),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE)  %>% clipr::write_clip() # %>% writeClipboard(format = 13)

# plot data collection dates ----
Sys.setlocale("LC_ALL", "English")
ggplot2::ggplot(df, aes(x=ehealth_eval_timestamp, y=evaluation_event)) + 
  geom_point() + 
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels="%b/%Y") +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1))

# write.csv(df, "test.csv")

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

  
