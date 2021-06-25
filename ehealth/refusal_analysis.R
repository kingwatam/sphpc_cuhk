rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(dplyr)
library(labelled)

Sys.setlocale(locale =  "cht") # set locale to traditional Chinese
setwd(sprintf("~%s/ehealth/wbs", setpath))
# # merge datasets ----
# df <- xlsx::read.xlsx2("EHealth_NEW_DATA_WBS_Complete_SCHSA_2021-06-05.xlsx", sheetName  = "Raw data"
#                        , encoding = "UTF-8"
#                        , header = TRUE
# )
# names(df)[names(df)=="Uid"] <- "member_id"
# 
# setwd(sprintf("~%s/ehealth/wbs", setpath))
# # get 18 district info for each centre
# temp <- xlsx::read.xlsx2("Result_18districtQ1 (2021-01-31).xlsx", sheetName  = "District"
#                            , encoding = "UTF-8"
#                            , header = TRUE
# )
# names(temp)[names(temp)=="Centre.code"] <- "Survey_centre"
# names(temp)[names(temp)=="X18.district"] <- "district_cht"
# 
# df <- merge(df, temp[, c("Survey_centre", "district_cht")], # extract item matched by case ID
#             by=c("Survey_centre"), all.x = TRUE)
# 
# # merge refusal and cannot contact data
# temp <- xlsx::read.xlsx2("EHealthII_baseline_reject list 20120623.xlsx", sheetName  = "reject"
#                          , encoding = "UTF-8"
#                          , header = TRUE
# )
# temp <- temp[1:159,] # remove extra rows
# names(temp)[1] <- "member_id"
# temp <- temp[, c(1, 7:8)]
# temp$type <- "refused"
# 
# 
# temp2 <- xlsx::read.xlsx2("EHealthII_baseline_cannot contact list 20120623.xlsx", sheetName  = "cannot contact"
#                          , encoding = "UTF-8"
#                          , header = TRUE
# )
# temp2 <- temp2[1:82,] # remove extra rows
# names(temp2)[1] <- "member_id"
# temp2 <- temp2[, c(1, 7:8)]
# temp2$type <- "unreachable"
# 
# temp <- rbind(temp, temp2)
# rm(temp2)
# 
# df <- merge(df, temp[, c("member_id", "reason_grouped", "round", "type")], # extract item matched by case ID
#             by=c("member_id"), all.x = TRUE)
# 
# # df <-  merge(temp, df, # extract item matched by case ID
# #                     by=c("Uid"), all.x = TRUE)
# 
# # # legacy code
# # # merge 1st refusal data
# # temp <- xlsx::read.xlsx2("WBS result of rejected and incomplete baseline 20210331.xlsx", sheetName  = "wbs 1st"
# #                             , encoding = "UTF-8"
# #                             , header = TRUE
# # )
# # temp <- temp[1:318,] # remove extra rows
# # temp$refused <- 1
# # df <- merge(df, temp[, c("Uid", "refused")], # extract item matched by case ID
# #             by=c("Uid"), all.x = TRUE)
# # rm(temp)
# # df$refused <- ifelse(is.na(df$refused), 0, df$refused)
# #
# # # merge 2nd refusal data
# # temp2 <- xlsx::read.xlsx2("WBS result of rejected and incomplete baseline 20210331.xlsx", sheetName  = "wbs 2nd"
# #                           , encoding = "UTF-8"
# #                           , header = TRUE
# # )
# #
# # temp2 <- temp2[1:19,] # remove extra rows
# # temp2$refused2 <- 1
# # df <- merge(df, temp2[, c("Uid", "refused2")], # extract item matched by case ID
# #             by=c("Uid"), all.x = TRUE)
# # rm(temp2)
# # df$refused2 <- ifelse(is.na(df$refused2), 0, df$refused2)
# #
# # temp <- xlsx::read.xlsx2("WBS result of rejected and incomplete baseline 20210331.xlsx", sheetName  = "round 1- 5"
# #                          , encoding = "UTF-8"
# #                          , header = TRUE
# # )
# # temp <- temp[1:337,] # remove extra rows
# # names(temp)[names(temp)=="UID"] <- "Uid"
# # df <- merge(df, temp[, c("Uid", "remark")], # extract item matched by case ID
# #             by=c("Uid"), all.x = TRUE)
# # rm(temp)
# 
# df$Survey_date <- as.Date(as.numeric(df$Survey_date), origin = "1899-12-30")
# df$Birth_date <- as.Date(as.numeric(df$Birth_date), origin = "1899-12-30")
# names(df)[names(df)=="X.U.4E2D..U.5FC3."] <- "centre_cht"
# names(df)[names(df)=="X.U.6A5F..U.69CB."] <- "ngo_cht"
# 
# saveRDS(df, file = "refusal_data.rds")

# load data ----
df <- readRDS("refusal_data.rds") 

# get REDCap data
setwd(sprintf("~%s/ehealth", setpath))
temp <- readRDS("ehealth_data.rds") 
df$type <- ifelse(df$member_id %in% unique(temp$member_id), "completed",  df$type)
df$type <- ifelse(is.na(df$type), "not completed", df$type)

df <- df[df$Questionnaire_code=="Q1",] # subset to first response
df$Age <- as.numeric(df$Age)
df$Age <- ifelse(df$Age<=50, NA, df$Age) # n=2 where age is wrong (age of 0 & 25)

df$age_group <- recode_age(df$Age, age_labels = NULL, second_group = 50, interval = 10, last_group = 80)
df <- droplevels(df) # drop empty categories in variables
vars <- df %>% 
  select(Hypertension, Diabetes, Cholesterol, Heart, Stroke, Copd, Renal, Disease_other, FS_total, SAR_total, 
         Memory, Self_rated_health, Satisfaction, Meaning_of_life, Happiness, Overall_score,
         Marital_status, Education, Living_status, Housing_type, 
         Incontinence, Hospital, Hospital_day, Aeservices, SOPD, GOPD, Clinic, Elderly_centre, Drug_use) %>% names()
df[vars] <- lapply(df[vars], as.numeric)

df$Marital_status <- labelled(df$Marital_status, c("Single" = 1, "Married" = 2, "Widowed" = 3, "Divorced/separated" = 4))
df$Marital_status <- to_factor(df$Marital_status)

df$Education <- labelled(df$Education, c("No schooling" = 1, "Primary" = 2, "Secondary" = 3, "Diploma or above" = 4))
df$Education <- to_factor(df$Education)

df$Living_status <- labelled(df$Living_status, c("Living alone" = 1, "Living with spouse" = 2, "Living with children" = 3, "Living with spouse and children" = 4, "Living with others (e.g.: maid)" = 5))
df$Living_status <- to_factor(df$Living_status)

df$Living_alone <- ifelse(df$Living_status == "Living alone", 1, 0)

df$Housing_type <- labelled(df$Housing_type, c("Public rental housing" = 1, "Subsidized housing" = 2, "Private housing" = 3, "Nursing home" = 4, "Other (e.g. subdivided flat)" = 5))
df$Housing_type <- to_factor(df$Housing_type)

df$Incontinence <- labelled(df$Incontinence, c("Incontinence" = 1, "Seldom" = 2, "Self-control" = 3))
df$Incontinence <- to_factor(df$Incontinence)

df$Aeservices_day <- car::recode(df$Aeservices_day, "
'' = 0;
' 6' = 6;
c('1次', 'I ', 'l') = 1;
'2-3' = 2.5;
'3-4次' = 3.5;
'5-6' = 5.5
")
df$Aeservices_day <- as.numeric(df$Aeservices_day)

df$Drug_use <- labelled(df$Drug_use, c("None" = 1, " 1-4" = 2, " 5+" = 3))
df$Drug_use <- to_factor(df$Drug_use)

# descriptive statistics ----
numVars <- c("Age", "age_group", "Gender", "ngo_cht", "district_cht", "Overall_score",
             "FS_total", "SAR_total", "Hypertension", "Diabetes", "Cholesterol", "Heart", "Stroke", "Copd", "Renal", "Disease_other", 
             "Self_rated_health", "Satisfaction", "Meaning_of_life", "Happiness", 
             "Marital_status", "Education", "Income", "Living_status", "Living_alone", "Housing_type", 
             "Incontinence", "Hospital", "Hospital_day", "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", "Drug_use")
catVars <- c("age_group", "Gender",  "ngo_cht", "district_cht",
             "Marital_status", "Income", "Education", "Living_status", "Housing_type", 
             "Incontinence", "Drug_use")

tableone::CreateTableOne(data =  df[df$type %in% c("completed", "refused"),], strata = "type", vars = numVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

tableone::CreateTableOne(data =  df[df$type %in% c("completed", "unreachable"),], strata = "type", vars = numVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

tableone::CreateTableOne(data =  df[df$type %in% c("completed", "not completed"),], strata = "type", vars = numVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
