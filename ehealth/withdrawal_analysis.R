rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(dplyr)
library(labelled)

setwd(sprintf("~%s/ehealth/wbs", setpath))
# # merge datasets ----
# df <- xlsx::read.xlsx2("EHealth_NEW_DATA_WBS_Complete_SCHSA_2021-03-27_update.xlsx", sheetName  = "Raw data"
#                        , encoding = "UTF-8"
#                        , header = TRUE
# )
# 
# # merge 1st refusal data
# temp <- xlsx::read.xlsx2("WBS result of rejected and incomplete baseline 20210331.xlsx", sheetName  = "wbs 1st"
#                             , encoding = "UTF-8"
#                             , header = TRUE
# )
# temp <- temp[1:318,] # remove extra rows
# temp$refused <- 1
# df <- merge(df, temp[, c("Uid", "refused")], # extract item matched by case ID
#             by=c("Uid"), all.x = TRUE)
# rm(temp)
# df$refused <- ifelse(is.na(df$refused), 0, df$refused)
# 
# # merge 2nd refusal data
# temp2 <- xlsx::read.xlsx2("WBS result of rejected and incomplete baseline 20210331.xlsx", sheetName  = "wbs 2nd"
#                           , encoding = "UTF-8"
#                           , header = TRUE
# )
# 
# temp2 <- temp2[1:19,] # remove extra rows
# temp2$refused2 <- 1
# df <- merge(df, temp2[, c("Uid", "refused2")], # extract item matched by case ID
#             by=c("Uid"), all.x = TRUE)
# rm(temp2)
# df$refused2 <- ifelse(is.na(df$refused2), 0, df$refused2)
# 
# temp <- xlsx::read.xlsx2("WBS result of rejected and incomplete baseline 20210331.xlsx", sheetName  = "round 1- 5"
#                          , encoding = "UTF-8"
#                          , header = TRUE
# )
# temp <- temp[1:337,] # remove extra rows
# names(temp)[names(temp)=="UID"] <- "Uid"
# df <- merge(df, temp[, c("Uid", "remark")], # extract item matched by case ID
#             by=c("Uid"), all.x = TRUE)
# rm(temp)
# 
# df$Survey_date <- as.Date(as.numeric(df$Survey_date), origin = "1899-12-30")
# df$Birth_date <- as.Date(as.numeric(df$Birth_date), origin = "1899-12-30")
# names(df)[names(df)=="X.U.4E2D..U.5FC3."] <- "centre_cht"
# names(df)[names(df)=="X.U.6A5F..U.69CB."] <- "ngo_cht"
# 
# saveRDS(df, file = "wbs_data.rds")

# load data ----
df <- readRDS("wbs_data.rds") 
df <- df[df$Questionnaire_code=="Q1",] # subset to first response
df$Age <- as.numeric(df$Age)
df$Age <- ifelse(df$Age<=50, NA, df$Age) # n=2 where age is wrong (age of 0 & 25)

df$age_group <- recode_age(df$Age, age_labels = NULL, second_group = 50, interval = 10, last_group = 80)
df <- droplevels(df) # drop empty categories in variables
vars <- df %>% 
  select(Hypertension, Diabetes, Cholesterol, Heart, Stroke, Copd, Renal, Disease_other, FS_total, SAR_total, 
         AMIC, Self_rated_health, Satisfaction,
         Marital_status, Education, Living_status, Housing_type) %>% names()
df[vars] <- lapply(df[vars], as.numeric)

df$Marital_status <- labelled(df$Marital_status, c("Single" = 1, "Married" = 2, "Widowed" = 3, "Divorced/separated" = 4))
df$Marital_status <- to_factor(df$Marital_status)

df$Education <- labelled(df$Education, c("No schooling" = 1, "Primary" = 2, "Secondary" = 3, "Diploma or above" = 4))
df$Education <- to_factor(df$Education)

df$Living_status <- labelled(df$Living_status, c("Living alone" = 1, "Living with spouse" = 2, "Living with children" = 3, "Living with spouse and children" = 4, "Living with others (e.g.: maid)" = 5))
df$Living_status <- to_factor(df$Living_status)

df$Housing_type <- labelled(df$Housing_type, c("Public rental housing" = 1, "Subsidized housing" = 2, "Private housing" = 3, "Nursing home" = 4, "Other (e.g. subdivided flat)" = 5))
df$Housing_type <- to_factor(df$Housing_type)

# descriptive statistics ----
numVars <- c("Age", "age_group", "Gender", "ngo_cht", 
             "FS_total", "SAR_total", "Hypertension", "Diabetes", "Cholesterol", "Heart", "Stroke", "Copd", "Renal", "Disease_other", 
             "Self_rated_health", "Satisfaction",
             "Marital_status", "Education", "Income", "Living_status", "Housing_type")
catVars <- c("age_group", "Gender",  "ngo_cht", 
             "Marital_status", "Income", "Education", "Living_status", "Housing_type")

tableone::CreateTableOne(data =  df, strata = "refused", vars = numVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
