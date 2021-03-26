rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(labelled) # foreign_to_labelled
library(dplyr)

setwd(sprintf("~%s/t2dm", setpath))
df <- foreign_to_labelled(haven::read_sav("Data_request_PHS2014_to_CUHK.sav"))

names(df)[names(df) == "Q1"] <- "sex"
names(df)[names(df) == "Q2"] <- "age"
# names(df)[names(df) == "Age_he"] <- "age" # age at health exam
names(df)[names(df) == "Q3"] <- "born_hk"
names(df)[names(df) == "Q5_g"] <- "educ"
names(df)[names(df) == "Q4"] <- "marital"
names(df)[names(df) == "Q6Q8_g"] <- "econ"
names(df)[names(df) == "Q7_g"] <- "occup"
names(df)[names(df) == "Q9"] <- "income"
names(df)[names(df) == "DCD"] <- "district"
names(df)[names(df) == "A1_g"] <- "hse_type"
names(df)[names(df) == "B8_g"] <- "hse_income"

df$sex <- drop_unused_value_labels(df$sex) %>% to_factor()
df$educ <- drop_unused_value_labels(df$educ) %>% to_factor()
df$marital <- drop_unused_value_labels(df$marital) %>% to_factor()
df$econ <- drop_unused_value_labels(df$econ) %>% to_factor()
df$occup <- drop_unused_value_labels(df$occup) %>% to_factor()
df$income <- drop_unused_value_labels(df$income) %>% to_factor()
df$income <- car::recode(df$income, " 'Refusal' = NA") # min age for PHS survey is 15
df$district <- drop_unused_value_labels(df$district) %>% to_factor()
df$hse_type <- drop_unused_value_labels(df$hse_type) %>% to_factor()
df$hse_income <- drop_unused_value_labels(df$hse_income) %>% to_factor()

df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 20, interval = 5, last_group = 75)
df$age_group <- car::recode(df$age_group, " '0-19' = '15-19' ") # min age for PHS survey is 15

df <- df[df$age <= 84,] # match age range of those who took health exam

# descriptive statistics ----

# create new case-control variable
df$HbA1c <- ifelse(df$HbA1c == 333, 3.8, df$HbA1c) # less than 3.8% censored 


df$case_inc <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & df$Q35 != 1, 1, 
                      ifelse(df$Q35a ==  1, 0, NA)) # incident cases as control
# df[df$case_inc %in% 1 & df$DM_prev != 1, c("HbA1c", "FPG")] # 5 cases not included in official variable

df$case_inc <- ifelse(df$DM_prev == 1, 1, 
                      ifelse(!(df$DM_prev %in% 1) & df$Q35a %in% 1, 0, NA)) # incident cases as control
df$case_pre <- ifelse(df$DM_prev == 1, 1, 
                      ifelse(df$Q35 ==  1, 0, NA)) # prevalent cases as control

numVars <- c("age_group", "sex", "marital", "educ", "econ", "district", "hse_income")
catVars <- c("age_group", "sex", "marital", "educ", "econ", "district", "hse_income")

tableone::CreateTableOne(data =  df, strata = "case_inc", vars = numVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
val_labels(df$DM_prev)
