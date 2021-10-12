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

vars <- c("Survey_centre", "wbs_survey_date", "gender", "dob",
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


allVars <- c("Carer", "Hypertension", "Hypertension_HA",
          "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", "Heart_score", "Stroke", "Copd", "Renal", 
          "Disease_other", "marital", "educ", "Income_oaa", "Income_oala", 
          "Income_ssa", "Income_work", "Income_saving", "Income_cssa", "Income_cssa_score", "Income_pension",
          "Income_child", "Income_other", "living_status", "housing_type", "Rent", "Own", "FS1", "FS2", "FS3",
          "FS4", "FS5", "FS_total", "FS_score", "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC",
          "AMIC_score", "Self_rated_health", "Self_rated_health_score", "Satisfaction", "Satisfaction_score",
          "Meaning_of_life", "Happiness", "Happiness_score", "Incontinence", "Hospital", "Hospital_day", 
          "Hospital_score", "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
          "Drug_use", "Drug_use_score", "risk_score")

ordinalVars <- c("marital")

import_func("ehealth_analysis.R")
Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(df = wbs[wbs$Round %in% c(1,2) & wbs$risk_level == 2, ], id = "member_id", group = "Round", to_English = FALSE, vars = allVars[], ordinalVars =  NULL, medianVars = NULL) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 
