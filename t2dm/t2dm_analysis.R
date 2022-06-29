rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(labelled) # foreign_to_labelled
library(dplyr)

# import data & clean ----
setwd(sprintf("~%s/t2dm", setpath))
df <- foreign_to_labelled(haven::read_sav("Data_request_PHS2014_to_CUHK.sav"))
temp <- xlsx::read.xlsx2("Copy of Data_request_PHS2014_to_CUHK_additional.xlsx", sheetName  = "Extract_cases"
                         , header = TRUE
)
temp[,2:8] <- sapply(temp[,2:8], as.numeric)
temp$Gender <- NULL
temp$Age <- NULL
temp$Dental.problem <- (temp$Dental.problem-2)*-1 # from 2 = No to 0 = No
df <-  merge(df, temp, # extract item matched by case ID
             by=c("DH_Reference_No"), all.x = TRUE)

rm(temp)

temp <- foreign_to_labelled(haven::read_sav("DH_PHS 2014-15_WC_HC_15.03.2022.sav"))
df <-  merge(df, temp, # extract item matched by case ID
             by=c("DH_Reference_No", "Q1", "Q2"), all.x = TRUE)
rm(temp)

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
names(df)[names(df) == "Q54_rgp"] <- "smoking"
names(df)[names(df) == "Q112"] <- "checkup"
names(df)[names(df) == "Q112_mm_cate"] <- "checkup_freq"

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
df$HT_prev <- drop_unused_value_labels(df$HT_prev) %>% to_factor()
df$hyperchol_prev <- drop_unused_value_labels(df$hyperchol_prev) %>% to_factor()
df$hyperchol_prev <- relevel(df$hyperchol_prev, ref = "Previously known history with normal cholesterol")
df$hyperchol_prev2 <- car::recode(df$hyperchol_prev,
                                   "c('Previously known history without current drug treatment',
                                   'Previously known history with current drug treatment') = 'Previously known history with/without current drug treatment'
                                   ")
df$hyperchol_prev2 <- relevel(df$hyperchol_prev2, ref = "Never had hypercholesterolaemia")
df$Q10 <- drop_unused_value_labels(df$Q10) %>% to_factor()
df$health <- (as.numeric(df$Q10)-6)*-1 # reverse self rated health from lower as better to lower as worse
df$smoking <- drop_unused_value_labels(df$smoking) %>% to_factor()
df$checkup <- drop_unused_value_labels(df$checkup) %>% to_factor()
df$checkup <- ifelse(df$checkup == "Yes", 1, ifelse(df$checkup == "No", 0, NA))
df$checkup_freq <- drop_unused_value_labels(df$checkup_freq) %>% to_factor()
df$checkup_freq <- car::recode(df$checkup_freq, " 'Not Applicable' = 'None'")
df$checkup_freq <- factor(df$checkup_freq, levels = c("None", "More than 24 months", "13-24 months", "Less than 13 months"))
df$checkup_freq <- relevel(df$checkup_freq, ref = "None")
names(df)[names(df)=="Q120"] <- "family_doctor"
df$family_doctor <- ifelse(df$family_doctor == 1, 1,
                           ifelse(df$family_doctor == 2, 0, NA))

df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 20, interval = 5, last_group = 75)
df$age_group <- car::recode(df$age_group, " '0-19' = '15-19' ") # min age for PHS survey is 15
df$age_cat <- recode_age(df$age, age_labels = NULL, second_group = 50, interval = 15, last_group = 80)
df$age_cat <- car::recode(df$age_cat, " '0-49' = '15-49' ") # min age for PHS survey is 15

df$female <- ifelse(df$sex == "Female", 1, 0)
df$male <- ifelse(df$sex == "Male", 1, 0)

df$HbA1c <- ifelse(df$HbA1c == 333, 3.8, df$HbA1c) # less than 3.8% is censored 

df$Waist_circum <- ifelse(df$Waist_circum == 777, NA, df$Waist_circum)
df$Hip_circum <- ifelse(df$Hip_circum == 777, NA, df$Hip_circum)
df$WHR <- ifelse(df$WHR == 777, NA, df$WHR)

# coding undiagnosed, incidence, prevalent ----
df$case_inc <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & (df$Q35a %!in% 1 & df$Q35 %!in% 1 & df$Q36c %!in% 1), 1, 
                       ifelse(df$Q35a ==  1, 0, NA)) # incident cases as control

df$case_prev <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & (df$Q35a %!in% 1 & df$Q35 %!in% 1 & df$Q36c %!in% 1), 1, 
                        ifelse(df$Q35 ==  1 | df$Q35a ==  1 | df$Q36c == 1, 0, NA)) # prevalent cases as control

df$case_inc_ <- ifelse(df$case_inc %in% 1, "undiagnosed",
                       ifelse(df$case_inc %in% 0, "incident DM", NA))
df$case_prev_ <- ifelse(df$case_prev %in% 1, "undiagnosed",
                        ifelse(df$case_prev %in% 0, "prevalent DM", NA))

df$normoglycemic <-  ifelse(  (df$HbA1c < 6.5 & df$FPG < 7), 1, 0)

df$case_inc_norm_ <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & (df$Q35a %!in% 1 & df$Q35 %!in% 1 & df$Q36c %!in% 1), "Undiagnosed", 
                              ifelse( df$Q35a ==  1, "Incident", 
                                             ifelse(  (df$HbA1c < 6.5 & df$FPG < 7) & (df$Q35a %!in% 1 & df$Q35 %!in% 1 & df$Q36c %!in% 1), "Normal", NA))) # excluding prevalent cases

df$case_prev_norm_ <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & (df$Q35a %!in% 1 & df$Q35 %!in% 1 & df$Q36c %!in% 1), "Undiagnosed", 
                             ifelse( df$Q35 ==  1 | df$Q35a ==  1 | df$Q36c == 1, "Prevalent", 
                                     ifelse(  (df$HbA1c < 6.5 & df$FPG < 7) & (df$Q35a %!in% 1 & df$Q35 %!in% 1 & df$Q36c %!in% 1), "Normal", NA))) # excluding prevalent cases

df$case_inc_norm_ <- relevel(as.factor(df$case_inc_norm_), ref = "Incident")
df$case_prev_norm_ <- relevel(as.factor(df$case_prev_norm_), ref = "Prevalent")

df$case_norm_ <- ifelse(df$case_inc_norm_ %!in% "Incident", as.character(df$case_inc_norm_), NA)

df$HTcase_prev_norm_ <- ifelse( (df$Systolic_BP >= 140 | df$Diastolic_BP >= 90) & (df$Q32  %!in% 1 & df$Q32a %!in% 1 & df$Q33a %!in% 1), "Undiagnosed HT", 
                          ifelse(df$Q32 == 1 | df$Q32a == 1 | df$Q33a == 1, "Prevalent", 
                                 ifelse( (df$Systolic_BP < 140 & df$Diastolic_BP < 90) & (df$Q32  %!in% 1 & df$Q32a %!in% 1 & df$Q33a %!in% 1), "Normal", NA))) # prevalent cases as control

df$HTcase_inc_norm_ <- ifelse( (df$Systolic_BP >= 140 | df$Diastolic_BP >= 90) & (df$Q32  %!in% 1 & df$Q32a %!in% 1 & df$Q33a %!in% 1), "Undiagnosed HT", 
                                ifelse(df$Q32a == 1 , "Incident", 
                                       ifelse( (df$Systolic_BP < 140 & df$Diastolic_BP < 90) & (df$Q32  %!in% 1 & df$Q32a %!in% 1 & df$Q33a %!in% 1), "Normal", NA))) # prevalent cases as control

df$cholcase_prev_norm_ <- ifelse( (df$Total_Chol >= 5.2) & (df$Q29  %!in% 1 & df$Q29a %!in% 1 & df$Q30a %!in% 1), "Undiagnosed hyperchol", 
                                ifelse(df$Q29 == 1 | df$Q29a == 1 | df$Q30a == 1, "Prevalent", 
                                       ifelse( (df$Total_Chol < 5.2) & (df$Q29  %!in% 1 & df$Q29a %!in% 1 & df$Q30a %!in% 1), "Normal", NA))) # prevalent cases as control

df$cholcase_inc_norm_ <- ifelse( (df$Total_Chol >= 5.2) & (df$Q29  %!in% 1 & df$Q29a %!in% 1 & df$Q30a %!in% 1), "Undiagnosed hyperchol", 
                               ifelse(df$Q29a == 1 , "Incident", 
                                      ifelse( (df$Total_Chol < 5.2) & (df$Q29  %!in% 1 & df$Q29a %!in% 1 & df$Q30a %!in% 1), "Normal", NA))) # prevalent cases as control

df$HTcase_inc_norm_ <- relevel(as.factor(df$HTcase_inc_norm_), ref = "Incident")
df$HTcase_prev_norm_ <- relevel(as.factor(df$HTcase_prev_norm_), ref = "Prevalent")
df$cholcase_inc_norm_ <- relevel(as.factor(df$cholcase_inc_norm_), ref = "Incident")
df$cholcase_prev_norm_ <- relevel(as.factor(df$cholcase_prev_norm_), ref = "Prevalent")

df <- df[df$age <= 84,] # match age range of those who took health exam
df$econ_active <- ifelse(df$econ == "Economically active persons", 1, 0)

# recode income ----
df$income <- factor(df$income, ordered = TRUE, levels = c("$0 No income",
                                                          "$1 - $1,999",
                                                          "$2,000 - $3,999",
                                                          "$4,000 - $4,999",
                                                          "$5,000 - $5,999",
                                                          "$6,000 - $6,999",
                                                          "$7,000 - $7,999",
                                                          "$8,000 - $8,999",
                                                          "$9,000 - $9,999",
                                                          "$10,000 - $12,499",
                                                          "$12,500 - $14,999",
                                                          "$15,000 - $19,999",
                                                          "$20,000 - $24,999",
                                                          "$25,000 - $29,999",
                                                          "$30,000 - $39,999",
                                                          "$40,000 - $49,999",
                                                          "$50,000 or above"))
df$income <- factor(df$income, ordered = FALSE)
            
df$income_num <- car::recode(df$income, "
'$0 No income' = 0;
'$1 - $1,999' = 1000;
'$2,000 - $3,999' = 3000;
'$4,000 - $4,999' = 4500;
'$5,000 - $5,999' = 5500;
'$6,000 - $6,999' = 6500;
'$7,000 - $7,999' = 7500;
'$8,000 - $8,999' = 8500;
'$9,000 - $9,999' = 9500;
'$10,000 - $12,499' = 11250;
'$12,500 - $14,999' = 13750;
'$15,000 - $19,999' = 17500;
'$20,000 - $24,999' = 22500;
'$25,000 - $29,999' = 27500;
'$30,000 - $39,999' = 35000;
'$40,000 - $49,999' = 45000;
'$50,000 or above' = 50000
")
df$income_num <- as.numeric(to_character(df$income_num))
                                              
# quantile(df$income, c(.25, 0.5, 0.75), type=1, na.rm = TRUE) # show quantiles for ordered factor
df$income_cat6 <- car::recode(df$income, "
'$0 No income' = '$0 No income';

c('$1 - $1,999',
'$2,000 - $3,999',
'$4,000 - $4,999',
'$5,000 - $5,999') = '$1 - $5,999';

c('$6,000 - $6,999',
'$7,000 - $7,999',
'$8,000 - $8,999',
'$9,000 - $9,999',
'$10,000 - $12,499') = '$6,000 - $12,499';

c('$12,500 - $14,999',
'$15,000 - $19,999') = '$12,500 - $19,999';

c('$20,000 - $24,999',
'$25,000 - $29,999') = '$20,000 - $29,999';

c('$30,000 - $39,999',
'$40,000 - $49,999',
'$50,000 or above') = '$30,000 or above'
")

df$income_cat6 <- factor(df$income_cat6, ordered = TRUE, levels = c("$0 No income",
                                                          "$1 - $5,999",
                                                          "$6,000 - $12,499",
                                                          "$12,500 - $19,999",
                                                          "$20,000 - $29,999",
                                                          "$30,000 or above"))
df$income_cat6 <- factor(df$income_cat6, ordered = FALSE)


df$income_cat3 <- car::recode(df$income, "
c('$0 No income',
'$1 - $1,999',
'$2,000 - $3,999',
'$4,000 - $4,999',
'$5,000 - $5,999') = '$0 - $5,999';

c('$6,000 - $6,999',
'$7,000 - $7,999',
'$8,000 - $8,999',
'$9,000 - $9,999',
'$10,000 - $12,499',
'$12,500 - $14,999',
'$15,000 - $19,999') = '$6,000 - $19,999';

c() = '$20,000 - $29,999';

c('$20,000 - $24,999',
'$25,000 - $29,999',
'$30,000 - $39,999',
'$40,000 - $49,999',
'$50,000 or above') = '$20,000 or above'
")

df$income_cat3 <- factor(df$income_cat3, ordered = TRUE, levels = c("$0 - $5,999",
                                                                    "$6,000 - $19,999",
                                                                    "$20,000 or above"))
df$income_cat3 <- factor(df$income_cat3, ordered = FALSE)


# physical activity ----
df <- df %>%
  rowwise() %>% 
  mutate(
    # MET_week_con = sum(8*pa_work_vig, 4*pa_work_mod, 4*pa_trans, 8*pa_recr_vig, 4*pa_recr_mod, na.rm = TRUE), # reproduce MET_week_con variable
    pa_vig = sum(pa_work_vig, pa_recr_vig, na.rm = TRUE),
    pa_mod = sum(pa_work_mod, pa_recr_mod, na.rm = TRUE),
    MET_work = sum(8*pa_work_vig, 4*pa_work_mod, na.rm = TRUE), 
    MET_recr = sum(8*pa_recr_vig, 4*pa_recr_mod, na.rm = TRUE),
    MET_trans = sum(4*pa_trans, na.rm = TRUE),
    MET_work_trans = sum(8*pa_work_vig, 4*pa_work_mod, 4*pa_trans, na.rm = TRUE)
  ) 

# df$who_pa <- ifelse(df$pa_mod >= 150 | df$pa_vig >= 75 | df$MET_week_con >= 600, 1, 0) # reproduce who_pa variable
# df$who_pa_MET <- ifelse(df$MET_week_con >= 600, 1, 0) 
# df$who_pa_vig <- ifelse(df$pa_vig >= 75, 1, 0)
df$who_pa_mod <- ifelse(df$pa_mod >= 30, 1, 0)
df$who_pa_trans <- ifelse(df$pa_trans >= 110, 1, 0)

df$pa_work_prop <- ifelse(df$MET_week_con %in% 0, 0, df$MET_work/df$MET_week_con)
df$pa_recr_prop <-  ifelse(df$MET_week_con %in% 0, 0, df$MET_recr/df$MET_week_con)
df$pa_trans_prop <-  ifelse(df$MET_week_con %in% 0, 0, df$MET_trans/df$MET_week_con)
df$pa_work_trans_prop <-  ifelse(df$MET_week_con %in% 0, 0, df$MET_work_trans/df$MET_week_con)

df$pa_work <- df$pa_work_mod + df$pa_work_vig
df$pa_recr <- df$pa_recr_mod + df$pa_recr_vig

# medical conditions ever diagnosed ----
df$cancer <- ifelse(df$Q26a == 1, 1, 
                    ifelse(df$Q26a == 2, 0, NA))
df$stroke <- ifelse(df$Q26b == 1, 1, 
                    ifelse(df$Q26b == 2, 0, NA))
df$CHD <- ifelse(df$Q26c == 1, 1, 
                    ifelse(df$Q26c == 2, 0, NA))
df$asthma <- ifelse(df$Q26d == 1, 1, 
                 ifelse(df$Q26d == 2, 0, NA))
df$copd <- ifelse(df$Q26e == 1, 1, 
                    ifelse(df$Q26e == 2, 0, NA))
df$oth_respir <- ifelse(df$Q26f == 1, 1, 
                  ifelse(df$Q26f == 2, 0, NA))
df$hi_glucose <- ifelse(df$Q35 == 2, 1,
                        ifelse(df$Q35 %in% c(1, 3), 0, NA))
df$diabetes <- ifelse(df$Q35 == 1, 1,
                        ifelse(df$Q35 %in% c(2, 3), 0, NA))
df$hyperchol <- ifelse(df$Q29 == 1, 1, 
                        ifelse(df$Q29 == 2, 0, NA))
df$hypertension <- ifelse(df$Q32 == 1, 1, 
                       ifelse(df$Q32 == 2, 0, NA))
df$musculo  <- ifelse(df$Q27j == 1, 1, 
                       ifelse(df$Q27j == 2, 0, NA))
df$skin  <- ifelse(df$Q27l == 1, 1, 
                      ifelse(df$Q27l == 2, 0, NA))
df$ENT  <- ifelse(df$Q27c == 1, 1, 
                   ifelse(df$Q27c == 2, 0, NA))
df$thyroid  <- ifelse(df$Q27d == 1, 1, 
                  ifelse(df$Q27d == 2, 0, NA))
df$liver  <- ifelse(df$Q27f == 1, 1, 
                      ifelse(df$Q27f == 2, 0, NA))
df$anemia  <- ifelse(df$Q27h == 1, 1, 
                    ifelse(df$Q27h == 2, 0, NA))
df$stomach  <- ifelse(df$Q27g == 1, 1, 
                     ifelse(df$Q27g == 2, 0, NA))
df$kidney  <- ifelse(df$Q27e == 1, 1, 
                      ifelse(df$Q27e == 2, 0, NA))
df$blood  <- ifelse(df$Q27i == 1, 1, 
                     ifelse(df$Q27i == 2, 0, NA))
df$immune  <- ifelse(df$Q27k == 1, 1, 
                    ifelse(df$Q27k == 2, 0, NA))
df$parkinson  <- ifelse(df$Q27b == 1, 1, 
                     ifelse(df$Q27b == 2, 0, NA))
df$epilepsy  <- ifelse(df$Q27a == 1, 1, 
                        ifelse(df$Q27a == 2, 0, NA))
df$depression  <- ifelse(df$Q27m == 1, 1, 
                       ifelse(df$Q27m == 2, 0, NA))
df$anxiety  <- ifelse(df$Q27n == 1, 1, 
                         ifelse(df$Q27n == 2, 0, NA))
df$schizo  <- ifelse(df$Q27o == 1, 1, 
                      ifelse(df$Q27o == 2, 0, NA))
df$dementia  <- ifelse(df$Q27p == 1, 1, 
                     ifelse(df$Q27p == 2, 0, NA))
df$oth_chronic  <- ifelse(df$Q27qrs_regp == 1, 1, 
                       ifelse(df$Q27qrs_regp == 2, 0, NA))
df$eye  <- ifelse(df$Q39 == 1, 1, 
                  ifelse(df$Q39 == 2, 0, NA))

# healthy (without co-morbidity) ----
all_conditions <- c("cancer", "stroke", "CHD",
                    "asthma", "copd", "oth_respir",
                    "hi_glucose", "diabetes", "hyperchol", "hypertension", 
                    "musculo", "skin", "ENT",
                    "thyroid", "liver", "anemia",
                    "stomach", "kidney", "blood",
                    "immune", "parkinson", "epilepsy",
                    "depression", "anxiety", "schizo",
                    "dementia", "oth_chronic", "eye")
mental_disorders <- c("depression", "anxiety", "schizo")
comorbid_conditions  <-  c("cancer", "stroke", "CHD",
                           "hi_glucose", "diabetes", "hyperchol", "hypertension",
                           "kidney")

df$any_disease <- ((rowSums(df[, all_conditions] == 1, na.rm=TRUE) > 0) * 1)
df$any_comorbidity <- ((rowSums(df[, comorbid_conditions] == 1, na.rm=TRUE) > 0) * 1)

df$mental_disorder <- ((rowSums(df[, mental_disorders] == 1, na.rm=TRUE) > 0) * 1)
df$depression_anxiety <- ((rowSums(df[, mental_disorders[1:2]] == 1, na.rm=TRUE) > 0) * 1)

df$case_allhealthy <- ifelse(is.na(df$Age_he), NA, 
                          ifelse(df$any_disease == 0 & df$case_prev %in% c(NA, 0), 0, 
                                 ifelse(df$case_prev == 1, 1, NA)))

# df$case_healthy_unhealthy <- ifelse(is.na(df$Age_he), NA, 
#                           ifelse(df$any_disease == 1 & df$case_prev %in% c(NA, 0), "unhealthy", 
#                                  ifelse(df$any_disease == 0 & df$case_prev %in% c(NA, 0), "healthy",
#                                         ifelse(df$case_prev == 1, "undiagnosed", NA))))

df$case_healthy_comorbid <- ifelse(is.na(df$Age_he), NA,
                                    ifelse(df$any_comorbidity == 1 & df$case_prev %in% c(NA, 0), "comorbid",
                                           ifelse(df$any_comorbidity == 0 & df$case_prev %in% c(NA, 0), "healthy",
                                                  ifelse(df$case_prev == 1, "undiagnosed", NA))))

df$case_prev_healthy <- ifelse(df$case_prev %in% 1, "undiagnosed",
                            ifelse(df$case_healthy_comorbid == "healthy", "healthy",
                                                           ifelse(df$case_prev == 0, "prevalent DM", NA)))

df$case_inc_healthy <- ifelse(df$case_inc %in% 1, "undiagnosed",
                               ifelse(df$case_healthy_comorbid == "healthy", "healthy",
                                      ifelse(df$case_inc == 0, "incident DM", NA)))

df$case_comorbid_ <- ifelse(df$case_prev %in% 1, "undiagnosed",
                               ifelse(df$case_healthy_comorbid == "comorbid", "comorbid", NA))

df$case_healthy_ <- ifelse(df$case_prev %in% 1, "undiagnosed",
                           ifelse(df$case_healthy_comorbid == "healthy", "healthy", NA))


# bmi categories ----
df$bmi_cat <- with(df, 
                   case_when(
                     BMI_recal < 20 ~ "< 20",
                     BMI_recal >= 20 & BMI_recal < 23 ~ "20-23",
                     BMI_recal >= 23 & BMI_recal < 25 ~ "23-25",
                     BMI_recal >= 25 & BMI_recal < 27.5 ~ "25-27.5",
                     BMI_recal >= 27.5 & BMI_recal < 30 ~ "27.5-30",
                     BMI_recal >= 30  ~ ">=30")) 
df$bmi_cat <- factor(df$bmi_cat, levels = c("< 20", "20-23", "23-25", "25-27.5", "27.5-30", ">=30"))

# create variables ----
df$age_cat_ <- recode_age(df$age, age_labels = c("0-34", "35-44", "45-54", "55-64", "65-74", "75+"))
df$age_cat_ <- car::recode(df$age_cat_, " '0-34' = '15-34' ") # min age for PHS survey is 15

df$married <- ifelse(df$marital %in% "Married", 1, 0)
df$public_hse <- ifelse(df$hse_type %in% "Public rental housing", 1, 0)

# grouping educ categories ----
df$educ <- car::recode(df$educ, "
'No schooling/Pre-primary' = 'Primary'
")
df$educ <- factor(df$educ, levels = c("Primary", "Secondary", "Post-secondary or above"))

# # save data ----
# save(df, file = "t2dm_data.RData")
# haven::write_sav(df, "t2dm_data.sav")

# multinomial logistic ----
library(nnet)
library(stargazer)
# library(mlogit)

df$case_inc_norm_ <- relevel(as.factor(df$case_inc_norm_), ref = "Normal")
# df$case_inc_norm_ <- relevel(as.factor(df$case_inc_norm_), ref = "Incident")

df$case_prev_norm_ <- relevel(as.factor(df$case_prev_norm_), ref = "Normal")
# df$case_prev_norm_ <- relevel(as.factor(df$case_prev_norm_), ref = "Prevalent")

temp <- df
# temp <- temp[temp$male == 1, ]
multinom_model <- multinom(case_inc_norm_ ~ age_cat_
                           # + I(age^2)
                           + male
                           # + Waist_circum
                           # + WHR
                           # + as.factor(bmi_cat)
                           # + BMI_recal
                           + health
                           + who_pa
                           + hypertension
                           + I(smoking=="Currently smoke")
                           # + hse_type+educ+married+econ_active+I(income_num/1000)+checkup
                           + hse_type+educ+married*male+econ_active*male+I(income_num/1000)*male+checkup*male
                           # + MET_week_con
                           # + MET_work + MET_trans + MET_recr
                           # + pa_work_prop + pa_trans_prop + pa_recr_prop
                           + pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod
                           + I(sed_time_perday/60)
                           # + pa_vig + pa_mod
                           , data = temp) 

multinom_model %>%
  stargazer(type="text", style = "default",
            apply.coef = exp,
            star.cutoffs = c(0.05, 0.01, 0.001),
            report=('vc*p'),
            ci = TRUE,
            t.auto=FALSE, p.auto=FALSE,
            add.lines = list(c("n", nrow(multinom_model$residuals), nrow(multinom_model$residuals)))) %>% clipr::write_clip()

glm_model <- glm(I(case_norm_=="Undiagnosed")~1+age_cat_
                 # +I(age^2)
                 +male
                 # + scale(Waist_circum)
                 # + scale(WHR)
                 # + as.factor(bmi_cat)
                 # + scale(BMI_recal)
                 # + hse_type+educ+married+econ_active+I(income_num/1000)+checkup
                 + hse_type+educ+married*male+econ_active*male+I(income_num/1000)*male+checkup*male
                 +health+who_pa+ hypertension+ I(smoking=="Currently smoke")
                 # + MET_work + MET_trans + MET_recr
                 # + pa_work_prop + pa_trans_prop + pa_recr_prop
                 +pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod
                 + I(sed_time_perday/60)
                 , family = binomial, data =  temp)

glm_model1 <- glm(I(case_norm_=="Undiagnosed")~1+age_cat_
                  # +I(age^2)
                 + male
                 + Waist_circum
                 + WHR
                 # + as.factor(bmi_cat)
                 + BMI_recal
                 # + hse_type+educ+married+econ_active+I(income_num/1000)+checkup
                 + hse_type+educ+married*male+econ_active*male+I(income_num/1000)*male+checkup*male
                 +health+who_pa+ hypertension+ I(smoking=="Currently smoke")
                 # + MET_work + MET_trans + MET_recr
                 # + pa_work_prop + pa_trans_prop + pa_recr_prop
                 +pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod
                 + I(sed_time_perday/60)
                 , family = binomial, data =  temp)

glm_model2 <- glm(case_inc~1+age_cat_
                  # +I(age^2)
                  +male
                  # + hse_type+educ+married+econ_active+I(income_num/1000)+checkup
                 + hse_type+educ+married*male+econ_active*male+I(income_num/1000)*male+checkup*male
                 # + live.alone
                 + health+who_pa+ hypertension+ I(smoking=="Currently smoke")
                 # + MET_work + MET_trans + MET_recr
                 # + pa_work_prop + pa_trans_prop + pa_recr_prop
                 +pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod
                 + I(sed_time_perday/60)
                 , family = binomial, data =  temp)

glm_model3 <- glm(case_prev~1+age_cat_
                  # +I(age^2)
                  +male
                  # + hse_type+educ+married+econ_active+I(income_num/1000)+checkup
                  + hse_type+educ+married*male+econ_active*male+I(income_num/1000)*male+checkup*male
                  # + live.alone
                  + health+who_pa+ hypertension+ I(smoking=="Currently smoke")
                  # + MET_work + MET_trans + MET_recr
                  # + pa_work_prop + pa_trans_prop + pa_recr_prop
                  +pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod
                  + I(sed_time_perday/60)
                  , family = binomial, data =  temp)

table <- combine_tables(NULL, 
                        exponentiate = TRUE,
                        decimal_places = 3,
                        show_CI = T,
                        # dep_var = paste0(outcome),
                        multinom_model,
                        update(multinom_model, case_prev_norm_ ~ . )
                        # , glm_model
                        # , glm_model1
                        , glm_model2
                        , glm_model3
)
table %>% clipr::write_clip()

glm(health ~ age
    # + I(age ^ 2)
    + male
    + scale(Waist_circum)
    + scale(WHR)
    + scale(BMI_recal)
    + health+who_pa
    + hypertension
    + I(smoking=="Currently smoke")
    # + hse_type+educ+marital+econ_active+I(income_num/1000)+checkup_freq
    # + MET_week_con
    # + MET_work + MET_trans + MET_recr
    # + pa_work_prop + pa_trans_prop + pa_recr_prop
    + pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod + pa_vig + pa_mod + sed_time_perday
    , family = gaussian, data = df) %>%
  stargazer(type="text", style = "default",  apply.coef = exp, ci = TRUE, t.auto=FALSE, p.auto=FALSE, nobs = TRUE)

# Hausman-McFadden test ----
m1 <- mlogit::mlogit(case_inc_norm_ ~ 0 | age_cat_
                     + male
                     + health
                     + who_pa
                     + hypertension
                     + I(smoking=="Currently smoke")
                     + hse_type+educ+married*male+econ_active*male+I(income_num/1000)*male+checkup*male
                     + pa_work_vig + pa_work_mod + pa_trans + pa_recr_vig + pa_recr_mod
                     + I(sed_time_perday/60)
                     , shape =  "wide", data =  temp, reflevel="Normal")
mlogit::hmftest(m1, update(m1, alt.subset=c("Normal", "Incident")))
mlogit::hmftest(m1, update(m1, alt.subset=c("Normal", "Undiagnosed")))
mlogit::hmftest(update(m1, reflevel="Incident"), update(m1, reflevel="Incident", alt.subset=c("Incident", "Undiagnosed")))

# generate descriptive statistics ----
df$current_smoker <- ifelse(df$smoking %in% "Currently smoke", 1, 0)

allVars <- c("age", "age_cat_", "sex",
             "health", "who_pa", "hypertension", "current_smoker", "checkup", 
             # "checkup_freq", 
             "hse_type",  "educ", "married",
             # "marital", "econ", "occup",
             "econ_active",  "income_num", "income_cat6",
             # , "district", "hse_income", "live.alone", "Family.history.DM"
             # , "family_doctor", "BMI_recal"
             "pa_work_vig", "pa_work_mod", "pa_trans", "pa_recr_vig", "pa_recr_mod", "sed_time_perday"
             )
catVars <- c("age_cat_", "sex", "educ", "marital", "econ", "occup", "income_cat6", "district", "hse_income", "hse_type", "checkup_freq", "HT_prev", "hyperchol_prev",
             "who_pa", "hypertension", "current_smoker", "checkup", "econ_active")

allVars <- c(allVars , 
             "MET_week_con", "MET_work", "MET_trans", "MET_recr",
             "pa_work_prop", "pa_trans_prop", "pa_recr_prop", "pa_vig", "pa_mod"
             # , "hse_income"
)

temp <- df[complete.cases(df[, c(allVars)]),]
# temp <- temp %>% filter(case_inc_norm_ %!in% "Undiagnosed")
gen_desc(temp, vars = allVars, nominalVars = catVars, medianVars = NULL, group = "case_inc_norm_") %>% clipr::write_clip()

# charts by income & housing ----
setwd(sprintf("~%s/sarcopenia", setpath))
import_func("sar_analysis.R")
setwd(sprintf("~%s/t2dm", setpath))

varlist <- t(array(c(c("case_inc_norm_", "case_inc_norm_"), 
                     c("hse_type", "hse_type"), 
                     c("income_num", "income_num")
),
dim = c(2,3))) %>% as.data.frame()

library(ggplot2)
get_plot(df[df$case_inc_norm_ %!in% NA, ], 
         x = "case_inc_norm_", y = "hse_type", 
         jitter_w = 0.25, jitter_h = 0.25)

get_plot(df[df$case_inc_norm_ %!in% NA, ], 
         x = "income_num", y = "HbA1c", 
         jitter_w = 0.25, jitter_h = 0.25)

# causal diagram ----
dagitty::dagitty('
dag {
bb="-3.484,-2.265,3.202,2.885"
"BMI/WC/WHR" [pos="-0.571,0.542"]
"self-rated health" [adjusted,pos="0.690,1.333"]
"undiagnosed DM" [outcome,pos="1.705,-0.031"]
HT [adjusted,pos="-1.076,-1.331"]
age [adjusted,pos="-2.271,-0.690"]
pa_recr_vig [adjusted,pos="0.284,-2.059"]
pa_work_vig [adjusted,pos="-0.627,-2.059"]
sex [adjusted,pos="-1.487,1.333"]
smoking [adjusted,pos="-1.982,0.256"]
who_pa [adjusted,pos="1.280,-1.709"]
working_class [latent,pos="-1.794,-1.589"]
"BMI/WC/WHR" -> "undiagnosed DM"
"BMI/WC/WHR" <-> HT
"self-rated health" -> "undiagnosed DM"
HT -> "self-rated health"
HT -> "undiagnosed DM"
HT <-> smoking
age -> "BMI/WC/WHR"
age -> "undiagnosed DM"
age -> HT
age -> working_class
pa_recr_vig -> "BMI/WC/WHR"
pa_recr_vig -> "self-rated health"
pa_recr_vig -> "undiagnosed DM"
pa_recr_vig -> who_pa
pa_work_vig -> "BMI/WC/WHR"
pa_work_vig -> "self-rated health"
pa_work_vig -> "undiagnosed DM"
pa_work_vig -> who_pa
sex -> "BMI/WC/WHR"
sex -> "self-rated health"
sex -> "undiagnosed DM"
sex -> HT
sex -> pa_recr_vig
sex -> pa_work_vig
sex -> smoking
smoking -> "BMI/WC/WHR"
smoking -> "undiagnosed DM"
who_pa -> "BMI/WC/WHR"
who_pa -> "self-rated health"
who_pa -> "undiagnosed DM"
who_pa -> HT
working_class -> "self-rated health"
working_class -> pa_recr_vig
working_class -> pa_work_vig
working_class -> smoking
working_class -> who_pa
}
')  %>% plot
# is-lasso ----
# dep_var <- "case_prev"

df$case_healthy <-ifelse(df$case_healthy_ == "healthy", 0, 1)
dep_var <- "case_prev"
xvars <- c("bmi_cat", allVars, "Systolic_BP", "Diastolic_BP", "hypertension")

# islasso
temp <- df[, c(dep_var, xvars)]
temp <- na.omit(temp)
set.seed(27121)
fit <- islasso(as.formula(paste0(dep_var, " ~ .")), data=temp)

# table <- data.frame()
table_temp <- summary(fit) %>% coef() %>% as.data.frame()
table_temp <- table_temp[table_temp$`Pr(>|z|)` < 0.9 | row.names(table_temp) %in% "(Intercept)",] # drop estimates if p-value >= 0.1, except for intercept
table_temp$Estimate <- starred_p(table_temp$`Pr(>|z|)`, decimal_places = 3, table_temp$Estimate)
table_temp$variables <- row.names(table_temp)
table_temp <- rbind(NA, table_temp)
table_temp$variables[table_temp$variables %in% NA] <- "N"
table_temp$Estimate[table_temp$variables == "N"] <-  fit$internal$n
table_temp <- rbind(NA, table_temp)
table_temp$variables[table_temp$variables %in% NA] <- "R2"
table_temp$Estimate[table_temp$variables == "R2"] <-  round(1 - ((fit$deviance/-2)) / (fit$null.deviance/-2) , 3) # 1- sum((fit$fitted.values - fit$data$y) ^ 2)/sum((fit$data$y - mean(fit$data$y)) ^ 2)  or  cor(fit$fitted.values, fit$data$y)^2 
table_temp <- table_temp %>% select(variables, Estimate)
# table <- merge(table, table_temp, all = TRUE)
# table <- rbind(table[table$X1=="N",], table[table$X1=="R2",], table[table$X1=="(Intercept)",], table[table$X1 %!in% c("N", "R2", "(Intercept)"),])


# DM risk scores ----
risk_scoring <- function(df, type = "NDS"){
  if (type == "NDS"){
    score_age <- with(df, 
                      case_when(
                        age <= 34 ~ 0, # age >= 25 (original)
                        age >= 35 & age <= 44 ~ 8,
                        age >= 45 & age <= 54 ~ 13,
                        age >= 55 & age <= 64 ~ 20,
                        age >= 65  ~ 22)) # & age <= 74 (original)
    score_bmi <- with(df, 
                      case_when(
                        BMI_recal < 20 ~ -6, # BMI_recal >= 18 (original)
                        BMI_recal >= 20 & BMI_recal < 23 ~ 0,
                        BMI_recal >= 23 & BMI_recal < 25 ~ 9,
                        BMI_recal >= 25 & BMI_recal < 27.5 ~ 14,
                        BMI_recal >= 27.5 & BMI_recal < 30 ~ 18,
                        BMI_recal >= 30  ~ 20)) 
    
    score_ht <- ifelse(df$hypertension %in% 1, 8, 0)
    
    score <- score_age+score_bmi+score_ht
  }
  
  if (type == "ADA"){
    score_age <- with(df, 
                      case_when(
                        age < 40 ~ 0, 
                        age >= 40 & age <= 49 ~ 1,
                        age >= 50 & age <= 59 ~ 2,
                        age >= 60  ~ 3)) 
    
    score_sex <- with(df, 
                      case_when(
                        sex %in% "Female" ~ 0, 
                        sex %in% "Male" ~ 1)) 
    
    score_famhx <- 0
    # with(df, 
    #                 case_when(
    #                   Family.history.DM %in% 1 ~ 1, 
    #                   Family.history.DM %in% 0 ~ 0)) 
    
    score_ht <- ifelse(df$hypertension %in% 1, 1, 0)
    
    score_bmi <- with(df, 
                      case_when(
                        BMI_recal < 23 ~ 0,
                        BMI_recal >= 23 & BMI_recal < 25 ~ 1,
                        BMI_recal >= 25 & BMI_recal < 40 ~ 2,
                        BMI_recal >= 40  ~ 3)) 
    
    score_active <- ifelse(df$who_pa %in% 1, -1, 0)
    
    score <- score_age+score_sex+score_famhx+score_bmi+score_ht+score_active
    score <- round(0+((score-0)*(9-0)/(8-0)), 1) # feature scale it from [0-51] to [0-41], when without family history of DM
  }
  
  if (type == "NCDRS"){
    score_age <- with(df, 
                      case_when(
                        age <= 24 ~ 0, # age >= 20 (original)
                        age >= 25 & age <= 34 ~ 4,
                        age >= 35 & age <= 39 ~ 8,
                        age >= 40 & age <= 44 ~ 11,
                        age >= 45 & age <= 49 ~ 12,
                        age >= 50 & age <= 54 ~ 13,
                        age >= 55 & age <= 59 ~ 15,
                        age >= 60 & age <= 64 ~ 16,
                        age >= 65  ~ 18)) # & age <= 84 (original)
    
    score_bmi <- with(df, 
                      case_when(
                        BMI_recal < 22 ~ 0,
                        BMI_recal >= 22 & BMI_recal < 24 ~ 1,
                        BMI_recal >= 24 & BMI_recal < 30 ~ 3,
                        BMI_recal >= 30  ~ 5)) 
    
    score_sex <- with(df, 
                      case_when(
                        sex %in% "Female" ~ 0, 
                        sex %in% "Male" ~ 2)) 
    
    score_waist <- 0 # replace it with waist circumference
    
    score_sbp <- with(df, 
                      case_when(
                        Systolic_BP < 110 ~ 0,
                        Systolic_BP >= 110 & Systolic_BP < 120 ~ 1,
                        Systolic_BP >= 120 & Systolic_BP < 130 ~ 3,
                        Systolic_BP >= 130 & Systolic_BP < 140 ~ 6,
                        Systolic_BP >= 140 & Systolic_BP < 150 ~ 7,
                        Systolic_BP >= 150 & Systolic_BP < 160 ~ 8,
                        Systolic_BP >= 160  ~ 10)) 
    
    score_famhx <- 0
    # with(df, 
    #                   case_when(
    #                     Family.history.DM %in% 1 ~ 6, 
    #                     Family.history.DM %in% 0 ~ 0)) 
    
    score <- score_age+score_bmi+score_sex+score_waist+score_sbp+score_famhx
    # score <- round(0+((score-0)*(51-0)/(41-0)), 1) # feature scale it from [0-41] to [0-51], when without waist circumference
    score <- round(0+((score-0)*(51-0)/(35-0)), 1) # feature scale it from [0-35] to [0-51], when without family hx and waist 
  }
  return(score)
}

df$score_NDS <- risk_scoring(df, type = "NDS")
df$score_NDS_ <- ifelse(df$score_NDS >= 28, 1, 0)

df$score_ADA <- risk_scoring(df, type = "ADA")
df$score_ADA_ <- ifelse(df$score_ADA >= 5, 1, 0)

df$score_NCDRS <- risk_scoring(df, type = "NCDRS")
df$score_NCDRS_ <- ifelse(df$score_NCDRS >= 25, 1, 0)

var <- "score_NCDRS"
var2 <- ""
gen_desc(df, vars = var, nominalVars = var2, group = "case_healthy") 
gen_desc(df, vars = var, nominalVars = var2, group = "case_comorbid")
gen_desc(df, vars = var, nominalVars = var2, group = "case_prev") 
gen_desc(df, vars = var, nominalVars = var2, group = "case_inc")

# exploratory analysis ----
table <- combine_tables(NULL, exponentiate = TRUE,
                        lm(pa_recr_vig~1+pa_recr_mod, data = df[df$case_prev_norm_ %!in% NA,]),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male+BMI_recal, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male+I(BMI_recal>23), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male+I(BMI_recal>30), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male+BMI_recal+hypertension, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male+BMI_recal+hypertension++I(smoking=="Currently smoke"), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_prev_norm_+age+I(age^2)+male+BMI_recal+hypertension+I(smoking=="Currently smoke")+hse_type, data =  df),
                        
                        lm(pa_recr_mod~1+pa_recr_vig, data = df[df$case_prev_norm_ %!in% NA,]),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male+BMI_recal, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male+I(BMI_recal>23), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male+I(BMI_recal>30), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male+BMI_recal+hypertension, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male+BMI_recal+hypertension++I(smoking=="Currently smoke"), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_prev_norm_+age+I(age^2)+male+BMI_recal+hypertension+I(smoking=="Currently smoke")+hse_type, data =  df)
)

table <- combine_tables(NULL, exponentiate = TRUE,
                        lm(pa_recr_vig~1+pa_recr_mod, data = df[df$case_inc_norm_ %!in% NA,]),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male+BMI_recal, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male+I(BMI_recal>23), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male+I(BMI_recal>30), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male+BMI_recal+hypertension, data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male+BMI_recal+hypertension++I(smoking=="Currently smoke"), data =  df),
                        lm(pa_recr_vig~1+pa_recr_mod+case_inc_norm_+age+I(age^2)+male+BMI_recal+hypertension+I(smoking=="Currently smoke")+hse_type, data =  df),
                        
                        lm(pa_recr_mod~1+pa_recr_vig, data = df[df$case_inc_norm_ %!in% NA,]),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male+BMI_recal, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male+I(BMI_recal>23), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male+I(BMI_recal>30), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male+BMI_recal+hypertension, data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male+BMI_recal+hypertension++I(smoking=="Currently smoke"), data =  df),
                        lm(pa_recr_mod~1+pa_recr_vig+case_inc_norm_+age+I(age^2)+male+BMI_recal+hypertension+I(smoking=="Currently smoke")+hse_type, data =  df)
)
                        

table <- combine_tables(NULL, exponentiate = TRUE,
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+I((pa_trans+pa_work_vig+pa_work_mod)>150), family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+pa_work_mod, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+pa_trans, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+pa_recr_vig, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+pa_recr_mod, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+sed_time_perday, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+checkup+pa_work_vig+pa_work_mod+pa_trans+pa_recr_vig+pa_recr_mod+sed_time_perday, family = binomial, data =  df)
)

glm(case_inc~1+marital, family = binomial, data =  df) %>% summary

table <- combine_tables(NULL, exponentiate = TRUE,
               glm(case_inc~1+age+male+I(MET_trans+MET_work+MET_recr), family = binomial, data =  df),
               glm(case_inc~1+age+male+who_pa, family = binomial, data =  df),
               glm(case_inc~1+age+male+I(pa_trans+pa_work_vig+pa_work_mod), family = binomial, data =  df),
               glm(case_inc~1+age+male+pa_recr_vig, family = binomial, data =  df),
               glm(case_inc~1+age+male+pa_recr_mod, family = binomial, data =  df),
               glm(case_inc~1+age+male+sed_time_perday, family = binomial, data =  df),
               glm(case_inc~1+age+male+pa_work_vig+pa_work_mod+pa_trans+pa_recr_vig+pa_recr_mod+sed_time_perday, family = binomial, data =  df)
               
               
)

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_prev~1, family = binomial, data =  df),
                        glm(case_prev~1+age, family = binomial, data =  df),
                        glm(case_prev~1+age+male, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_prev~1+age+male+educ+marital+econ_active+live.alone+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_prev~1+age+male+educ+marital+econ_active+live.alone+Family.history.DM+I(income_num/1000), family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health, family = binomial, data =  df), 
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+who_pa, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup_freq, family = binomial, data =  df)
)
table %>% clipr::write_clip()

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_prev~1, family = binomial, data =  df),
                        glm(case_prev~1+age, family = binomial, data =  df),
                        glm(case_prev~1+age+male, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+live.alone+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+live.alone+Family.history.DM+I(income_num/1000), family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health, family = binomial, data =  df), 
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health+who_pa, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup_freq, family = binomial, data =  df)
)

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_inc~1, family = binomial, data =  df),
                        glm(case_inc~1+age, family = binomial, data =  df),
                        glm(case_inc~1+age+male, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_inc~1+age+male+educ+marital+econ_active+live.alone+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_inc~1+age+male+educ+marital+econ_active+live.alone+Family.history.DM+I(income_num/1000), family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+health, family = binomial, data =  df), 
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+who_pa, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup_freq, family = binomial, data =  df)
)

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_inc~1, family = binomial, data =  df),
                        glm(case_inc~1+age, family = binomial, data =  df),
                        glm(case_inc~1+age+male, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+live.alone+I(income_num/1000), family = binomial, data =  df),
                        # glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+live.alone+Family.history.DM+I(income_num/1000), family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health, family = binomial, data =  df), 
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health+who_pa, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HTcase_prev_norm_+cholcase_prev_norm_+educ+marital+econ_active+I(income_num/1000)+health+who_pa+checkup_freq, family = binomial, data =  df)
)

glm(case_prev~1+age+male+educ+I(income_num/1000)+cholcase_prev_norm_2, family = binomial, data =  df) %>% summary()
glm(case_prev~1+cholcase_prev_norm_, family = binomial, data =  df) %>% summary()

explore_var <- function(df){
  table <- data.frame(matrix(ncol = 2,  nrow = 0))
  row_count <- 1
  col_count <- 2
  for (i in which(names(df)=="Q26a"):which(names(df)=="Q39b5")){ # 
    var <- names(df)[i]
    table[row_count, 1] <- var
    if (length(unique(df[, var]))>3){
      row_count <- row_count + 1
      next 
    }
    df[, var] <- ifelse(df[, var] == 1, 1, 0)
    eval_("fit <- glm(case_prev~1+age_cat+male+", var,", family = binomial, data =  df) %>% summary()")
    beta <- iferror(exp(fit$coefficients[var, 1]), NA)
    p_value <- iferror(fit$coefficients[var, 4], NA)
    table[row_count, col_count] <-  starred_p(p_value, 4, beta)
    row_count <- row_count + 1
  }
  return(table)
}
table <- explore_var(df)

# descriptive statistics ----
# create new case-control variable
allVars <- c("age", "age_cat", "sex", "educ", "marital", "econ", "econ_active", "occup", "income_num", "income_cat3", "district", "hse_income", "hse_type", 
             "live.alone", "Family.history.DM", "health", "who_pa", "checkup", "checkup_freq", "family_doctor", "BMI_recal")
catVars <- c("age_cat", "sex", "educ", "marital", "econ", "occup", "income_cat3", "district", "hse_income", "hse_type", "checkup_freq", "HT_prev", "hyperchol_prev")

df$occup[!is.na(df$case_inc)]  <- drop_unused_value_labels(df$occup[!is.na(df$case_inc)] )

allVars <- "occup"
tableone::CreateTableOne(data =  df, strata = "case_inc", vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("MET_week_con", "MET_work", "MET_trans", "MET_recr", 
             "pa_work_prop", "pa_trans_prop", "pa_recr_prop", 
             "pa_work_vig", "pa_work_mod", "pa_trans", "pa_recr_vig", "pa_recr_mod", "pa_vig", "pa_mod", "sed_time_perday",
             "who_pa")
tableone::CreateTableOne(data =  df, strata = "case_inc", vars = allVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
