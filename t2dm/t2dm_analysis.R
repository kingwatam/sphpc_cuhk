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
df$health <- (as.numeric(df$Q10)-6)*-1 # reverse self rate health from lower as better to lower as worse
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

df$case_inc <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & (df$Q35a != 1 & df$Q35 != 1), 1, 
                       ifelse(df$Q35a ==  1, 0, NA)) # incident cases as control

df$case_prev <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & df$Q35 != 1, 1, 
                        ifelse(df$Q35 ==  1, 0, NA)) # prevalent cases as control

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


df$pa_work_prop <- df$MET_work/df$MET_week_con
df$pa_recr_prop <- df$MET_recr/df$MET_week_con
df$pa_trans_prop <- df$MET_trans/df$MET_week_con
df$pa_work_trans_prop <- df$MET_work_trans/df$MET_week_con

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

df$case_comorbid <- ifelse(df$case_prev %in% 1, "undiagnosed",
                               ifelse(df$case_healthy_comorbid == "comorbid", "comorbid", NA))

# generate descriptive statistics ----
gen_desc <- function(data, vars, ordinalVars, medianVars, group = "time", show_NA = FALSE, both_tests = FALSE){ 
  categories <- unique(df[[group]]) 
  if (!show_NA){
    categories <- categories[categories %!in% NA]
  }
  
  table <- data.frame(matrix(ncol = (2 + length(categories) + 1),  nrow = 0))
  
  row_count <- 1 
  col_label <- 2
  
  colnames(table)[1] <- ""
  colnames(table)[col_label] <- ""
  
  for (i in (1:length(categories))){
    assign(paste0("col_", i), col_label + i)
    table[row_count, get_("col_", i)] <- categories[i]
  }
  
  table[row_count, 1] <- group
  
  col_pval <- col_label + length(categories) + 1
  colnames(table) <- rep("", ncol(table))
  colnames(table)[col_pval] <- "p-value"
  
  row_count <- row_count + 1
  
  table[row_count, 1] <- "N"
  table[row_count, col_label] <- ""

  
  for (i in (1:length(categories))){
    table[row_count, get_("col_", i)] <- length(data[[group]][data[[group]] %in% categories[i]])
  }
  
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
    for (i in (1:length(categories))){
      category <- categories[i]
      
      # if (class(df[[var]]) == "character") next()
      # if (all(df[[var]][which(df[[group]] %in% category)] %in% NA)) next()
      
      if (var %in% ordinalVars){

        table[row_count, 1] <- var
        
        p_value <- ifwarning(chisq.test(x = df[[group]], y = df[[var]])[['p.value']], chisq.test(x = df[[group]], y = df[[var]], simulate.p.value = TRUE, B=3000)[['p.value']])
        if (p_value < 0.001){
          table[row_count, col_pval] <- "<0.001"
        } else {
          table[row_count, col_pval] <- p_value %>% round(digits = 3)
        }

        row_count_ <- row_count # save count
        total <- length(df[[var]][which(df[[group]] %in% category)])
        
        for (val in unique(df[[var]])[order(unique(df[[var]]))]){
          table[row_count, col_label] <- val
          
          if (val %in% NA){
            table[row_count, col_label] <- "N/A"
          }
          
          n <-  length(df[[var]][which(df[[var]] %in% val & df[[group]] %in% category)])
          
          table[row_count, get_("col_", i)] <- paste0(n, " (", round_format(n/total*100, 1), "%)")
          
          if(n > 0) {row_count <- row_count + 1} 
        }
        
        row_count__ <- row_count
        row_count <- row_count_ # reset count
      } 
      
      if (var %!in% ordinalVars){

        table[row_count, 1] <- var
        
        anova.test <- aov(formula(paste0(var, " ~ ", group)),  data = df) %>% summary()
        p_value <- anova.test[[1]][["Pr(>F)"]][[1]]
        if (p_value < 0.001){
          table[row_count, col_pval] <- "<0.001"
        } else {
          table[row_count, col_pval] <- p_value %>% round(digits = 3)
        }
        
        table[row_count, col_label] <- "mean (sd)"
        table[row_count, get_("col_", i)] <-  get_mean_sd(df[[var]][which(df[[group]] %in% category)])
        
      }
      
    }
    
    row_count <- iferror(row_count__, row_count + 1)
    ifwarning(rm(row_count__), NA)
  }
  return(table)
}

allVars <- c("age", "age_cat", "sex", "educ", "marital", "econ", "econ_active", "occup", "income_num", "income_cat3", "district", "hse_income", "hse_type", 
             # "live.alone", "Family.history.DM", 
             "health", "who_pa", "checkup", "checkup_freq", "family_doctor", "BMI_recal")
catVars <- c("age_cat", "sex", "educ", "marital", "econ", "occup", "income_cat3", "district", "hse_income", "hse_type", "checkup_freq", "HT_prev", "hyperchol_prev")

allVars <- c(allVars , "MET_week_con", "MET_work", "MET_trans", "MET_recr", 
             "pa_work_prop", "pa_trans_prop", "pa_recr_prop", 
             "pa_work_vig", "pa_work_mod", "pa_trans", "pa_recr_vig", "pa_recr_mod", "pa_vig", "pa_mod", "sed_time_perday"
             )

df$case_inc_ <- ifelse(df$case_inc %in% 1, "undiagnosed",
                       ifelse(df$case_inc %in% 0, "incident DM", NA))
df$case_prev_ <- ifelse(df$case_prev %in% 1, "undiagnosed",
                       ifelse(df$case_prev %in% 0, "prevalent DM", NA))

gen_desc(df, vars = allVars, ordinalVars = catVars, medianVars = NULL, group = "case_healthy") %>% clipr::write_clip()

# exploratory analysis ----
table <- combine_tables(NULL, exponentiate = TRUE,
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+I((pa_trans+pa_work_vig+pa_work_mod)>150), family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+pa_work_mod, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+pa_trans, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+pa_recr_vig, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+pa_recr_mod, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+sed_time_perday, family = binomial, data =  df),
               glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+checkup+pa_work_vig+pa_work_mod+pa_trans+pa_recr_vig+pa_recr_mod+sed_time_perday, family = binomial, data =  df)
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
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health, family = binomial, data =  df), 
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_prev~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup_freq, family = binomial, data =  df)
)
table %>% clipr::write_clip()

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_prev~1, family = binomial, data =  df),
                        glm(case_prev~1+age, family = binomial, data =  df),
                        glm(case_prev~1+age+male, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health, family = binomial, data =  df), 
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_prev~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup_freq, family = binomial, data =  df)
)

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_inc~1, family = binomial, data =  df),
                        glm(case_inc~1+age, family = binomial, data =  df),
                        glm(case_inc~1+age+male, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health, family = binomial, data =  df), 
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_inc~1+age+male+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup_freq, family = binomial, data =  df)
)

table <- combine_tables(NULL, exponentiate = TRUE,
                        glm(case_inc~1, family = binomial, data =  df),
                        glm(case_inc~1+age, family = binomial, data =  df),
                        glm(case_inc~1+age+male, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000), family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health, family = binomial, data =  df), 
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup, family = binomial, data =  df),
                        glm(case_inc~1+age+male+HT_prev+hyperchol_prev+educ+marital+econ_active+I(income_num/1000)+live.alone+Family.history.DM+health+who_pa+checkup_freq, family = binomial, data =  df)
)

glm(case_prev~1+age+male+educ+I(income_num/1000)+live.alone+Family.history.DM+hyperchol_prev2, family = binomial, data =  df) %>% summary()
glm(case_prev~1+hyperchol_prev, family = binomial, data =  df) %>% summary()

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
