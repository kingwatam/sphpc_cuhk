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


df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 20, interval = 5, last_group = 75)
df$age_group <- car::recode(df$age_group, " '0-19' = '15-19' ") # min age for PHS survey is 15
df$age_cat <- recode_age(df$age, age_labels = NULL, second_group = 50, interval = 15, last_group = 80)
df$age_cat <- car::recode(df$age_cat, " '0-49' = '15-49' ") # min age for PHS survey is 15

df$female <- ifelse(df$sex == "Female", 1, 0)
df$male <- ifelse(df$sex == "Male", 1, 0)

df$HbA1c <- ifelse(df$HbA1c == 333, 3.8, df$HbA1c) # less than 3.8% is censored 

df$case_inc <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & df$Q35a != 1, 1, 
                       ifelse(df$Q35a ==  1, 0, NA)) # incident cases as control

df$case_prev <- ifelse( (df$HbA1c >= 6.5 | df$FPG >= 7) & df$Q35 != 1, 1, 
                        ifelse(df$Q35 ==  1, 0, NA)) # prevalent cases as control

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


# main analysis ----
gen_table <- function(fit, exponentiate = FALSE){
  table <- data.frame(matrix(ncol = 2,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  dep_var <- as.character(formula(fit)[2])
  
  if (!is.null(summary(fit)$isLmer)){
    n <- iferror(nobs(fit), NA)
    n_unique <- iferror(summary(fit)$ngrps, NA)
    table[row_count, 1] <- "N (unique)"
    table[row_count, col_count] <-  paste0(n, " (", n_unique, ")")
    row_count <- row_count + 1
    
  } else {
    n <- iferror(nobs(fit), NA)
    table[row_count, 1] <- "N"
    table[row_count, col_count] <-  n
    row_count <- row_count + 1
    
  }
  
  for (var in row.names(summary(fit)$coef)){
    
    if (!(var %in% unlist(table[1]))){
      table[row_count, 1] <- var
      if (grepl("age_group.", var, fixed = TRUE)){
        var_name <- substr(var, nchar("age_group.")+2, nchar(var))
        table[row_count, 1] <- var_name
      } else if (grepl("age_group", var, fixed = TRUE)){
        var_name <- substr(var, nchar("age_group")+1, nchar(var))
        table[row_count, 1] <- var_name
      }
    } else {
      row_count <- match(var, unlist(table[1]))
    }
    
    print(paste(dep_var, var))
    
    colnames(table)[col_count] <- dep_var
    
    if (exponentiate){
      beta <- iferror(exp(summary(fit)$coef[var, 1]), NA)
    } else {
      beta <- iferror(summary(fit)$coef[var, 1], NA)
      se <-  iferror(summary(fit)$coef[var, 2], NA)
      lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
      upperCI <- iferror(beta + qnorm(0.975) * se, NA)
    }
    if (!is.null(summary(fit)$isLmer)){
      p_value <- iferror(summary(fit)$coef[var, 5], NA)
    } else {
      p_value <- iferror(summary(fit)$coef[var, 4], NA)
    }
    
    # table[row_count, col_count] <-  paste0(n, ", ", starred_p(p_value, 3, beta))
    table[row_count, col_count] <-  starred_p(p_value, 4, beta)
    
    row_count <- row_count + 1
    
  }
  col_count <- col_count + 1
  return(table)
}

combind_tables <- function(table, exponentiate = FALSE, ...){
  for (i in 1:length(list(...))){
    new_table <- gen_table(list(...)[[i]], exponentiate) 
    dep_vars <- names(table)
    dep_var <- names(new_table)[2]
    names(table) <- c("X1",rep(2:ncol(table)))
    table <- plyr::join(table, new_table, by=c("X1"), type="full")
    names(table) <- c(dep_vars, dep_var)
    names(table)[names(table) == "X1"] <- ""
  }
  return(table)
}

df <- df[df$age <= 84,] # match age range of those who took health exam
df$econ_active <- ifelse(df$econ == "Economically active persons", 1, 0)

table <- gen_table(glm(case_prev~1, family = binomial, data =  df), exponentiate = TRUE)
table <- combind_tables(table, exponentiate = TRUE,
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

table <- gen_table(glm(case_prev~1, family = binomial, data =  df), exponentiate = TRUE)
table <- combind_tables(table, exponentiate = TRUE,
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

table <- gen_table(glm(case_inc~1, family = binomial, data =  df), exponentiate = TRUE)
table <- combind_tables(table, exponentiate = TRUE,
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

table <- gen_table(glm(case_inc~1, family = binomial, data =  df), exponentiate = TRUE)
table <- combind_tables(table, exponentiate = TRUE,
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
allVars <- c("age", "age_cat", "sex", "educ", "marital", "econ", "income_num", "income_cat3", "district", "hse_income", "hse_type", "live.alone", "Family.history.DM", "health", "who_pa", "checkup", "checkup_freq")
catVars <- c("age_cat", "sex", "educ", "marital", "econ", "income_cat3", "district", "hse_income", "hse_type", "checkup_freq")

tableone::CreateTableOne(data =  df, strata = "case_prev", vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()