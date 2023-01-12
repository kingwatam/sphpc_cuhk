rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(readxl)

# Sys.setlocale(locale =  "cht") # set locale to traditional Chinese
setwd(sprintf("~%s/health_literacy", setpath))

# survey data cleaning ----
var_names <- names(read_xlsx("Prof_Poon_Health_Literacy_Female_Breast_Test_22042022_king.xlsx", 
                             sheet  = "Prof_Poon_Health_Literacy_Demog", n_max = 0))

df <- readxl::read_xlsx("Prof_Poon_Health_Literacy_Female_Breast_Test_22042022_king.xlsx", sheet  = "Prof_Poon_Health_Literacy_Demog"
                  , col_names = TRUE, guess_max = 1000, col_types = ifelse(grepl("AGE", var_names), "text", "guess")) # numeric instead of date for age variable

df$AGE <- as.numeric(df$AGE)

# scoring <- function(df){
#   df %>% dplyr::select(starts_with("H_")) %>% colnames(.) -> HLS_SF12
#   # df %>% dplyr::select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
#   
#   df <- df %>%
#     mutate(
#       hls_sf12 = (rowMeans(.[HLS_SF12], na.rm = FALSE)-1)*50/3
#       )
#   return(subset(df, select = c(hls_sf12
#   )))
# }
# 
# df <- cbind(df, scoring(df))

df$age_cat <- recode_age(df$AGE, second_group = 50, interval = 5, last_group = 65)
df$age_cat <- car::recode(df$age_cat, " c('0-49') = NA ") # age range 50-74

names(df)[names(df)=="3C"] <- "educ"
names(df)[names(df)=="3D"] <- "marital"
names(df)[names(df)=="3E"] <- "econ"
names(df)[names(df)=="3F"] <- "hkborn"
names(df)[names(df)=="3G"] <- "income"
names(df)[names(df)=="3H"] <- "hse_income"
names(df)[names(df)=="3_I"] <- "health"

disease_vars <- c("DM", "LIVER", "HYPER", "LIPID", "IHD", "COAD", "STROKE", "CIRRHOSIS", "GERD", "COMOR_OTHER", "OWN_CA")

df$cd <- rowSums((df[, disease_vars]-2)*-1) # number of chronic diseases including cancer, 1=Yes, 2=No
for (var in disease_vars){
  df[[var]] <- car::recode(df[[var]], " 1 = 'Yes'; 2 = 'No' ")
  df[[var]] <- factor(df[[var]], levels =  c('Yes', 'No'))
}

df$cd_nocancer <- ifelse(df$OWN_CA %in% "Yes", df$cd-1, df$cd)

df$educ2 <- car::recode(df$educ, "
1:2 = 'Primary school or below';
3 = 'Secondary 1-3';
4:5 = 'Secondary 4-7';
6:7 = 'College/university or above'
")

df$educ2 <- factor(df$educ2, levels =  c('Primary school or below', 'Secondary 1-3', 'Secondary 4-7', 'College/university or above'))

df$educ <- car::recode(df$educ, "
1 = 'No formal schooling';
2 = 'Primary school';
3 = 'Secondary 1-3';
4:5 = 'Secondary 4-7';
6:7 = 'College/university or above'
")

df$educ <- factor(df$educ, levels =  c('No formal schooling', 'Primary school', 'Secondary 1-3', 'Secondary 4-7', 'College/university or above'))

df$marital <- car::recode(df$marital, "
c(1, 5)='Married/cohabitating'; 2='Unmarried'; 3='Separated/divorced'; 4='Widowed'
")

df$marital <- factor(df$marital, levels =  c('Married/cohabitating', 'Unmarried', 'Separated/divorced', 'Widowed'))

df$econ <- car::recode(df$econ, "
1='Full-time'; 2='Part-time'; 3='Retired'; 4='Student'; 5='Housewife'; 6='Unemployed'; 7='Self-employed'; 8=NA
")
df$econ <- factor(df$econ, levels =  c('Full-time', 'Part-time', 'Retired', 'Student', 'Housewife', 'Unemployed', 'Self-employed'))

df$hkborn <- car::recode(df$hkborn, "
1='Yes'; 2:3='No'
")
df$hkborn <- factor(df$hkborn, levels =  c('Yes', 'No'))

df$income2 <- car::recode(df$income, "
1:2='10,000 or below'; 3:4='10,001-20,000'; 5:6='20,001-30,000'; 7:9='30,001 or above'; 10=NA
")

df$income2 <- factor(df$income2, levels = c('10,000 or below', '10,001-20,000', '20,001-30,000', '30,001 or above'))

df$income3 <- car::recode(df$income, "
1='5,000 or below';2='5,001-10,000'; 3='10,001-15,000'; 4='15,001-20,000'; 5:6='20,001-30,000'; 7:8='30,001-40,000'; 9='40,001 or above'; 10=NA
")

df$income3 <- factor(df$income3, levels = c('5,000 or below','5,001-10,000', '10,001-15,000', '15,001-20,000', '20,001-30,000', '30,001-40,000', '40,001 or above'))


df$income <- car::recode(df$income, "
1='5,000 or below';2='5,001-10,000'; 3='10,001-15,000'; 4='15,001-20,000'; 5='20,001-25,000'; 
6='25,001-30,000'; 7='30,001-35,000'; 8='35,001-40,000'; 9='40,001 or above'; 10=NA
")

df$income <- factor(df$income, levels = c('5,000 or below','5,001-10,000', '10,001-15,000', '15,001-20,000', '20,001-25,000', 
                                          '25,001-30,000', '30,001-35,000', '35,001-40,000', '40,001 or above'))


df$hse_income2 <- car::recode(df$hse_income, "
1='10,000 or below'; 3:4='10,001-20,000'; 5:6='20,001-30,000'; 7:8='30,001-40,000'; 9='40,001 or above'; 10=NA
")

df$hse_income2 <- factor(df$hse_income2, levels = c('10,000 or below', '10,001-20,000', '20,001-30,000', 
                                                   '30,001-40,000', '40,001 or above'))


df$hse_income <- car::recode(df$hse_income, "
1='5,000 or below';2='5,001-10,000'; 3='10,001-15,000'; 4='15,001-20,000'; 5='20,001-25,000'; 
6='25,001-30,000'; 7='30,001-35,000'; 8='35,001-40,000'; 9='40,001 or above'; 10=NA
")

df$hse_income <- factor(df$hse_income, levels = c('5,000 or below','5,001-10,000', '10,001-15,000', '15,001-20,000', '20,001-25,000', 
                                                  '25,001-30,000', '30,001-35,000', '35,001-40,000', '40,001 or above'))

df$health <- car::recode(df$health, "1='Excellent'; 2='Good'; 3='Fair'; 4='Poor'; 5='Very poor'
")

df$health <- factor(df$health, levels = c('Excellent', 'Good', 'Fair', 'Poor', 'Very poor'))

df$HLCat3 <- car::recode(df$HLCat, "
1 = 'Inadequate';
2 = 'Problematic';
3:4 = 'Sufficient/Excellent';
")

df$HLCat <- car::recode(df$HLCat, "
1 = 'Inadequate';
2 = 'Problematic';
3 = 'Sufficient';
4 = 'Excellent'
")

df$HLCat <- factor(df$HLCat, levels = c("Inadequate", "Problematic", "Sufficient", "Excellent"))
df$HLCat3 <- factor(df$HLCat3, levels = c("Inadequate", "Problematic", "Sufficient/Excellent"))

df$concordant_B1 <- ifelse(df$Modrisk_Breast %in% 1 & df$B_1 %in% 1:2, 1, 
                           ifelse(df$Modrisk_Breast %in% 0 & df$B_1 %in% 3:4, 1, 0))

# any family history of breast cancer
df$fhx_bc <- ifelse(df$FDR1_CA_HIST %in% 6 |
                      df$FDR2_CA_HIST %in% 6 |
                      df$FDR3_CA_HIST %in% 6 |
                      df$FDR4_CA_HIST %in% 6 |
                      df$FDR5_CA_HIST %in% 6 |
                      grepl("breast|乳", df$FDR_CA_RE, ignore.case = TRUE), 1, 0)

df$fhx_bc <- car::recode(df$fhx_bc, "
1='Yes'; 0='No'
")
df$fhx_bc <- factor(df$fhx_bc, levels =  c('Yes', 'No'))

df$Modrisk_Breast <- car::recode(df$Modrisk_Breast, "
1='Yes'; 0='No'
")
df$Modrisk_Breast <- factor(df$Modrisk_Breast, levels =  c('Yes', 'No'))

# descriptive statistics ----
allVars <- c("AGE", "age_cat", "WAIST", "BMI", "educ2", "marital", "econ", "hkborn", "income2", "hse_income2", "health", "HL", "HLCat3", 
             "cd", "DM", "LIVER", "HYPER", "LIPID", "IHD", "COAD", "STROKE", "CIRRHOSIS", "GERD", "COMOR_OTHER", "OWN_CA", "fhx_bc", "Modrisk_Breast","cd_nocancer",
             "B_1", "B_2", "B_3", "B_4A", "B_4B", "B_4C", "B_4D", "B_4E", "B_4F", "B_4G", "B_4H", "B_4I", "B_4J", "B_4K", "B_4L", 
             "B_5A", "B_5B", "B_5C", "B_5D", "B_5E", "B_5F", "B_5G", "concordant_B1")
catVars <- c("age_cat", "educ", "marital", "econ", "hkborn", "income2", "hse_income2", "health", "HLCat3",
             "DM", "LIVER", "HYPER", "LIPID", "IHD", "COAD", "STROKE", "CIRRHOSIS", "GERD", "COMOR_OTHER", "OWN_CA", "fhx_bc", "Modrisk_Breast", "concordant_B1",
             "educ2")

gen_desc(df, vars = allVars, nominalVars = catVars, medianVars = NULL
         , group = "HLCat3"
         ) %>% clipr::write_clip()

tableone::CreateTableOne(data =  df, 
                         strata = c("HLCat3"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()


# regression results ----
df$HLCat <- relevel(df$HLCat, ref = "Excellent")
df$HLCat3 <- relevel(df$HLCat3, ref = "Sufficient/Excellent")
df$educ <- relevel(df$educ, ref = "College/university or above")
df$educ2 <- relevel(df$educ2, ref = "College/university or above")

dep_vars <- c("B_1", "B_2", "B_3", "B_4A", "B_4B", "B_4C", "B_4D", "B_4E", "B_4F", "B_4G", "B_4H", "B_4I", "B_4J", "B_4K", "B_4L", 
              "B_5A", "B_5B", "B_5C", "B_5D", "B_5E", "B_5F", "B_5G", "concordant_B1")

table <- combinetab_loop(df, dep_vars, formula = " ~ 1+HLCat3", show_CI = TRUE, reg_model = lm)
table <- combinetab_loop(df, "concordant_B1", formula = " ~ 1+HLCat3", exponentiate = TRUE, show_CI = TRUE, reg_model = glm, family = binomial())
table <- combinetab_loop(df, dep_vars, formula = " ~ 1+age_cat+HLCat3")
table <- combinetab_loop(df, "concordant_B1", formula = " ~ 1+age_cat+HLCat3", exponentiate = TRUE, reg_model = glm, family = binomial())
table <- combinetab_loop(df, dep_vars, formula = " ~ 1+age_cat+cd_nocancer+I(OWN_CA=='Yes')+HLCat3")
table <- combinetab_loop(df, "concordant_B1", formula = " ~ 1+age_cat+cd_nocancer+I(OWN_CA=='Yes')+HLCat3", exponentiate = TRUE, reg_model = glm, family = binomial())

table <- combinetab_loop(df, dep_vars, show_CI = TRUE, formula = " ~ 1+age_cat+cd_nocancer+I(OWN_CA=='Yes')+HLCat3+I(fhx_bc=='Yes')+
                         WAIST+BMI+educ2+marital+econ+I(hkborn=='Yes')+income2")
                         # +I(DM=='Yes')+I(LIVER=='Yes')+I(HYPER=='Yes')+I(LIPID=='Yes')+I(IHD=='Yes')+I(COAD=='Yes')+I(STROKE=='Yes')+I(CIRRHOSIS=='Yes')+I(GERD=='Yes')+I(COMOR_OTHER=='Yes')
table <- combinetab_loop(df, "concordant_B1", show_CI = TRUE, formula = " ~ 1+age_cat+cd_nocancer+I(OWN_CA=='Yes')+HLCat3+I(fhx_bc=='Yes')+
                         WAIST+BMI+educ2+marital+econ+I(hkborn=='Yes')+income2", exponentiate = TRUE, reg_model = glm, family = binomial())


table <- combinetab_loop(df, "HL", formula = " ~ 1+age_cat+cd_nocancer+I(OWN_CA=='Yes')+I(fhx_bc=='Yes')+
                         WAIST+BMI+educ2+marital+econ+I(hkborn=='Yes')+income2", show_CI = F, reg_model = lm)

temp <- df %>% filter(income2 %!in% NA)
table <- combine_tables(NULL,
                        show_CI = F,
                        lm(HL~ 1+age_cat, data = temp),
                        lm(HL~ 1+cd_nocancer, data = temp),
                        lm(HL~ 1+I(OWN_CA=='Yes'), data = temp),
                        lm(HL~ 1+I(fhx_bc=='Yes'), data = temp),
                        lm(HL~ 1+WAIST, data = temp),
                        lm(HL~ 1+BMI, data = temp),
                        lm(HL~ 1+educ2, data = temp),
                        lm(HL~ 1+marital, data = temp),
                        lm(HL~ 1+econ, data = temp),
                        lm(HL~ 1+I(hkborn=='Yes'), data = temp),
                        lm(HL~ 1+income2, data = temp)
                        )

table <- combine_tables(NULL,
                        show_CI = F,
                        lm(HL~ 1+educ2+econ, data = temp),
                        lm(HL~ 1+income2+econ, data = temp),
                        lm(HL~ 1+educ2+income2+econ, data = temp)
)

table %>% clipr::write_clip()

# temp <- df %>% select(age_cat, cd_nocancer, OWN_CA, HLCat3,
#                         WAIST, BMI, educ2, marital, econ, hkborn, income2) %>% na.omit(df)

# Cronbach's alpha ----
library(psych) # alpha, tr
vars_B4 <- c("B_4A", "B_4B", "B_4C", "B_4D", "B_4E", "B_4F", "B_4G", "B_4H", "B_4I", "B_4J", "B_4K", "B_4L")
vars_B5 <- c("B_5A", "B_5B", "B_5C", "B_5D", "B_5E", "B_5F", "B_5G")

df %>% 
  select(vars_B4) %>%
  alpha(title = "B_4", check.keys=TRUE) %>% .[c("title", "total")] 

c_alpha(df[vars_B4], var =  vars_B4)
c_alpha(df[vars_B5], var =  vars_B5)

