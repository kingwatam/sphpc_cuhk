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

df$HLCat <- car::recode(df$HLCat, "
1 = 'Inadequate';
2 = 'Problematic';
3 = 'Sufficient';
4 = 'Excellent'
")

df$HLCat <- factor(df$HLCat, levels = c("Inadequate", "Problematic", "Sufficient", "Excellent"))
df$HLCat <- relevel(df$HLCat, ref = "Excellent")

# regression results ----
dep_vars <- c("B_1", "B_2", "B_3", "B_4A", "B_4B", "B_4C", "B_4D", "B_4E", "B_4F", "B_4G", "B_4H", "B_4I", "B_4J", "B_4K", "B_4L", 
              "B_5A", "B_5B", "B_5C", "B_5D", "B_5E", "B_5F", "B_5G", "B_5H")

table <- combinetab_loop(df, dep_vars, formula = " ~ 1+HL")
table <- combinetab_loop(df, dep_vars, formula = " ~ 1+HLCat")
table <- combinetab_loop(df, dep_vars, formula = " ~ 1+AGE+HL")
table <- combinetab_loop(df, dep_vars, formula = " ~ 1+AGE+HLCat")

table %>% clipr::write_clip()

# descriptive statistics ----
allVars <- c("age.0", "gender.0", "cd.0", "ht2.0", "dyslip.0", "dm.0", "skeletal.0", "chronicpain.0", "medication.0", "edu.0", "work.0", "marriage.0")
catVars <- c("gender.0",
             "ht2.0", "dyslip.0", "chronicpain.0", "dm.0", "skeletal.0",
             "work.0", "marriage.0")
tableone::CreateTableOne(data =  dfwide, 
                         # strata = c(""),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

