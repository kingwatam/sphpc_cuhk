rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(labelled)
library(gee)
library(geepack)
library(lme4)
library(lmerTest)

setwd(sprintf("~%s/mindfulness_fgid", setpath))
df <- foreign_to_labelled(haven::read_sav("GAD_GI_cleaned_outcomes_150901_180615.sav", encoding = "UTF-8")) 
dfwide <- foreign_to_labelled(haven::read_sav("GAD_GI_cleaned_150831_180608.sav", encoding = "UTF-8")) 

# descriptive statistics ----
allVars <- c("Age", "SEX", "Education", "Occupation_2", "ReMarital", "ReIncome")
catVars <- c("SEX", "Education", "Occupation_2", "ReMarital", "ReIncome")
tableone::CreateTableOne(data =  dfwide, strata = "ARM", vars = allVars, factorVars = catVars, addOverall = TRUE) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

# GEE results ----
library(gee)
# df$Time1 <- ifelse(df$Time == 1, 1, 0)
# df$Time2 <- ifelse(df$Time == 2, 1, 0)
# df$Time3 <- ifelse(df$Time == 3, 1, 0)
# df$Time4 <- ifelse(df$Time == 4, 1, 0)
# df$Time5 <- ifelse(df$Time == 5, 1, 0)

full_rank <- function(data){
  data_num <- sapply(data, as.numeric)
  # data <- data[,duplicated(cor(data_num))]
  # data %>% View
  data <- data[complete.cases(data),]
  data <- data[, qr(data)$pivot[seq_len(qr(data)$rank)]]
  return((data))
}

gee(GI_da_A ~  ARM * as.factor(Time),
       data = df,
       id = SUBNO, family = binomial, corstr = "unstructured", na.action = na.omit) %>% summary()

geeglm(GI_da_A ~  ARM + as.factor(Time),
    data = df,
    id = SUBNO, family = binomial, corstr = "unstructured") %>% summary()

glmm_fgid <- glmer(GI_da_A~ 1+ARM*as.factor(Time)+ (1| SUBNO), family = binomial, data = df) 

table <- combine_tables(NULL,
                        exponentiate = FALSE,
                        glmer(GERD_da~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(GERD_da~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        
                        glmer(FD_da~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(FD_da~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),

                        glmer(FD_da_3mth~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(FD_da_3mth~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),

                        glmer(IBS_da~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(IBS_da~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),

                        glmer(IBS_da_3mth~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(IBS_da_3mth~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),

                        glmer(GI_da_A~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(GI_da_A~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),

                        glmer(GI_da_A_3mth~ 
                                dummy(ARM, "1") + dummy(ARM, "2") + 
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                                dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df),
                        glmer(GI_da_A_3mth~
                                dummy(ARM, "1") + dummy(ARM, "3") +
                                dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                                dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                                dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") +
                                dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") + (1| SUBNO), family = binomial, data = df)
                        )

df$ARM <- as.numeric(df$ARM)
df$Time <- as.numeric(df$Time)

table2 <- combine_tables(NULL,
                         exponentiate = FALSE,
                         gee(GERD_da ~  
                              dummy(ARM, "1") + dummy(ARM, "2") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        gee(GERD_da ~  
                              dummy(ARM, "1") + dummy(ARM, "3") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        
                        gee(FD_da ~  
                              dummy(ARM, "1") + dummy(ARM, "2") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        gee(FD_da ~  
                              dummy(ARM, "1") + dummy(ARM, "3") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        
                        gee(FD_da_3mth ~  
                              dummy(ARM, "1") + dummy(ARM, "2") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        gee(FD_da_3mth ~  
                              dummy(ARM, "1") + dummy(ARM, "3") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        
                        gee(IBS_da ~  
                              dummy(ARM, "1") + dummy(ARM, "2") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        gee(IBS_da ~  
                              dummy(ARM, "1") + dummy(ARM, "3") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        
                        gee(IBS_da_3mth ~  
                              dummy(ARM, "1") + dummy(ARM, "2") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        gee(IBS_da_3mth ~  
                              dummy(ARM, "1") + dummy(ARM, "3") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),

                        # gee(GI_da_A ~
                        #       dummy(ARM, "1") + dummy(ARM, "2") +
                        #       dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                        #       dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") +
                        #       dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") +
                        #       dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                        #     data = df,
                        #     id = SUBNO, family = binomial, corstr = "unstructured"),
                        # gee(GI_da_A ~  
                        #       dummy(ARM, "1") + dummy(ARM, "3") + 
                        #       dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                        #       dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                        #       dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                        #       dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                        #     data = df, 
                        #     id = SUBNO, family = binomial, corstr = "unstructured"),
                        
                        gee(GI_da_A_3mth ~  
                              dummy(ARM, "1") + dummy(ARM, "2") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "2"):dummy(Time, "2") + dummy(ARM, "2"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured"),
                        gee(GI_da_A_3mth ~  
                              dummy(ARM, "1") + dummy(ARM, "3") + 
                              dummy(Time, "2") + dummy(Time, "3") + dummy(Time, "4") + dummy(Time, "5") +
                              dummy(ARM, "1"):dummy(Time, "2") + dummy(ARM, "1"):dummy(Time, "3") + 
                              dummy(ARM, "3"):dummy(Time, "2") + dummy(ARM, "3"):dummy(Time, "3") + 
                              dummy(ARM, "1"):dummy(Time, "4") + dummy(ARM, "1"):dummy(Time, "5") ,
                            data = df, 
                            id = SUBNO, family = binomial, corstr = "unstructured")
                        
                        )

