rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(foreign)
library(dplyr)
library(lme4)
library(lmerTest) # calculate p-values in summary()
library(ggplot2)
library(ggpubr) # ggerrorplot

setwd(sprintf("~%s/multimorbidity", setpath))

# data clean and merge full data from T0, T1, & T2
df <- read.dta("jc_acitivity_FU1 2019Mar26 v2.dta") # baseline (bl, f0) & first follow-up (f1)



# data cleaning based on data used for COVID paper (without some obs from baseline) ----
df <- haven::read_sav("JC_covid_data_Jul_Wide_20200728.sav")

df <- merge(df, full[, c("case_id", "efs4f0", "MOCA_total_bl", "MOCA_totalf1")], # extract social support item matched by case ID
            by=c("case_id"), all.x = TRUE) # MOCA_total_bl & MOCA_totalf0 are identical

df$support_f0 <- car::recode(df$efs4f0, "
'Always' = 1;
'Sometimes' = 1; 
'Never' = 2
")

df$efs4f0 <- car::recode(df$efs4f0, "
'Always' = 0;
'Sometimes' = 1; 
'Never' = 2
")

df$MOCA_total_tel <- NA

df <- df[, c("case_id",
             "datef0", "datef1", "date_tel",
             "agef0", "agef1", "age_tel",
             "Female", "CD",
             "eduless6", "notmarried", "alone", "unemployed", "cssa", "somkef0", "drinkf0", 
             "EQ5D_HK_bl", "EQ5D_HKf1", "EQ5D_index_tel", 
             "eq5d6_bl", "eq5d6f1", "eq5d6tel",
             "support_f0", "support_f1", "support_tel", # truncated support variable 
             "efs4f0", "efs4f1", "EFS4tel",  
             "meaning_bl", "meaningf1", "meaningtel",
             "Loneliness_T_bl", "Loneliness_Tf1", "loneliness_total_tel", 
             "Loneliness_E_bl", "Loneliness_Ef1", "loneliness_e_tel",
             "Loneliness_S_bl", "Loneliness_Sf1", "loneliness_s_tel",
             "GAD_bl", "gadf1", "gad_total_tel", 
             "GAD7_group_bl", "GAD7_groupf1", "gad_gp_tel", 
             "ISI_bl", "isif1", "isi_total_tel",
             "ISI_group_bl", "ISI_groupf1", "isi_gp_tel",
             "PHQ_bl", "phqf1", "phq_total_tel", 
             "PHQ9_group_bl", "PHQ9_groupf1", "phq_gp_tel", 
             "MOCA_total_bl", "MOCA_totalf1", "MOCA_total_tel")]


names(df)[names(df) == "Female"]  <- "female"

# fix mis-coding in PHQ groups
df$PHQ9_group_bl <- car::recode(df$PHQ_bl, "
0:4 = 1;
5:9 = 2;
10:14 = 3;
15:19 = 4;
20:hi = 5
")
df$PHQ9_groupf1 <- car::recode(df$phqf1, "
0:4 = 1;
5:9 = 2;
10:14 = 3;
15:19 = 4;
20:hi = 5
")
df$phq_gp_tel <- car::recode(df$phq_total_tel, "
0:4 = 1;
5:9 = 2;
10:14 = 3;
15:19 = 4;
20:hi = 5
")

# fix mis-coding in ISI groups
df$ISI_group_bl <- car::recode(df$ISI_bl, "
0:7 = 1;
8:14 = 2;
15:21 = 3;
22:hi = 4
")

df$ISI_groupf1 <- car::recode(df$isif1, "
0:7 = 1;
8:14 = 2;
15:21 = 3;
22:hi = 4
")

df$isi_gp_tel <- car::recode(df$isi_total_tel, "
0:7 = 1;
8:14 = 2;
15:21 = 3;
22:hi = 4
")

df[, c("efs4f0", "efs4f1", "EFS4tel")] <- sapply(df[, c("efs4f0", "efs4f1", "EFS4tel")], function(x){(as.numeric(x)-2)*-1}) # reverse code from 0:2 to 2:0
df$efs4f0 <- car::recode(df$efs4f0, # ad-hoc fix for strange behaviour for efs4f0
                         "
-1 = 0;
0 = 1;
1 = 2
")

# # create history of social support changes 
# df$efs4_hist <- ifelse(df$efs4f0 %in% NA | df$efs4f1 %in% NA | df$EFS4tel %in% NA, NA,
#                        paste(df$efs4f0, df$efs4f1, df$EFS4tel, sep = ""))

# from wide to long
df <- reshape(as.data.frame(df), 
              direction = "long", 
              idvar = "case_id", 
              varying = list(c("datef0", "datef1", "date_tel"),
                             c("agef0", "agef1", "age_tel"),
                             c("EQ5D_HK_bl", "EQ5D_HKf1", "EQ5D_index_tel"),
                             c("eq5d6_bl", "eq5d6f1", "eq5d6tel"),
                             c("support_f0", "support_f1", "support_tel"),
                             c("efs4f0", "efs4f1", "EFS4tel"), 
                             c("meaning_bl", "meaningf1", "meaningtel"), 
                             c("Loneliness_T_bl", "Loneliness_Tf1", "loneliness_total_tel"), 
                             c("Loneliness_E_bl", "Loneliness_Ef1", "loneliness_e_tel"),
                             c("Loneliness_S_bl", "Loneliness_Sf1", "loneliness_s_tel"),
                             c("GAD_bl", "gadf1", "gad_total_tel"), 
                             c("GAD7_group_bl", "GAD7_groupf1", "gad_gp_tel"), 
                             c("ISI_bl", "isif1", "isi_total_tel"),
                             c("ISI_group_bl", "ISI_groupf1", "isi_gp_tel"),
                             c("PHQ_bl", "phqf1", "phq_total_tel"), 
                             c("PHQ9_group_bl", "PHQ9_groupf1", "phq_gp_tel"), 
                             c("MOCA_total_bl", "MOCA_totalf1", "MOCA_total_tel")
              ), 
              v.names=c("date", 
                        "age", 
                        "eq5d",
                        "eq5dvas",
                        "support", 
                        "efs4", 
                        "meaning",
                        "loneliness",
                        "loneliness_emo",
                        "loneliness_soc",
                        "gad",
                        "gad_group",
                        "isi",
                        "isi_group",
                        "phq", 
                        "phq_group",
                        "moca"), 
              timevar="time",
              times=c("0", "1", "2")) 

df$mci <- ifelse((df$moca < 21 & df$eduless6 == 1) | 
                   (df$moca < 22 & df$eduless6 %in% c(0, NA)), 1,
                 ifelse(df$moca %in% NA, NA, 0))

df$support <- (as.numeric(df$support)-2)*-1 # from 2=never;1=always/sometimes to 0=never;1=always/sometimes
df$support <- df$efs4

df$efs4 <- car::recode(df$efs4, "
2 = 'Always';
1 = 'Sometimes';
0 = 'Never'
")

df[, c("support", "meaning")] <- sapply(df[, c("support", "meaning")], function(x){as.numeric(x)}) 
# from long to wide
dfwide <- reshape(data=df, idvar= c("case_id"),
                  timevar = "time",
                  v.names=c("date", 
                            "age", 
                            "eq5d",
                            "eq5dvas",
                            "support", 
                            "efs4", 
                            "meaning",
                            "loneliness",
                            "loneliness_emo",
                            "loneliness_soc",
                            "gad",
                            "gad_group",
                            "isi",
                            "isi_group",
                            "phq", 
                            "phq_group",
                            "moca", "mci"), 
                  direction="wide")

saveRDS(df, file = "JC_covid_data_long.rds")
saveRDS(dfwide, file = "JC_covid_data_wide.rds")