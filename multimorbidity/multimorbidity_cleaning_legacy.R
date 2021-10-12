rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(labelled) # foreign_to_labelled
library(dplyr)
library(lme4)
library(lmerTest) # calculate p-values in summary()
library(ggplot2)
library(ggpubr) # ggerrorplot
library(eq5d)

setwd(sprintf("~%s/multimorbidity", setpath))

# # data cleaning based on data used for COVID paper (without some obs from baseline) ----
# df <- haven::read_sav("JC_covid_data_Jul_Wide_20200728.sav")
# t0t1 <- foreign_to_labelled(haven::read_sav("jc_acitivity_FU1 2019Mar26 v2.sav", encoding = "MS936"))
# 
# df <- merge(df, t0t1[, c("case_id", "efs4f0", "MOCA_total_bl", "MOCA_totalf1")], # extract social support item matched by case ID
#             by=c("case_id"), all.x = TRUE) # MOCA_total_bl & MOCA_totalf0 are identical
# 
# df$support_f0 <- car::recode(to_character(df$efs4f0), "
# 'Always' = 1;
# 'Sometimes' = 1;
# 'Never' = 2
# ")
# 
# df$efs4f0 <- car::recode(to_character(df$efs4f0), "
# 'Always' = 0;
# 'Sometimes' = 1;
# 'Never' = 2
# ")
# 
# df$MOCA_total_tel <- NA
# 
# df <- df[, c("case_id",
#              "datef0", "datef1", "date_tel",
#              "agef0", "agef1", "age_tel",
#              "Female", "CD",
#              "eduless6", "notmarried", "alone", "unemployed", "cssa", "somkef0", "drinkf0",
#              "EQ5D_HK_bl", "EQ5D_HKf1", "EQ5D_index_tel",
#              "eq5d6_bl", "eq5d6f1", "eq5d6tel",
#              "support_f0", "support_f1", "support_tel", # truncated support variable
#              "efs4f0", "efs4f1", "EFS4tel",
#              "meaning_bl", "meaningf1", "meaningtel",
#              "Loneliness_T_bl", "Loneliness_Tf1", "loneliness_total_tel",
#              "Loneliness_E_bl", "Loneliness_Ef1", "loneliness_e_tel",
#              "Loneliness_S_bl", "Loneliness_Sf1", "loneliness_s_tel",
#              "GAD_bl", "gadf1", "gad_total_tel",
#              "GAD7_group_bl", "GAD7_groupf1", "gad_gp_tel",
#              "ISI_bl", "isif1", "isi_total_tel",
#              "ISI_group_bl", "ISI_groupf1", "isi_gp_tel",
#              "PHQ_bl", "phqf1", "phq_total_tel",
#              "PHQ9_group_bl", "PHQ9_groupf1", "phq_gp_tel",
#              "MOCA_total_bl", "MOCA_totalf1", "MOCA_total_tel",
#              "SAR_total_bl", "SAR_totalf1", "sar_total_tel")]
# 
# 
# names(df)[names(df) == "Female"]  <- "female"
# 
# # fix mis-coding in PHQ groups
# df$PHQ9_group_bl <- car::recode(df$PHQ_bl, "
# 0:4 = 1;
# 5:9 = 2;
# 10:14 = 3;
# 15:19 = 4;
# 20:hi = 5
# ")
# df$PHQ9_groupf1 <- car::recode(df$phqf1, "
# 0:4 = 1;
# 5:9 = 2;
# 10:14 = 3;
# 15:19 = 4;
# 20:hi = 5
# ")
# df$phq_gp_tel <- car::recode(df$phq_total_tel, "
# 0:4 = 1;
# 5:9 = 2;
# 10:14 = 3;
# 15:19 = 4;
# 20:hi = 5
# ")
# 
# # fix mis-coding in ISI groups
# df$ISI_group_bl <- car::recode(df$ISI_bl, "
# 0:7 = 1;
# 8:14 = 2;
# 15:21 = 3;
# 22:hi = 4
# ")
# 
# df$ISI_groupf1 <- car::recode(df$isif1, "
# 0:7 = 1;
# 8:14 = 2;
# 15:21 = 3;
# 22:hi = 4
# ")
# 
# df$isi_gp_tel <- car::recode(df$isi_total_tel, "
# 0:7 = 1;
# 8:14 = 2;
# 15:21 = 3;
# 22:hi = 4
# ")
# 
# df[, c("efs4f0", "efs4f1", "EFS4tel")] <- sapply(df[, c("efs4f0", "efs4f1", "EFS4tel")], function(x){(as.numeric(x)-2)*-1}) # reverse code from 0:2 to 2:0
# df$efs4f0 <- car::recode(df$efs4f0, # ad-hoc fix for strange behaviour for efs4f0
#                          "
# -1 = 0;
# 0 = 1;
# 1 = 2
# ")
# 
# # # create history of social support changes
# # df$efs4_hist <- ifelse(df$efs4f0 %in% NA | df$efs4f1 %in% NA | df$EFS4tel %in% NA, NA,
# #                        paste(df$efs4f0, df$efs4f1, df$EFS4tel, sep = ""))
# 
# # from wide to long
# df <- reshape(as.data.frame(df),
#               direction = "long",
#               idvar = "case_id",
#               varying = list(c("datef0", "datef1", "date_tel"),
#                              c("agef0", "agef1", "age_tel"),
#                              c("EQ5D_HK_bl", "EQ5D_HKf1", "EQ5D_index_tel"),
#                              c("eq5d6_bl", "eq5d6f1", "eq5d6tel"),
#                              c("support_f0", "support_f1", "support_tel"),
#                              c("efs4f0", "efs4f1", "EFS4tel"),
#                              c("meaning_bl", "meaningf1", "meaningtel"),
#                              c("Loneliness_T_bl", "Loneliness_Tf1", "loneliness_total_tel"),
#                              c("Loneliness_E_bl", "Loneliness_Ef1", "loneliness_e_tel"),
#                              c("Loneliness_S_bl", "Loneliness_Sf1", "loneliness_s_tel"),
#                              c("GAD_bl", "gadf1", "gad_total_tel"),
#                              c("GAD7_group_bl", "GAD7_groupf1", "gad_gp_tel"),
#                              c("ISI_bl", "isif1", "isi_total_tel"),
#                              c("ISI_group_bl", "ISI_groupf1", "isi_gp_tel"),
#                              c("PHQ_bl", "phqf1", "phq_total_tel"),
#                              c("PHQ9_group_bl", "PHQ9_groupf1", "phq_gp_tel"),
#                              c("MOCA_total_bl", "MOCA_totalf1", "MOCA_total_tel"),
#                              c("SAR_total_bl", "SAR_totalf1", "sar_total_tel")
#               ),
#               v.names=c("date",
#                         "age",
#                         "eq5d",
#                         "eq5dvas",
#                         "support",
#                         "efs4",
#                         "meaning",
#                         "loneliness",
#                         "loneliness_emo",
#                         "loneliness_soc",
#                         "gad",
#                         "gad_group",
#                         "isi",
#                         "isi_group",
#                         "phq",
#                         "phq_group",
#                         "moca",
#                         "sar"),
#               timevar="time",
#               times=c("0", "1", "2"))
# 
# df$mci <- ifelse(df$moca %in% NA, NA,
#                  ifelse(df$moca >= 22, 0, 1))
# 
# df$support <- (as.numeric(df$support)-2)*-1 # from 2=never;1=always/sometimes to 0=never;1=always/sometimes
# df$support <- df$efs4
# 
# df$efs4 <- car::recode(df$efs4, "
# 2 = 'Always';
# 1 = 'Sometimes';
# 0 = 'Never'
# ")
# 
# df[, c("support", "meaning")] <- sapply(df[, c("support", "meaning")], function(x){as.numeric(x)})
# # from long to wide
# dfwide <- reshape(data=df, idvar= c("case_id"),
#                   timevar = "time",
#                   v.names=c("date",
#                             "age",
#                             "eq5d",
#                             "eq5dvas",
#                             "support",
#                             "efs4",
#                             "meaning",
#                             "loneliness",
#                             "loneliness_emo",
#                             "loneliness_soc",
#                             "gad",
#                             "gad_group",
#                             "isi",
#                             "isi_group",
#                             "phq",
#                             "phq_group",
#                             "moca", "mci",
#                             "sar"),
#                   direction="wide")
# 
# saveRDS(df, file = "JC_covid_data_long.rds")
# saveRDS(dfwide, file = "JC_covid_data_wide.rds")

# # data clean T2 data ----
# t2 <- xlsx::read.xlsx2("2016-2019 JC project-follow-up II (Responses)_king.xlsx", sheetName  = "Form responses 1"
#                        , encoding = "UTF-8"
#                        , header = FALSE
# )
# t2 <- t2[2:nrow(t2),] # remove header
# names(t2)[1:5] <- c("Timestamp", "Case.number.", "Patient.initial.", "Age", "Date.of.assessment.")
# t2$Timestamp <- as.Date(as.numeric(t2$Timestamp), origin = "1899-12-30")
# t2$Date.of.assessment. <- as.Date(as.numeric(t2$Date.of.assessment.), origin = "1899-12-30")
# names(t2)[names(t2) == "Case.number."] <- "sopd"
# t2 <- t2[1:50] # remove last variable for encoding
# # data clean T2 data
# # PHQ
# names(t2)[6:14] <- sprintf("phq%sf2", 1:9)
# q_phq <- names(t2)[6:14]
# t2[q_phq] <- as.numeric(sapply(t2[q_phq], after_char, "=") )
# # GAD
# names(t2)[15:21] <- sprintf("gad%sf2", 1:7)
# q_gad <- names(t2)[15:21]
# t2[q_gad] <- as.numeric(sapply(t2[q_gad], after_char, "=") )
# # loneliness scale
# names(t2)[22:28] <- sprintf("ls%sf2", 1:7)
# q_ls <- names(t2)[22:28]
# t2[q_ls[1:6]] <- as.numeric(sapply(t2[q_ls[1:6]], before_char, "*") )
# t2[q_ls[7]] <- as.numeric(sapply(t2[q_ls[7]], before_char, "=") )
# # ISI
# names(t2)[29:35] <- sprintf("isi%sf2", 1:7)
# q_isi <- names(t2)[29:35]
# t2[q_isi] <- as.numeric(sapply(t2[q_isi], before_char, "=") )
# # EQ5D
# names(t2)[36:41] <- sprintf("eq5d%sf2", 1:6)
# q_eq5d <- names(t2)[36:41]
# t2[q_eq5d[1:5]] <- as.numeric(sapply(t2[q_eq5d[1:5]], before_char, ")") )
# # SAR (sarcopenia)
# names(t2)[42:46] <- sprintf("sar%sf2", 1:5)
# q_sar <- names(t2)[42:46]
# t2[q_sar] <- as.numeric(sapply(t2[q_sar], before_char, "=") )
# # support
# names(t2)[47] <- "efs4f2"
# t2["efs4f2"] <- as.numeric(sapply(t2["efs4f2"], before_char, "=") )
# # meaning of life
# names(t2)[48] <- "meaningf2"
# t2$meaningf2[t2$meaningf2 == ""] <- NA
# # Rater
# names(t2)[49] <- "raterf2"
# # Insomnia (single-item)
# names(t2)[50] <- "insomniaf2"
# t2["insomniaf2"] <- as.numeric(sapply(t2["insomniaf2"], before_char, "=") )
# t2$insomniaf2[t2$insomniaf2 == ""] <- NA
# 
# scoring <- function(t2) {
#   t2[q_ls[1:3]] <- sapply(t2[q_ls[1:3]], car::recode, "
#   1:2 = 1
#   ") # recode (0,1,2) to (0,1,1) for scoring
#   t2[q_ls[4:6]] <- sapply(t2[q_ls[4:6]], car::recode, "
#   c(2,0) = 1;
#   1 = 0
#   ") # recode (0,1,2) to (1,0,1) for scoring
# 
#   # reverse EQ5D items
#   t2[q_eq5d[1:5]] <-  (t2[q_eq5d[1:5]]-6)*-1
# 
#   t2$eq5d_score <- ifelse(is.na(t2$eq5d1f2) | is.na(t2$eq5d2f2) | is.na(t2$eq5d3f2) | is.na(t2$eq5d4f2) | is.na(t2$eq5d5f2),
#                           NA, paste0(t2$eq5d1f2, t2$eq5d2f2, t2$eq5d3f2, t2$eq5d4f2, t2$eq5d5f2))
# 
#   # outcome scoring
#   t2 <- t2 %>%
#     mutate(
#       phqf2 = rowSums(.[q_phq], na.rm = FALSE),
#       gadf2 = rowSums(.[q_gad], na.rm = FALSE),
#       Loneliness_Tf2 = rowSums(.[q_ls[1:6]], na.rm = FALSE),
#       Loneliness_Ef2 = rowSums(.[q_ls[1:3]], na.rm = FALSE), # emotional loneliness
#       Loneliness_Sf2 = rowSums(.[q_ls[4:6]], na.rm = FALSE), # social loneliness
#       Loneliness_Qf2 = .[, q_ls[7]],
#       isif2 = rowSums(.[q_isi], na.rm = FALSE),
#       SAR_totalf2 = rowSums(.[q_sar], na.rm = FALSE)
#     )
#   t2$EQ5D_HKf2 =  eq5d(scores=t2$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)
# 
#   return(subset(t2, select = c(phqf2, gadf2, Loneliness_Tf2, Loneliness_Ef2, Loneliness_Sf2, Loneliness_Qf2,
#                                isif2, EQ5D_HKf2, SAR_totalf2
#                                )))
# }
# t2 <- cbind(t2, scoring(t2))
# 
# saveRDS(t2, "2020_data_cleaned.rds")

# merge full data from T0, T1, & T2 ----
t0t1 <- foreign_to_labelled(haven::read_sav("jc_acitivity_FU1 2019Mar26 v2.sav", encoding = "MS936"))
# t0t1 <- foreign_to_labelled(haven::read_dta("jc_acitivity_FU1 2019Mar26 v2.dta", encoding = "MS936")) # baseline (bl, f0) & first follow-up (f1), import values instead of factors, encoding is MS simplified Chinese
# t0t1 <- foreign::read.dta("jc_acitivity_FU1 2019Mar26 v2.dta", convert.factors = TRUE) # baseline (bl, f0) & first follow-up (f1), import factors instead of values
t2 <- readRDS("2020_data_cleaned.rds") # duplicates found and removed in T2 data (sopd UID FMC20311381N & GYCK1716113N)

# transfer labels from T0T1 data to T2
get_labels <- function(df1, df2, var_name, n, test_identical = FALSE){
  for (var in sprintf("%s%sf2", var_name, 1:n)){
    for (var2 in sprintf("%s%sf0", var_name, 1:n)){
      val_labels(df1[var]) <- val_labels(df2[var2])[[1]]
      
     if(test_identical){
       error_msg <- c("The follow variables are not identical")
       for (var3 in sprintf("%s%sf1", var_name, 1:n)){
         if (!identical(val_labels(t0t1[var2])[[1]], val_labels(t0t1[var3])[[1]])){
           error_msg <- append(error_msg, var2)
           print(var2)
         }
       }
       if (length(error_msg) > 1){
         stop(error_msg)
       }
     }
    }
  }
  return(df1)
}

t2 <- get_labels(t2, t0t1, var_name =  "phq", n = 9, test_identical = TRUE)
t2 <- get_labels(t2, t0t1, var_name =  "gad", n = 7, test_identical = TRUE)
t2 <- get_labels(t2, t0t1, var_name =  "ls", n = 6, test_identical = TRUE)
t2 <- get_labels(t2, t0t1, var_name =  "sar", n = 4, test_identical = TRUE)
val_labels(t2$sar5f2) <- val_labels(t0t1$sar5f0)
val_labels(t2$efs4f2) <- val_labels(t0t1$efs4f0)

# temp <- merge(t2[1:4], t0t1[, c("sopd", "case_id")], # extract item matched by case ID
#                by=c("sopd"), all.x = TRUE)

df <- merge(t0t1, t2, # extract item matched by case ID
              by=c("sopd"), all.x = TRUE)
# df <- merge(t2, t0t1, # extract item matched by case ID
#             by=c("sopd"), all.x = TRUE)

temp <- haven::read_sav("JC_covid_data_Jul_Wide_20200728.sav") 
names(temp)[names(temp) == "age_tel"] <- "agef2"
df <- merge(df, temp[, c("case_id", "agef1", "agef2")], # extract item matched by case ID
            by=c("case_id"), all.x = TRUE)
rm(temp)


df$eduf0[which(df$eduf0 == 55)] <- 5 # appears to be incorrect entry
df$MOCA_totalf2 <- NA
df$agef0 <- as.numeric(df$age) 
df$agef2 <- as.numeric(df$Age) # use raw values instead
# df$agef2 <- ifelse(df$agef2 < df$agef0, 
#                    df$agef0 + round((df$Date.of.assessment. - df$datef0)/365.2422)
#                                     , df$agef2) # four errors where agef2 is lower than agef0
# df$agef1 <- ifelse(is.na(df$agef1), df$agef0 + round((df$agef2-df$agef0)*as.vector(df$datef1 - df$datef0)/as.vector(df$Date.of.assessment. - df$datef0)),
#                    df$agef1)# age interpolated between T0 & T2 ages and survey dates
df$agef1 <- df$agef0 + round((df$datef1-df$datef0)/365.2422)
df$agef1 <- ifelse(is.na(df$datef1), NA, df$agef1) # remove calculated age if survey data for T1 isn't available
df$agef0[df$sopd == "GLYH0611542N"] <- 76
df$agef1[df$sopd == "GLYH0611542N"] <- 78

df$agef0[df$sopd == "FMC21016164U"] <- 67
df$agef1[df$sopd == "FMC21016164U"] <- 69

df$agef0[df$sopd == "GLYH0916389P"] <- 65
df$agef1[df$sopd == "GLYH0916389P"] <- 66
df$agef2[df$sopd == "GLYH0916389P"] <- 68

# # generate Excel file for problematic age values
# temp <- df
# names(temp)[names(temp) == "Age"] <- "agef2_raw"
# names(temp)[names(temp) == "Date.of.assessment."] <- "datef2"
# temp$agef1minusf0 <- temp$agef1 - temp$agef0
# temp$agef2minusf0 <- temp$agef2 - temp$agef0
# temp$agef2minusf1 <- temp$agef2 - temp$agef1
# temp$agef2minusf2_raw <- temp$agef2 - as.numeric(temp$agef2_raw)
# temp <- temp[temp$cohort == 1, ] # remove <60yo or number of chronic disease < 2, and duplicates
# 
# write_excel("data_excerpt.xlsx", temp[, c("case_id", "sopd", "initialf0",
#                                           "datef0", "agef0", "datef1", "agef1", "datef2", "agef2", "agef2_raw",
#                                           "agef1minusf0", "agef2minusf0", "agef2minusf1", "agef2minusf2_raw"
#                                           )])
# rm(temp)

df <- df %>%
  mutate(mci_catf0 = case_when(
    MOCA_total_bl > 26 ~ "None",
    MOCA_total_bl >= 15 & MOCA_total_bl <= 26 ~ "Moderate",
    MOCA_total_bl < 15 ~ "Severe"
  ))
df <- df %>%
  mutate(mci_catf1 = case_when(
    MOCA_totalf1 > 26 ~ "None",
    MOCA_totalf1 >= 15 & MOCA_totalf1 <= 26 ~ "Moderate",
    MOCA_totalf1 < 15 ~ "Severe"
  ))
df$mci_catf2 <- NA

df$mcif0 <- ifelse(df$MOCA_total_bl %in% NA, NA,
                   ifelse(df$MOCA_total_bl >= 22, 0, 1))
df$mcif1 <- ifelse(df$MOCA_totalf1 %in% NA, NA,
                   ifelse(df$MOCA_totalf1 >= 22, 0, 1))
df$mcif2 <- NA

df$hgs1f2 <- NA
df$hgs2f2 <- NA
df$hgs3f2 <- NA
df$hgs4f2 <- NA

df$EFS_totalf2 <- NA
df$FS_totalf1 <- NA
df$FS_totalf2 <- NA

df$BMIf2 <- NA

df$waistf2 <- NA

names(df)[names(df) == "md1f0"] <- "htf0"
names(df)[names(df) == "md2f0"] <- "dyslipf0"
names(df)[names(df) == "md3f0"] <- "dmf0"
names(df)[names(df) == "msd1f0"] <- "chronicpainf0"
df$chronicpainf0 <- ifelse(df$chronicpainf0 >= 1, 1, 0)
names(df)[names(df) == "msd2f0"] <- "inflamf0"

df <- reshape(as.data.frame(df[, c("gender", "chronic_diseasef0", "cohort", "eduf0", "smokef0", "alcoholf0", "audit2xf0",
                                   "htf0", "dyslipf0", "dmf0", "chronicpainf0", "inflamf0", "medicationf0", "workf0", "marriagef0",
                                   "MMSE_totalf0",
                                  c("datef0", "datef1", "Date.of.assessment."),
                                  c("agef0", "agef1", "agef2"),
                                  c("EQ5D_HK_bl", "EQ5D_HKf1", "EQ5D_HKf2"),
                                  c("eq5d6_bl", "eq5d6f1", "eq5d6f2"),
                                  # c("support_f0", "support_f1", "support_tel"),
                                  c("efs4f0", "efs4f1", "efs4f2"), 
                                  c("meaning_bl", "meaningf1", "meaningf2"), 
                                  c("Loneliness_T_bl", "Loneliness_Tf1", "Loneliness_Tf2"), 
                                  c("Loneliness_E_bl", "Loneliness_Ef1", "Loneliness_Ef2"),
                                  c("Loneliness_S_bl", "Loneliness_Sf1", "Loneliness_Sf2"),
                                  c("GAD_bl", "GADf1", "gadf2"), # GAD_bl = GADf0
                                  # c("GAD7_group_bl", "GAD7_groupf1", "gad_gp_tel"), 
                                  c("ISIf0", "isif1", "isif2"),
                                  # c("ISI_group_bl", "ISI_groupf1", "isi_gp_tel"),
                                  c("PHQ_bl", "PHQf1", "phqf2"), 
                                  # c("PHQ9_group_bl", "PHQ9_groupf1", "phq_gp_tel"), 
                                  c("MOCA_total_bl", "MOCA_totalf1", "MOCA_totalf2"),
                                  c("SAR_total_bl", "SAR_totalf1", "SAR_totalf2"),
                                  c("sar1f0", "sar1f1", "sar1f2"),
                                  c("sar2f0", "sar2f1", "sar2f2"),
                                  c("sar3f0", "sar3f1", "sar3f2"),
                                  c("sar4f0", "sar4f1", "sar4f2"),
                                  c("sar5f0", "sar5f1", "sar5f2"),
                                  c("hgs1f0", "hgs1f1", "hgs1f2"),
                                  c("hgs2f0", "hgs2f1", "hgs2f2"),
                                  c("hgs3f0", "hgs3f1", "hgs3f2"),
                                  c("hgs4f0", "hgs4f1", "hgs4f2"),
                                  c("mcif0", "mcif1", "mcif2"),
                                  c("mci_catf0", "mci_catf1", "mci_catf2"),
                                  c("EFS_totalf0", "EFS_totalf1", "EFS_totalf2"),
                                  c("FS_totalf0", "FS_totalf1", "FS_totalf2"),
                                  c("BMIf0", "BMIf1", "BMIf2"),
                                  c("waistf0", "waistf1", "waistf2"))]), 
              direction = "long", 
              idvar = "case_id", 
              varying = list(c("datef0", "datef1", "Date.of.assessment."),
                             c("agef0", "agef1", "agef2"),
                             c("EQ5D_HK_bl", "EQ5D_HKf1", "EQ5D_HKf2"),
                             c("eq5d6_bl", "eq5d6f1", "eq5d6f2"),
                             # c("support_f0", "support_f1", "support_tel"),
                             c("efs4f0", "efs4f1", "efs4f2"), 
                             c("meaning_bl", "meaningf1", "meaningf2"), 
                             c("Loneliness_T_bl", "Loneliness_Tf1", "Loneliness_Tf2"), 
                             c("Loneliness_E_bl", "Loneliness_Ef1", "Loneliness_Ef2"),
                             c("Loneliness_S_bl", "Loneliness_Sf1", "Loneliness_Sf2"),
                             c("GAD_bl", "GADf1", "gadf2"), # GAD_bl = GADf0
                             # c("GAD7_group_bl", "GAD7_groupf1", "gad_gp_tel"), 
                             c("ISIf0", "isif1", "isif2"),
                             # c("ISI_group_bl", "ISI_groupf1", "isi_gp_tel"),
                             c("PHQ_bl", "PHQf1", "phqf2"), 
                             # c("PHQ9_group_bl", "PHQ9_groupf1", "phq_gp_tel"), 
                             c("MOCA_total_bl", "MOCA_totalf1", "MOCA_totalf2"),
                             c("SAR_total_bl", "SAR_totalf1", "SAR_totalf2"), 
                             c("sar1f0", "sar1f1", "sar1f2"),
                             c("sar2f0", "sar2f1", "sar2f2"),
                             c("sar3f0", "sar3f1", "sar3f2"),
                             c("sar4f0", "sar4f1", "sar4f2"),
                             c("sar5f0", "sar5f1", "sar5f2"),
                             c("hgs1f0", "hgs1f1", "hgs1f2"),
                             c("hgs2f0", "hgs2f1", "hgs2f2"),
                             c("hgs3f0", "hgs3f1", "hgs3f2"),
                             c("hgs4f0", "hgs4f1", "hgs4f2"),
                             c("mcif0", "mcif1", "mcif2"),
                             c("mci_catf0", "mci_catf1", "mci_catf2"),
                             c("EFS_totalf0", "EFS_totalf1", "EFS_totalf2"),
                             c("FS_totalf0", "FS_totalf1", "FS_totalf2"),
                             c("BMIf0", "BMIf1", "BMIf2"),
                             c("waistf0", "waistf1", "waistf2")
              ), 
              v.names=c("date", 
                        "age", 
                        "eq5d",
                        "eq5dvas",
                        # "support", 
                        "efs4", 
                        "meaning",
                        "loneliness",
                        "loneliness_emo",
                        "loneliness_soc",
                        "gad",
                        # "gad_group",
                        "isi",
                        # "isi_group",
                        "phq", 
                        # "phq_group",
                        "moca",
                        "sarc_f",
                        "sar1",
                        "sar2",
                        "sar3",
                        "sar4",
                        "sar5",
                        "hgs1", # first measurement of hand-grip strength of left hand
                        "hgs2", # second measurement of hand-grip strength of left hand
                        "hgs3", # first measurement of hand-grip strength of right hand
                        "hgs4", # first measurement of hand-grip strength of right hand
                        "mci",
                        "mci_cat",
                        "efs",
                        "fs", 
                        "bmi",
                        "waist"), 
              timevar="time",
              times=c("0", "1", "2")) 

df$hgs_l <- pmax(df$hgs1, df$hgs2) # better hand-grip strength of left hand
df$hgs_r <-  pmax(df$hgs3, df$hgs4) # better hand-grip strength of right hand
df$hgs <- ifelse(df$hgs_l %in% NA, df$hgs_r, 
                 ifelse(df$hgs_r %in% NA, df$hgs_l, (df$hgs_l + df$hgs_r)/2)) # average hand-grip strength
df$gender <- to_factor(df$gender)
df$sar_hgs <- ifelse(is.na(df$hgs) | is.na(df$gender) | is.na(df$sarc_f), NA, 
                     ifelse(df$hgs < 28 & df$gender == "Male" & df$sarc_f >= 4 , 1, 
                            ifelse(df$hgs < 18 & df$gender == "Female"  & df$sarc_f >= 4, 1, 0)))

# further data wrangling  ----
df <- df[df$cohort == 1, ] # remove <60yo or number of chronic disease < 2, and duplicates
names(df)[names(df) == "chronic_diseasef0"] <- "CD"
df$support <- car::recode(to_character(df$efs4), "
'Always' = 2;
'Sometimes' = 1; 
'Never' = 0
")
df$female <- car::recode(to_character(df$gender), "
'Female' = 1;
'Male' = 0
")
df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)
df$age_group <- relevel(as.factor(df$age_group), ref = "60-69")
df$meaning <- as.numeric(df$meaning)
df <- df %>% filter(age>=60 | is.na(age))
df$eq5dvas <- as.numeric(df$eq5dvas)
df$sar <- ifelse(df$sarc_f >= 4, 1, 0)
df$gender <- ifelse(df$female == 1, "Female", "Male")
df <- droplevels(df) # drop empty categories in variables

dfwide <- reshape(data=df, idvar= c("case_id"),
                  timevar = "time",
                  v.names=c("date", 
                            "age", 
                            "eq5d",
                            "eq5dvas",
                            # "support", 
                            "efs4", 
                            "meaning",
                            "loneliness",
                            "loneliness_emo",
                            "loneliness_soc",
                            "gad",
                            # "gad_group",
                            "isi",
                            # "isi_group",
                            "phq", 
                            # "phq_group",
                            "moca",
                            "sarc_f",
                            "sar1",
                            "sar2",
                            "sar3",
                            "sar4",
                            "sar5",
                            "hgs1", # first measurement of hand-grip strength of left hand
                            "hgs2", # second measurement of hand-grip strength of left hand
                            "hgs3", # first measurement of hand-grip strength of right hand
                            "hgs4", # second measurement of hand-grip strength of right hand
                            "hgs_l",
                            "hgs_r",
                            "hgs",
                            "sar_hgs",
                            "mci", 
                            "mci_cat",
                            "efs",
                            "fs",
                            "waist",
                            "bmi",
                            "support",
                            "age_group",
                            "sar"),
                  direction="wide")

saveRDS(df, file = "t0t1t2_data.rds")
# write_excel("t0t1t2_data.xlsx", df)
saveRDS(dfwide, file = "t0t1t2_data_wide.rds")
