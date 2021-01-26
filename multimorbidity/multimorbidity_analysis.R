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

setwd(sprintf("~%s/multimorbidity", setpath))
df <- haven::read_sav("JC_covid_data_Jul_Wide_20200728.sav")
full <- read.dta("jc_acitivity_FU1 2019Mar26 v2.dta") # baseline (bl, f0) & first follow-up (f1)

df <- merge(df, full[, c("case_id", "efs4f0", "MOCA_total_bl", "MOCA_totalf1")], # extract social support item matched by case ID
              by=c("case_id"), all.x = TRUE) # MOCA_total_bl & MOCA_totalf0 are identical

# data cleaning ----
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

# df$efs4_hist2 <- car::recode(df$efs4_hist, "
# c('000', '111', '222') = 'No change';
# c('001', '002', '011', '012', '022', '112', '122') = 'Better';
# c('010', '020', '021', '120', '121') = 'Better then worse';
# c('100', '110', '200', '210', '211', '220', '221') = 'Worse';
# c('101', '102', '201', '202', '212') = 'Worse then better'
# ")
# df$efs4_hist2 <- relevel(as.factor(df$efs4_hist2), ref = 'No change')
df$efs4 <- relevel(as.factor(df$efs4), ref = 'Never')

# df$time <- as.numeric(df$time)

df$covid <- if_else(df$time == 2, 1, 0)

# main analysis ----
reg_table <- function(data){
  table <- data.frame(matrix(ncol = 15,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  for (dep_var in c("meaning", "support", "eq5d", "eq5dvas", 
                    # "moca", "mci",
                    "loneliness", "loneliness_emo", "loneliness_soc", 
                    "gad", "gad_group", "isi", "phq", "phq_group")){
    
    dep_var <- paste0(dep_var, ".2")
    
    for (var in c("meaning", "support", "eq5d", "eq5dvas", "moca", "mci",
                  "loneliness", "loneliness_emo", "loneliness_soc", 
                  "gad", "gad_group", "isi", "phq", "phq_group")){
      var_original <- var
      
      # create difference variables
      eval_("data$", var, ".dif10", " <- ", "data$", var, ".1 -", "data$", var, ".0")
      eval_("data$", var, ".dif21", " <- ", "data$", var, ".2 -", "data$", var, ".1")
      eval_("data$", var, ".dif20", " <- ", "data$", var, ".2 -", "data$", var, ".0")

      for (suffix in c(".dif10", ".dif21", ".dif20")){
        var <- paste0(var_original, suffix)
        
        # var <- paste0(var_original, ".1")
        
        if (!(var %in% unlist(table[1]))){
          table[row_count, 1] <- var
        } else {
          row_count <- match(var, unlist(table[1]))
        }
        
        print(paste(dep_var, var))
        
        colnames(table)[col_count] <- dep_var
        
        iferror(fit <- eval_(
          "lm(", dep_var, "~", var, ", data = data)"
        ), next)
        
        n <- iferror(nobs(fit), NA)
        beta <- iferror(summary(fit)$coef[var, 1], NA)
        se <-  iferror(summary(fit)$coef[var, 2], NA)
        lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
        upperCI <- iferror(beta + qnorm(0.975) * se, NA)
        p_value <- iferror(summary(fit)$coef[var, 4], NA)
        
        # table[row_count, col_count] <-  paste0(n, ", ", starred_p(p_value, 3, beta))
        table[row_count, col_count] <-  starred_p(p_value, 3, beta)
        
        row_count <- row_count + 1
      }
    }
    col_count <- col_count + 1
  }
  return(table)
}

reg_table2 <- function(data){ # change as dependent variable
  table <- data.frame(matrix(ncol = 15,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  for (dep_var in c("meaning", "support", "eq5d", "eq5dvas", "moca", "mci",
                    "loneliness", "loneliness_emo", "loneliness_soc", 
                    "gad", "gad_group", "isi", "phq", "phq_group")){
    
    # create difference variables
    eval_("data$", dep_var, ".dif10", " <- ", "data$", dep_var, ".1 -", "data$", dep_var, ".0")
    eval_("data$", dep_var, ".dif21", " <- ", "data$", dep_var, ".2 -", "data$", dep_var, ".1")
    eval_("data$", dep_var, ".dif20", " <- ", "data$", dep_var, ".2 -", "data$", dep_var, ".0")
    
    dep_var <- paste0(dep_var, ".dif21")
    
    for (var in c("meaning", "support", "eq5d", "eq5dvas", "moca", "mci",
                  "loneliness", "loneliness_emo", "loneliness_soc", 
                  "gad", "gad_group", "isi", "phq", "phq_group")){
      var_original <- var
      
      for (suffix in c(".dif10", ".dif21", ".dif20")){
        var <- paste0(var_original, suffix)

        if (var == dep_var){next}
        
        # var <- paste0(var_original, ".1")
        
        if (!(var %in% unlist(table[1]))){
          table[row_count, 1] <- var
        } else {
          row_count <- match(var, unlist(table[1]))
        }
        
        print(paste(dep_var, var))
        
        colnames(table)[col_count] <- dep_var
        
        iferror(fit <- eval_(
          "lm(", dep_var, "~", var, ", data = data)"
        ), next)
        
        n <- iferror(nobs(fit), NA)
        beta <- iferror(summary(fit)$coef[var, 1], NA)
        se <-  iferror(summary(fit)$coef[var, 2], NA)
        lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
        upperCI <- iferror(beta + qnorm(0.975) * se, NA)
        p_value <- iferror(summary(fit)$coef[var, 4], NA)
        
        # table[row_count, col_count] <-  paste0(n, ", ", starred_p(p_value, 3, beta))
        table[row_count, col_count] <-  iferror(starred_p(p_value, 3, beta), NA)
        
        rm(fit, beta, p_value)
        row_count <- nrow(table) + 1
      }
    }
    col_count <- col_count + 1
  }
  return(table)
}

table2 <- reg_table2(dfwide)

dfwide$mci_hist <- ifelse(dfwide$mci.0 %in% NA | dfwide$mci.1 %in% NA, NA,
                       paste(dfwide$mci.0, dfwide$mci.1, sep = ""))


# generate difference variables
for (var in c("meaning", "support", "eq5d", "eq5dvas", "moca", "mci",
              "loneliness", "loneliness_emo", "loneliness_soc", 
              "gad", "gad_group", "isi", "phq", "phq_group")){
  
  eval_("dfwide$", var, ".dif10", " <- ", "dfwide$", var, ".1 -", "dfwide$", var, ".0")
  eval_("dfwide$", var, ".dif21", " <- ", "dfwide$", var, ".2 -", "dfwide$", var, ".1")
  eval_("dfwide$", var, ".dif20", " <- ", "dfwide$", var, ".2 -", "dfwide$", var, ".0")
}

lm(isi.2~ 1+age.2+female+cssa+CD+
     mci.dif10+support.dif21+meaning.dif21, data = dfwide) %>% summary()

lm(eq5d.2~ 1+age.2+female+cssa+CD+
     gad_group.dif21, data = dfwide) %>% summary()

lmer(meaning~ 1+date+CD+moca+ (1| case_id) ,
     REML = TRUE, data = df, ) %>% summary()

lmer(EQ5D~ 1+date+CD+CD:covid+ (1| case_id) ,
     REML = TRUE, data = df, ) %>% summary()

nlme::lme(EQ5D~ 1+time+meaning+meaning:covid+support+support:covid+CD+CD:covid, random =~ 1 | case_id, 
                 nlme::corAR1(form = ~ 1 | case_id),
                 data = df, na.action=na.omit) %>% summary()
# nlme::ACF(fit)

lmer(EQ5D~ 1+time+CD+CD:covid+ (1| case_id) ,
     REML = TRUE, data = df) %>% summary()

lmer(meaning~ 1+time+age+female+cssa+alone+EFS4_hist3+ (1 | case_id) ,
     REML = TRUE, data = df) %>% summary()

lm(meaning.2~ 1+meaning.1+age.2+female+cssa+alone+support.1+support.2, data = dfwide) %>% summary()

# LM charts ----
ggplot(df, aes(x=CD, y=meaning, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=support, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=EQ5D, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Number of chronic conditions", y = "EQ5D-5L Score") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=EQVAS, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Number of chronic conditions", y = "EQ-VAS") +
  geom_smooth(method=lm, aes(fill=time))


ggplot(df, aes(x=date, y=time, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

# plot data collection dates
ggplot(df, aes(x=date, y=time)) + 
  geom_point() + 
  scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", date_labels="%b/%Y") +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1))
