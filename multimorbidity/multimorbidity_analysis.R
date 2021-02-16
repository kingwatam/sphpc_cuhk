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
df <- readRDS("JC_covid_data_long.rds")
dfwide <- readRDS("JC_covid_data_wide.rds")

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
                    "gad", "gad_group", "isi", "isi_group", "phq", "phq_group")){
    
    dep_var <- paste0(dep_var, ".2")
    
    for (var in c("meaning", "support", "eq5d", "eq5dvas", "moca", "mci",
                  "loneliness", "loneliness_emo", "loneliness_soc", 
                  "gad", "gad_group", "isi", "isi_group", "phq", "phq_group")){
      var_original <- var
      
      # create difference variables
      eval_("data$", var, ".dif10", " <- ", "data$", var, ".1 -", "data$", var, ".0")
      eval_("data$", var, ".dif21", " <- ", "data$", var, ".2 -", "data$", var, ".1")
      eval_("data$", var, ".dif20", " <- ", "data$", var, ".2 -", "data$", var, ".0")

      for (suffix in c(".dif10", ".dif21", ".dif20")){
        var <- paste0(var_original, suffix)
        
        # var <- paste0(var_original, ".2") # instead of difference
        
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
  table <- data.frame(matrix(ncol = 16,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  for (dep_var in c("meaning", "support", "eq5d", "eq5dvas",
                    "loneliness", "loneliness_emo", "loneliness_soc", 
                    "gad", "gad_group", "isi", "isi_group", "phq", "phq_group",
                    "moca", "mci")){
    
    # create difference variables
    eval_("data$", dep_var, ".dif10", " <- ", "data$", dep_var, ".1 -", "data$", dep_var, ".0")
    eval_("data$", dep_var, ".dif21", " <- ", "data$", dep_var, ".2 -", "data$", dep_var, ".1")
    eval_("data$", dep_var, ".dif20", " <- ", "data$", dep_var, ".2 -", "data$", dep_var, ".0")
    
    dep_var <- paste0(dep_var, ".dif10")
    
    for (var in c("meaning", "support", "eq5d", "eq5dvas", 
                  "loneliness", "loneliness_emo", "loneliness_soc", 
                  "gad", "gad_group", "isi",  "isi_group", "phq", "phq_group",
                  "moca", "mci")){
      var_original <- var
      
      eval_("data$", var, ".dif10", " <- ", "data$", var, ".1 -", "data$", var, ".0")
      eval_("data$", var, ".dif21", " <- ", "data$", var, ".2 -", "data$", var, ".1")
      eval_("data$", var, ".dif20", " <- ", "data$", var, ".2 -", "data$", var, ".0")
      
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

table <- reg_table2(dfwide)

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

lm(isi.dif21~ 1+age.2+female+cssa+CD+
     mci.dif10+support.dif21+loneliness.dif21+phq.dif21, data = dfwide) %>% summary()

lm(meaning.dif21~ 1+age.2+female+cssa+CD+isi.dif21+
     mci.dif10+support.dif21+loneliness.dif21+phq.dif21, data = dfwide) %>% summary()

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
ggline(df, x = "time", y = "mci", add = "mean_ci") # %>% ggpar(ylim = c(0, 5.5))

ggerrorplot(df, x = "time", y = "support",
            desc_stat = "mean_ci"
            , add = "mean", error.plot = "errorbar"
) # %>% ggpar(ylim = c(0, 5.5))

ggplot(df, aes(x=time, y=meaning, color=support, shape=support)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=support))

ggplot(df, aes(x=meaning, y=eq5dvas, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=meaning, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=support, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=eq5d, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Number of chronic conditions", y = "EQ5D-5L Score") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=CD, y=eq5dvas, color=time, shape=time)) +
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
