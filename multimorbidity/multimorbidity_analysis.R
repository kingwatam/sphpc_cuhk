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
library(labelled) # 

setwd(sprintf("~%s/multimorbidity", setpath))
# df <- readRDS("JC_covid_data_long.rds")
# write.csv(df, file = "JC_covid_data_long.csv")
# dfwide <- readRDS("JC_covid_data_wide.rds")
df <- readRDS("t0t1t2_data.rds")
dfwide <- readRDS("t0t1t2_data_wide.rds")

# df$efs4_hist2 <- car::recode(df$efs4_hist, "
# c('000', '111', '222') = 'No change';
# c('001', '002', '011', '012', '022', '112', '122') = 'Better';
# c('010', '020', '021', '120', '121') = 'Better then worse';
# c('100', '110', '200', '210', '211', '220', '221') = 'Worse';
# c('101', '102', '201', '202', '212') = 'Worse then better'
# ")
# df$efs4_hist2 <- relevel(as.factor(df$efs4_hist2), ref = 'No change')
df$efs4 <- relevel(to_factor(df$efs4), ref = 'Never')

# df$time <- as.numeric(df$time)

df$covid <- if_else(df$time == 2, 1, 0)
df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)
dfwide$age_group.2 <- recode_age(dfwide$age.2, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)

# descriptive statistics ----
allVars <- c("age", "age_group", "gender", "CD", "meaning", "support", "eq5d", "eq5dvas", "isi", "gad", "loneliness", "loneliness_emo", "loneliness_soc")
catVars <- c("age_group", "gender")
tableone::CreateTableOne(data =  df, strata = "time", vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

# preliminary analysis ----
reg_table <- function(data){
  table <- data.frame(matrix(ncol = 4,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  for (dep_var in c("sar_score.0", "sar_score.1", "sar_score.2")){
    
    for (var in c("moca.0", "moca.1")){
      
      if (!(var %in% unlist(table[1]))){
        table[row_count, 1] <- var
      } else {
        row_count <- match(var, unlist(table[1]))
      }
      
      print(paste(dep_var, var))
      
      colnames(table)[col_count] <- dep_var
      
      age_var <- paste0("age_group.", substr(dep_var, nchar(dep_var), nchar(dep_var)))
      
      iferror(fit <- eval_(
        "lm(", dep_var, "~", age_var, "+", "female+CD+", var, ", data = data)"
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
    col_count <- col_count + 1
  }
  return(table)
}

gen_table <- function(fit){
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
    
    beta <- iferror(summary(fit)$coef[var, 1], NA)
    se <-  iferror(summary(fit)$coef[var, 2], NA)
    lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
    upperCI <- iferror(beta + qnorm(0.975) * se, NA)
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

combind_tables <- function(table, ...){
  for (i in 1:length(list(...))){
    new_table <- gen_table(list(...)[[i]]) 
    dep_vars <- names(table)
    dep_var <- names(new_table)[2]
    names(table) <- c("X1",rep(2:ncol(table)))
    table <- plyr::join(table, new_table, by=c("X1"), type="full")
    names(table) <- c(dep_vars, dep_var)
    names(table)[names(table) == "X1"] <- ""
  }
  return(table)
}

# sar_score as dependent variable
table <- gen_table(lm(sar_score.0~ 1+age_group.0+female+CD+moca.0, data = dfwide))
table <- combind_tables(table,
                        lm(sar_score.1~ 1+age_group.1+female+CD+moca.1, data = dfwide),
                        lm(sar_score.1~ 1+age_group.1+female+CD+sar_score.0+moca.1, data = dfwide),
                        lm(sar_score.1~ 1+age_group.1+female+CD+moca.0, data = dfwide),
                        lm(sar_score.1~ 1+age_group.1+female+CD+sar_score.0+moca.0, data = dfwide),
                        lm(sar_score.2~ 1+age_group.2+female+CD+moca.1, data = dfwide),
                        lm(sar_score.2~ 1+age_group.2+female+CD+sar_score.0+moca.1, data = dfwide),
                        lm(sar_score.2~ 1+age_group.2+female+CD+sar_score.1+moca.1, data = dfwide),
                        lm(sar_score.2~ 1+age_group.2+female+CD+moca.0, data = dfwide),
                        lm(sar_score.2~ 1+age_group.2+female+CD+sar_score.0+moca.0, data = dfwide),
                        lm(sar_score.2~ 1+age_group.2+female+CD+sar_score.1+moca.0, data = dfwide) 
)

# moca as dependent variable
table <- gen_table(lm(moca.0~ 1+age_group.0+female+CD+ sar_score.0, data = dfwide) )
table <- combind_tables(table,
                        lm(moca.1~ 1+age_group.1+female+CD+sar_score.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sar_score.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sar_score.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sar_score.0, data = dfwide)
)

# longitudinal
table <- gen_table(lmer(sar_score~ 1+age_group+female+CD+moca+ (1| case_id) , REML = TRUE, data = df))
table <- combind_tables(table,
                        lmer(moca~ 1+age_group+female+CD+sar_score+ (1| case_id) , REML = TRUE, data = df)
)

# binary SARC-F & binary/categorical MCI
dfwide$mci.0 <- relevel(as.factor(dfwide$mci.0),ref="None") # for categorical MCI
dfwide$mci.1 <- relevel(as.factor(dfwide$mci.1),ref="None") # for categorical MCI

table <- gen_table(glm(sar.0~1+age_group.0+female+CD+mci.0, family = binomial, data =  dfwide))
table <- combind_tables(table,
                        glm(sar.1~1+age_group.1+female+CD+mci.1, family = binomial, data =  dfwide),
                        glm(sar.1~1+age_group.1+female+CD+sar.0+mci.1, family = binomial, data =  dfwide),
                        glm(sar.1~1+age_group.1+female+CD+mci.0, family = binomial, data =  dfwide),
                        glm(sar.1~1+age_group.1+female+CD+sar.0+mci.0, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+mci.1, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.1+mci.1, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.0+mci.1, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+mci.0, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.1+mci.0, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.0+mci.0, family = binomial, data =  dfwide)
)

# binary MCI as dependent variable
table <- gen_table(glm(mci.0~1+age_group.0+female+CD+sar.0, family = binomial, data =  dfwide))
table <- combind_tables(table,
                        glm(mci.1~1+age_group.1+female+CD+sar.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+sar.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+sar.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+sar.0, family = binomial, data =  dfwide)
)

table %>% clipr::write_clip()

df$time <- relevel(as.factor(df$time), ref = "1")
lmer(support~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(eq5d~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(eq5dvas~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(isi~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df[df$time %in% c(1,2),]) %>% summary() 
lmer(gad~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df[df$time %in% c(1,2),]) %>% summary() 
lmer(loneliness~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(loneliness_emo~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(loneliness_soc~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 

df$support_ <- car::recode(df$support, "1 = 0; 2 = 1")
glmer_fit <- glmer(support_~ 1+age_group+female+CD+time*meaning+ (1| case_id), family = binomial, data = df) 
summary(glmer_fit) 

# exploratory analysis (legacy) ----
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

# table <- reg_table2(dfwide)

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

df$age_group <- relevel(as.factor(df$age_group), ref = "60-69")
df$time <- relevel(as.factor(df$time), ref = "1")

lmer(support~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(eq5d~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(eq5dvas~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(isi~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df[df$time %in% c(1,2),]) %>% summary() 
lmer(gad~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df[df$time %in% c(1,2),]) %>% summary() 
lmer(loneliness~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(loneliness_emo~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 
lmer(loneliness_soc~ 1+age_group+female+CD+time*meaning+ (1| case_id) , REML = TRUE, data = df) %>% summary() 

df$support_ <- car::recode(df$support, "1 = 0; 2 = 1")
glmer_fit <- glmer(support_~ 1+age_group+female+CD+time*meaning+ (1| case_id), family = binomial, data = df) 
summary(glmer_fit) 

df$isi_group_ <- car::recode(df$isi_group, "c(1,2) = 0; c(3,4) = 1")
df$gad_group_ <- car::recode(df$gad_group, "c(1,2) = 0; c(3,4) = 1")

library(mixor)
df <- df[order(df$case_id),]
df <- droplevels(df) # drop unused levels
mixor_fit <- mixor(loneliness_emo ~  1+age_group+female+CD+time*meaning ,
                   data = df, id = case_id, link = "logit") 
summary(mixor_fit)

library(ordinal) # clm
count <- 2 # starting at 2 due to 1 sometimes would give erroneous highly significant p-value results
clmm_fit <- clmm(as.factor(support) ~ 1+age_group+female+CD+time*meaning + (1 | case_id), 
                 data = df, link="logit", Hess=TRUE, nAGQ=count) 
summary(clmm_fit)

library(multgee)
multgee_fit <- ordLORgee(support ~ 1+age_group+female+CD+time*meaning ,
                         data = df, id = case_id, repeated = time,  link = "logit") 
summary(multgee_fit)

# library(repolr)
# df <- df[order(df$time),]
# repolr(ordered(support) ~ 1,
#        subjects="case_id", data=na.omit(df), times=c(1,2,3), categories=3) %>% summary()

# library(geepack)
# df <- df[order(df$support),]
# ordgee(ordered(support) ~ 1+age_group+female+CD+time*meaning,
#        id = case_id, data = df_) %>% summary()

# library(MCMCglmm)
# df_ <- df %>% filter(!is.na(support) & !is.na(age_group) & !is.na(female) & !is.na(CD) & !is.na(time) & !is.na(meaning))
# mcmcglmm_fit <- MCMCglmm(support ~ 1+age_group+female+CD+time*meaning,
#                          data = df_, random = ~case_id,  family = "ordinal")
# summary(mcmcglmm_fit)

lmer(meaning~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df, ) %>% summary() 
lmer(eq5d~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df, ) %>% summary() 
lmer(isi~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df[df$time>=1,], ) %>% summary() 
lmer(gad~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df[df$time>=1,], ) %>% summary() 
lmer(loneliness~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df, ) %>% summary() 
lmer(loneliness_emo~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df, ) %>% summary() 
lmer(loneliness_soc~ 1+age_group+female+CD+time*support+ (1| case_id) , REML = TRUE, data = df, ) %>% summary() 

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

ggerrorplot(df, x = "time", y = "meaning",
            desc_stat = "mean_ci"
            , add = "mean", error.plot = "errorbar"
            , facet.by = c("female", "age_group")
) # %>% ggpar(ylim = c(0, 5.5))

ggplot(df, aes(x=meaning, y=support, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Social Support") +
    geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=support, y=meaning, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Social Support", y = "Meaning in life") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=meaning, y=eq5d, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Health-related quality of life (EQ5D-5L)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=meaning, y=eq5dvas, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Health-related quality of life (EQ-VAS)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df %>% filter(time >= 1), aes(x=meaning, y=isi, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Insomnia (ISI)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df %>% filter(time >= 1), aes(x=meaning, y=gad, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Stress (GAD-7)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df %>% filter(time >= 0), aes(x=meaning, y=loneliness, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Loneliness (DJG)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df %>% filter(time >= 0), aes(x=meaning, y=loneliness_emo, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Emotional loneliness (DJG)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df %>% filter(time >= 0), aes(x=meaning, y=loneliness_soc, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Social loneliness (DJG)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df %>% filter(time >= 0), aes(x=meaning, y=phq, color=time, shape=time)) +
  # geom_point() +
  labs(x = "Meaning in life", y = "Depression (PHQ)") +
  geom_smooth(method=lm, aes(fill=time))

ggplot(df, aes(x=date, y=time, color=time, shape=time)) +
  # geom_point() +
  geom_smooth(method=lm, aes(fill=time))

# plot data collection dates
ggplot(df, aes(x=date, y=time)) + 
  geom_point() + 
  scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", date_labels="%b/%Y") +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1))
