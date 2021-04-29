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
library(patchwork) # combine ggplots using "+"

setwd(sprintf("~%s/multimorbidity", setpath))
df <- readRDS("t0t1t2_data.rds")
dfwide <- readRDS("t0t1t2_data_wide.rds")

df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)

gen_table <- function(fit){
  table <- data.frame(matrix(ncol = 2,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  dep_var <- as.character(formula(fit)[2])
  
  if (!is.null(summary(fit)$isLmer) | class(fit)[1] %in% "glmerMod"){ # check if linear mixed model (lmer)
    n <- iferror(nobs(fit), NA)
    n_unique <- iferror(summary(fit)$ngrps, NA)
    table[row_count, 1] <- "N (unique)"
    table[row_count, col_count] <-  paste0(n, " (", n_unique, ")")
    row_count <- row_count + 1
    
    icc <- iferror(performance::icc(fit)[[1]], NA)
    icc <- round_format(icc, 3)
    table[row_count, 1] <- "ICC"
    table[row_count, col_count] <-  icc
    row_count <- row_count + 1
  } else if (class(fit)[1] %in% "glm") {
    n <- iferror(nobs(fit), NA)
    table[row_count, 1] <- "N"
    table[row_count, col_count] <-  n
    row_count <- row_count + 1
    
    adj_r2 <- iferror(1 - ((summary(fit)$deviance/-2)-(length(fit$coeff)-1)) / (summary(fit)$null.deviance/-2), NA)
    adj_r2 <- round_format(adj_r2, 3)
    table[row_count, 1] <- "Adjusted R2"
    table[row_count, col_count] <-  adj_r2
    row_count <- row_count + 1
  } else {
    n <- iferror(nobs(fit), NA)
    table[row_count, 1] <- "N"
    table[row_count, col_count] <-  n
    row_count <- row_count + 1
    
    adj_r2 <- iferror(round_format(summary(fit)$adj.r.squared, 3), NA)
    table[row_count, 1] <- "Adjusted R2"
    table[row_count, col_count] <-  adj_r2
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
    if (!is.null(summary(fit)$isLmer) & !(class(fit)[1] %in% "glmerMod")){
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

dfwide$sarc_f.10 <- dfwide$sarc_f.1 - dfwide$sarc_f.0
dfwide$sarc_f.21 <- dfwide$sarc_f.2 - dfwide$sarc_f.1
dfwide$moca.10 <- dfwide$moca.1 - dfwide$moca.0
dfwide$hgs.10 <- dfwide$hgs.1 - dfwide$hgs.0

dfwide$sarc_f_mean.10 <- rowMeans(cbind(dfwide$sarc_f.1, dfwide$sarc_f.0))
dfwide$sarc_f_mean.21 <- rowMeans(cbind(dfwide$sarc_f.2, dfwide$sarc_f.1))
dfwide$moca_mean.10 <- rowMeans(cbind(dfwide$moca.1,dfwide$moca.0))
dfwide$hgs_mean.10 <- rowMeans(cbind(dfwide$hgs.1,dfwide$hgs.0))

## charts ----
# MoCA vs SARC-F vs HGS histograms ----
hist1 <- ggplot(dfwide, aes(x=moca.0)) + geom_histogram() 
hist4 <- ggplot(dfwide, aes(x=moca.1)) + geom_histogram() 
hist2 <- ggplot(dfwide, aes(x=sarc_f.0)) + geom_histogram() 
hist5 <- ggplot(dfwide, aes(x=sarc_f.1)) + geom_histogram() 
hist3 <- ggplot(dfwide, aes(x=hgs.0)) + geom_histogram() 
hist6 <- ggplot(dfwide, aes(x=hgs.1)) + geom_histogram() 
hist1 + hist2 + hist3 + hist4 + hist5 + hist6 

get_plot_main <- function(df, x, y, xlab, ylab, jitter_w = 0, jitter_h = 0, 
                     yintercept = NULL, xintercept = NULL, text_size = 4){
  plot <- ggplot(df, aes_string(x=x, y=y) ) +
    geom_smooth(method=lm, color="black", formula = y~x) +
    ggpmisc::stat_poly_eq(formula = y~x, 
                          aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                          parse = TRUE) +
    # geom_bin2d(bins =  50) + # stat_bin2d(bins=50, aes(fill = ..density..))  
    geom_jitter(width = jitter_w, height = jitter_h) +
    xlab(xlab) + ylab(ylab) +
    geom_hline(yintercept = yintercept) + geom_vline(xintercept = xintercept) +
    theme(text = element_text(size=rel(text_size)),
          strip.text.x = element_text(size=rel(text_size*0.85)),
          strip.text.y = element_text(size=rel(text_size*0.85)))
  return(plot)
}

get_plot <- function(df, x, y, jitter_w = 0, jitter_h = 0, 
                     yintercept = NULL, xintercept = NULL, text_size = 4){
  return(get_plot_main(df, 
                       x = x, y = y, 
                       xlab = varlist[varlist$V1==x,2], ylab = varlist[varlist$V1==y,2],
                       jitter_w = jitter_w, jitter_h = jitter_h, 
                       yintercept = yintercept, xintercept = xintercept, text_size = text_size))
}

varlist <- t(array(c(c("moca.0", "MoCA in T0 (higher =  better)"), 
                     c("moca.1", "MoCA in T1 (higher =  better)"),  
                     c("sarc_f.0", "SARC-F in T0 (lower = better)"),  
                     c("sarc_f.1", "SARC-F in T1 (lower = better)"),  
                     c("hgs.0", "Handgrip strength in T0 (higher = better)"),  
                     c("hgs.1", "Handgrip strength in T1 (higher = better)"), 
                     c("sarc_f.10", "Change in SARC-F (T1 minus T0, lower = better)"),
                     c("hgs.10", "Change in handgrip strength (T1 minus T0, higher = better)"),
                     c("moca.10", "Change in MoCA (T1 minus T0, higher = better)")
                     ),
                   dim = c(2,9))) %>% as.data.frame()

# MoCA vs SARC-F vs HGS scatterplots (different x & y) ----
plot1 <- get_plot(dfwide, 
                        x = "moca.0", y = "sarc_f.0",
                        jitter_w = 0.25, jitter_h = 0.25)
plot2 <- get_plot(dfwide, 
                        x = "moca.1", y = "sarc_f.1",
                        jitter_w = 0.25, jitter_h = 0.25)
plot3 <- get_plot(dfwide, 
                        x = "moca.0", y = "hgs.0",
                        jitter_w = 0.25, jitter_h = 0)
plot4 <- get_plot(dfwide, 
                        x = "moca.1", y = "hgs.1",
                        jitter_w = 0.25, jitter_h = 0)
plot1+plot2+plot3+plot4

plot5 <- get_plot(dfwide, 
                        x = "moca.0", y = "sarc_f.1",
                        jitter_w = 0.25, jitter_h = 0.25)
plot6 <- get_plot(dfwide, 
                         x = "moca.0", y = "hgs.1",
                         jitter_w = 0.25, jitter_h = 0)

plot1+plot2+plot5+plot3+plot4+plot6

get_plot(dfwide, 
         x = "sarc_f.0", y = "hgs.0",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)
get_plot(dfwide, 
         x = "sarc_f.1", y = "hgs.1",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)


## baseline vs change ----
get_plot(dfwide, 
         x = "moca.0", y = "sarc_f.10",
         jitter_w = 0.25, jitter_h = 0.25, text_size = 4)
get_plot(dfwide, 
         x = "moca.0", y = "hgs.10",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)
get_plot(dfwide, 
         x = "sarc_f.0", y = "sarc_f.10",
         jitter_w = 0.25, jitter_h = 0.25, text_size = 4)
get_plot(dfwide, 
         x = "hgs.0", y = "hgs.10",
         jitter_w = 0, jitter_h = 0, text_size = 4)
get_plot(dfwide, 
         x = "moca.0", y = "moca.10",
         jitter_w = 0.25, jitter_h = 0.25, text_size = 4)
get_plot(dfwide, 
         x = "hgs.0", y = "sarc_f.10",
         jitter_w = 0, jitter_h = 0.25, text_size = 4)
get_plot(dfwide, 
         x = "sarc_f.0", y = "hgs.10",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)

# change in MoCA vs change in SARC-F ----
get_plot(dfwide, 
         x = "moca.10", y = "sarc_f.10",
         jitter_w = 0.25, jitter_h = 0.25, text_size = 4)
get_plot(dfwide, 
         x = "sarc_f.10", y = "moca.10",
         jitter_w = 0.25, jitter_h = 0.25, text_size = 4)

# change in MoCA vs change in HGS ----
get_plot(dfwide, 
         x = "moca.10", y = "hgs.10",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)
get_plot(dfwide, 
         x = "hgs.10", y = "moca.10",
         jitter_w = 0, jitter_h = 0.25, text_size = 4)

# SARC-F change vs mean SARC-F (BA plot) ----
test <- dfwide %>% filter(!is.na(sarc_f.1) & !is.na(sarc_f.0))
BlandAltmanLeh::bland.altman.plot(test$sarc_f.1, test$sarc_f.0, graph.sys = "ggplot2", geom_count=TRUE) +
  xlab("Mean of SARC-F at T0 & T1 (lower = better)") + ylab("Difference (T1 minus T0, lower = better)") +
  theme(text = element_text(size=rel(4)),
          strip.text.x = element_text(size=rel(3.5)),
          strip.text.y = element_text(size=rel(3.5)))
  
ggplot(dfwide, aes(x=sarc_f_mean.10, y=sarc_f.10 )) +
  # geom_bin2d(bins =  50) + # stat_bin2d(bins=50, aes(fill = ..density..))  
  geom_jitter(width = 0.25, height = 0.25) +
  xlab("Mean of SARC-F at T0 & T1 (lower = better)") + ylab("Difference (T1 minus T0, lower = better)") +
  geom_hline(yintercept = mean(dfwide$sarc_f.10, na.rm = TRUE), linetype = "dashed") + 
  geom_hline(yintercept = mean(dfwide$sarc_f.10, na.rm = TRUE)+1.96*sd(dfwide$sarc_f.10, na.rm = TRUE), linetype = "dashed") +
  geom_hline(yintercept = mean(dfwide$sarc_f.10, na.rm = TRUE)-1.96*sd(dfwide$sarc_f.10, na.rm = TRUE), linetype = "dashed") + 
  theme(text = element_text(size=rel(4)),
        strip.text.x = element_text(size=rel(3.5)),
        strip.text.y = element_text(size=rel(3.5)))

# MoCA BA-plot ----
test <- dfwide %>% filter(!is.na(moca.1) & !is.na(moca.0))
BlandAltmanLeh::bland.altman.plot(test$moca.1, test$moca.0, graph.sys = "ggplot2", geom_count=TRUE) +
  xlab("Mean of MoCA at T0 & T1 (higher = better)") + ylab("Difference (T1 minus T0, higher = better)") +
  theme(text = element_text(size=rel(4)),
        strip.text.x = element_text(size=rel(3.5)),
        strip.text.y = element_text(size=rel(3.5)))

ggplot(dfwide, aes(x=moca_mean.10, y=moca.10 )) +
  # geom_bin2d(bins =  50) + # stat_bin2d(bins=50, aes(fill = ..density..))  
  geom_jitter(width = 0.25, height = 0.25) +
  xlab("Mean of MoCA at T0 & T1 (higher = better)") + ylab("Difference (T1 minus T0, higher = better)") +
  geom_hline(yintercept = mean(dfwide$moca.10, na.rm = TRUE), linetype = "dashed") + 
  geom_hline(yintercept = mean(dfwide$moca.10, na.rm = TRUE)+1.96*sd(dfwide$moca.10, na.rm = TRUE), linetype = "dashed") +
  geom_hline(yintercept = mean(dfwide$moca.10, na.rm = TRUE)-1.96*sd(dfwide$moca.10, na.rm = TRUE), linetype = "dashed") + 
  theme(text = element_text(size=rel(4)),
        strip.text.x = element_text(size=rel(3.5)),
        strip.text.y = element_text(size=rel(3.5)))

# HGS BA-plot ----
test <- dfwide %>% filter(!is.na(hgs.1) & !is.na(hgs.0))
BlandAltmanLeh::bland.altman.plot(test$hgs.1, test$hgs.0, graph.sys = "ggplot2") +
  xlab("Mean of handgrip strength at T0 & T1 (higher = better)") + ylab("Difference (T1 minus T0, higher = better)") +
  theme(text = element_text(size=rel(4)),
        strip.text.x = element_text(size=rel(3.5)),
        strip.text.y = element_text(size=rel(3.5)))

ggplot(dfwide, aes(x=hgs_mean.10, y=hgs.10 )) +
  # geom_bin2d(bins =  50) + # stat_bin2d(bins=50, aes(fill = ..density..))  
  geom_jitter(width = 0.25, height = 0.25) +
  xlab("Mean of handgrip strength at T0 & T1 (higher = better)") + ylab("Difference (T1 minus T0, higher = better)") +
  geom_hline(yintercept = mean(dfwide$hgs.10, na.rm = TRUE), linetype = "dashed") + 
  geom_hline(yintercept = mean(dfwide$hgs.10, na.rm = TRUE)+1.96*sd(dfwide$hgs.10, na.rm = TRUE), linetype = "dashed") +
  geom_hline(yintercept = mean(dfwide$hgs.10, na.rm = TRUE)-1.96*sd(dfwide$hgs.10, na.rm = TRUE), linetype = "dashed") + 
  theme(text = element_text(size=rel(4)),
        strip.text.x = element_text(size=rel(3.5)),
        strip.text.y = element_text(size=rel(3.5)))

# correlation matrix ----
cor_matrix <- dfwide %>% select(sarc_f.0, sarc_f.1, sarc_f.2,
                                moca.0, moca.1, 
                                hgs.0, hgs.1,
                                sarc_f.10, sarc_f.21,
                                moca.10,
                                hgs.10) %>% cor(method = "pearson", use = "pairwise") 
cor_matrix[upper.tri(cor_matrix, diag = FALSE)]<-""
cor_matrix %>% clipr::write_clip()

## regressions ----
# difference as explanatory variable ----
table <- gen_table(lm(moca.10~ 1+age_group.0+female+CD+moca.0+sarc_f.0+sarc_f.10, data = dfwide))
table <- combind_tables(table,
                        lm(moca.10~ 1+age_group.0+female+CD+sarc_f.0+sarc_f.10, data = dfwide),
                        lm(sarc_f.10~ 1+age_group.0+female+CD+sarc_f.0+moca.0+moca.10, data = dfwide),
                        lm(sarc_f.10~ 1+age_group.0+female+CD+moca.0+moca.10, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+CD+moca.0+sarc_f.0+sarc_f.10, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.0+female+CD+sarc_f.0+moca.0+moca.10, data = dfwide),
                        lm(sarc_f.10~ 1+age_group.0+female+CD+sarc_f_mean.10, data = dfwide),
                        lm(moca.10~ 1+age_group.0+female+CD+sarc_f_mean.10, data = dfwide)
)

# moca as dependent variable ----
table <- gen_table(lm(moca.0~ 1+age_group.0+female+CD+sarc_f.0, data = dfwide) )
table <- combind_tables(table,
                        lm(moca.1~ 1+age_group.1+female+CD+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sarc_f.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sarc_f.0, data = dfwide)
)

table <- gen_table(lm(moca.0~ 1+age_group.0+female+CD+hgs.0, data = dfwide) )
table <- combind_tables(table,
                        lm(moca.1~ 1+age_group.1+female+CD+hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+hgs.0, data = dfwide),
                        
                        lm(moca.0~ 1+age_group.0+female+CD+sar_hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sar_hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sar_hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sar_hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sar_hgs.0, data = dfwide)
)

# hgs & sar_hgs as dependent variable ----
table <- gen_table(lm(hgs.0~ 1+age_group.0+female+CD+moca.0, data = dfwide))
table <- combind_tables(table,
                        lm(hgs.1~ 1+age_group.1+female+CD+moca.1, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+hgs.0+moca.1, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+hgs.0+moca.0, data = dfwide) 
)

table <- gen_table(glm(sar_hgs.0~ 1+age_group.0+female+CD+moca.0, family = binomial, data = dfwide))
table <- combind_tables(table,
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+moca.1, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+sar_hgs.0+moca.1, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+moca.0, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+sar_hgs.0+moca.0, family = binomial, data = dfwide) 
)

# sarc_f as dependent variable ----
table <- gen_table(lm(sarc_f.0~ 1+age_group.0+female+CD+moca.0, data = dfwide))
table <- combind_tables(table,
                        lm(sarc_f.1~ 1+age_group.1+female+CD+moca.1, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.1+female+CD+sarc_f.0+moca.1, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.1+female+CD+moca.0, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.1+female+CD+sarc_f.0+moca.0, data = dfwide),
                        lm(sarc_f.2~ 1+age_group.2+female+CD+moca.1, data = dfwide),
                        lm(sarc_f.2~ 1+age_group.2+female+CD+sarc_f.0+moca.1, data = dfwide),
                        lm(sarc_f.2~ 1+age_group.2+female+CD+sarc_f.1+moca.1, data = dfwide),
                        lm(sarc_f.2~ 1+age_group.2+female+CD+moca.0, data = dfwide),
                        lm(sarc_f.2~ 1+age_group.2+female+CD+sarc_f.0+moca.0, data = dfwide),
                        lm(sarc_f.2~ 1+age_group.2+female+CD+sarc_f.1+moca.0, data = dfwide) 
)

# longitudinal ----
table <- gen_table(lmer(sarc_f~ 1+age_group+female+CD+moca+ (1| case_id) , REML = TRUE, data = df))
table <- combind_tables(table,
                        glmer(sar~ 1+age_group+female+CD+moca+ (1| case_id), family = binomial, data = df),
                        lmer(hgs~ 1+age_group+female+CD+moca+ (1| case_id) , REML = TRUE, data = df),
                        glmer(sar_hgs~ 1+age_group+female+CD+moca+ (1| case_id), family = binomial, data = df),
                        lmer(moca~ 1+age_group+female+CD+sarc_f+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1+age_group+female+CD+sar+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1+age_group+female+CD+hgs+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1+age_group+female+CD+sar_hgs+ (1| case_id) , REML = TRUE, data = df),
                        glmer(mci~ 1+age_group+female+CD+sarc_f+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1+age_group+female+CD+sar+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1+age_group+female+CD+hgs+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1+age_group+female+CD+sar_hgs+ (1| case_id), family = binomial, data = df)
)


table <- gen_table(lmer(sarc_f~ 1+age_group+female +moca+ (1| case_id) , REML = TRUE, data = df))
table <- combind_tables(table,
                        glmer(sar~ 1+age_group+female +moca+ (1| case_id), family = binomial, data = df),
                        lmer(hgs~ 1+age_group+female +moca+ (1| case_id) , REML = TRUE, data = df),
                        glmer(sar_hgs~ 1+age_group+female +moca+ (1| case_id), family = binomial, data = df),
                        lmer(moca~ 1+age_group+female +sarc_f+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1+age_group+female +sar+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1+age_group+female +hgs+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1+age_group+female +sar_hgs+ (1| case_id) , REML = TRUE, data = df),
                        glmer(mci~ 1+age_group+female +sarc_f+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1+age_group+female +sar+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1+age_group+female +hgs+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1+age_group+female +sar_hgs+ (1| case_id), family = binomial, data = df)
)

table <- gen_table(lmer(sarc_f~ 1  +moca+ (1| case_id) , REML = TRUE, data = df))
table <- combind_tables(table,
                        glmer(sar~ 1  +moca+ (1| case_id), family = binomial, data = df),
                        lmer(hgs~ 1  +moca+ (1| case_id) , REML = TRUE, data = df),
                        glmer(sar_hgs~ 1  +moca+ (1| case_id), family = binomial, data = df),
                        lmer(moca~ 1  +sarc_f+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1  +sar+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1  +hgs+ (1| case_id) , REML = TRUE, data = df),
                        lmer(moca~ 1  +sar_hgs+ (1| case_id) , REML = TRUE, data = df),
                        glmer(mci~ 1  +sarc_f+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1  +sar+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1  +hgs+ (1| case_id), family = binomial, data = df),
                        glmer(mci~ 1  +sar_hgs+ (1| case_id), family = binomial, data = df)
)



test <- df
table <- gen_table(lmer(sarc_f~ 1 + (1| case_id) , REML = TRUE, data = test))
table <- combind_tables(table,
                        glmer(sar~ 1 + (1| case_id), family = binomial, data = test),
                        lmer(hgs~ 1 + (1| case_id) , REML = TRUE, data = test),
                        glmer(sar_hgs~ 1 + (1| case_id), family = binomial, data = test),
                        lmer(moca~ 1 + (1| case_id) , REML = TRUE, data = test),
                        glmer(mci~ 1 + (1| case_id), family = binomial, data = test)
)


# binary SARC-F & binary/categorical MCI ----
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

dfwide$mci_cat.0 <- relevel(as.factor(dfwide$mci_cat.0),ref="None") # for categorical MCI
dfwide$mci_cat.1 <- relevel(as.factor(dfwide$mci_cat.1),ref="None") # for categorical MCI

table <- gen_table(glm(sar.0~1+age_group.0+female+CD+mci_cat.0, family = binomial, data =  dfwide))
table <- combind_tables(table,
                        glm(sar.1~1+age_group.1+female+CD+mci_cat.1, family = binomial, data =  dfwide),
                        glm(sar.1~1+age_group.1+female+CD+sar.0+mci_cat.1, family = binomial, data =  dfwide),
                        glm(sar.1~1+age_group.1+female+CD+mci_cat.0, family = binomial, data =  dfwide),
                        glm(sar.1~1+age_group.1+female+CD+sar.0+mci_cat.0, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+mci_cat.1, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.1+mci_cat.1, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.0+mci_cat.1, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+mci_cat.0, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.1+mci_cat.0, family = binomial, data =  dfwide),
                        glm(sar.2~1+age_group.2+female+CD+sar.0+mci_cat.0, family = binomial, data =  dfwide)
)

# binary MCI as dependent variable ----
table <- gen_table(glm(mci.0~1+age_group.0+female+CD+sar.0, family = binomial, data =  dfwide))
table <- combind_tables(table,
                        glm(mci.1~1+age_group.1+female+CD+sar.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+sar.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+sar.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+sar.0, family = binomial, data =  dfwide),
                        
                        glm(mci.0~1+age_group.1+female+CD+hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+hgs.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+hgs.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+hgs.0, family = binomial, data =  dfwide),
                        
                        glm(mci.0~1+age_group.1+female+CD+sar_hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+sar_hgs.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+sar_hgs.1, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+sar_hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.1+female+CD+mci.0+sar_hgs.0, family = binomial, data =  dfwide)
)

# R2 comparisons (moca) ----
table <- gen_table(lm(moca.0~ 1+age_group.0, data = dfwide) )
table <- combind_tables(table,
                        lm(moca.0~ 1+female, data = dfwide),
                        lm(moca.0~ 1+CD, data = dfwide),
                        lm(moca.0~ 1+sarc_f.0, data = dfwide),
                        lm(moca.0~ 1+age_group.0+female+CD+sarc_f.0, data = dfwide),
                        lm(moca.0~ 1+hgs.0, data = dfwide),
                        lm(moca.0~ 1+age_group.0+female+CD+hgs.0, data = dfwide),
                        lm(moca.0~ 1+sar_hgs.0, data = dfwide),
                        lm(moca.0~ 1+age_group.0+female+CD+sar_hgs.0, data = dfwide),
                        
                        lm(moca.1~ 1+female, data = dfwide),
                        lm(moca.1~ 1+CD, data = dfwide),
                        lm(moca.1~ 1+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+moca.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sarc_f.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sarc_f.0, data = dfwide),
                        
                        lm(moca.1~ 1+hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+hgs.1, data = dfwide),
                        lm(moca.1~ 1+moca.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+hgs.1, data = dfwide),
                        lm(moca.1~ 1+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+hgs.0, data = dfwide),
                        lm(moca.1~ 1+sar_hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sar_hgs.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sar_hgs.1, data = dfwide),
                        lm(moca.1~ 1+sar_hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sar_hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sar_hgs.0, data = dfwide)
)

# R2 comparisons (MCI) ----
table <- gen_table(glm(mci.0~ 1+age_group.0, family = binomial, data = dfwide) )
table <- combind_tables(table,
                        glm(mci.0~ 1+female, family = binomial, data = dfwide),
                        glm(mci.0~ 1+CD, family = binomial, data = dfwide),
                        glm(mci.0~ 1+sarc_f.0, family = binomial, data = dfwide),
                        glm(mci.0~ 1+age_group.0+female+CD+sarc_f.0, family = binomial, data = dfwide),
                        glm(mci.0~ 1+hgs.0, family = binomial, data = dfwide),
                        glm(mci.0~ 1+age_group.0+female+CD+hgs.0, family = binomial, data = dfwide),
                        glm(mci.0~ 1+sar_hgs.0, family = binomial, data = dfwide),
                        glm(mci.0~ 1+age_group.0+female+CD+sar_hgs.0, family = binomial, data = dfwide),
                        
                        glm(mci.1~ 1+female, family = binomial, data = dfwide),
                        glm(mci.1~ 1+CD, family = binomial, data = dfwide),
                        glm(mci.1~ 1+sarc_f.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+sarc_f.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+mci.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+mci.0+sarc_f.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+sarc_f.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+mci.0+sarc_f.0, family = binomial, data = dfwide),
                        
                        glm(mci.1~ 1+hgs.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+hgs.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+mci.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+mci.0+hgs.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+hgs.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+hgs.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+mci.0+hgs.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+sar_hgs.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+sar_hgs.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+mci.0+sar_hgs.1, family = binomial, data = dfwide),
                        glm(mci.1~ 1+sar_hgs.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+sar_hgs.0, family = binomial, data = dfwide),
                        glm(mci.1~ 1+age_group.1+female+CD+mci.0+sar_hgs.0, family = binomial, data = dfwide)
)


table %>% clipr::write_clip()

