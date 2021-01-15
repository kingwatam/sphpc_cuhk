rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(foreign)
library(dplyr)
library(lme4)
library(lmerTest) # calculate p-values in summary()

setwd(sprintf("~%s/multimorbidity", setpath))
dfwide <- readRDS("JC_covid_data_wide.rds")

# CLPM meaning & support ----
CLPM_meaning_support <- '
  # Estimate the lagged effects between the observed variables.
  support.1 + meaning.1 ~ support.0 + meaning.0 
  support.2 + meaning.2 ~ support.1 + meaning.1 
  
  # # Regression of observed variables on z1 (constrained).
  # support.0 + support.1 + support.2  ~ age.0 + female + cssa + alone # Constrained over time.
  # meaning.0 + meaning.1 + meaning.2 ~ age.0 + female + cssa + alone # Constrained over time.
  
  # Estimate the covariance between the observed variables at the first wave. 
  support.0 ~~ meaning.0 # Covariance
  
  # Estimate the covariances between the residuals of the observed variables.
  support.1 ~~ meaning.1
  support.2 ~~ meaning.2
  
  # Estimate the (residual) variance of the observed variables.
  
  support.0 ~~ support.0 # Variances
  meaning.0 ~~ meaning.0 
  support.1 ~~ support.1 # Residual variances
  meaning.1 ~~ meaning.1 
  support.2 ~~ support.2 
  meaning.2 ~~ meaning.2 
'
# CLPM meaning & ED5Q ----
CLPM_meaning_EQ5D <- '
  # Estimate the lagged effects between the observed variables.
  EQ5D.1 + meaning.1 ~ EQ5D.0 + meaning.0 
  EQ5D.2 + meaning.2 ~ EQ5D.1 + meaning.1 
  
  # # Regression of observed variables on z1 (constrained).
  # EQ5D.0 + EQ5D.1 + EQ5D.2  ~ age.0 + female + cssa + alone # Constrained over time.
  # meaning.0 + meaning.1 + meaning.2 ~ age.0 + female + cssa + alone # Constrained over time.
  
  # Estimate the covariance between the observed variables at the first wave. 
  EQ5D.0 ~~ meaning.0 # Covariance
  
  # Estimate the covariances between the residuals of the observed variables.
  EQ5D.1 ~~ meaning.1
  EQ5D.2 ~~ meaning.2
  
  # Estimate the (residual) variance of the observed variables.
  
  EQ5D.0 ~~ EQ5D.0 # Variances
  meaning.0 ~~ meaning.0 
  EQ5D.1 ~~ EQ5D.1 # Residual variances
  meaning.1 ~~ meaning.1 
  EQ5D.2 ~~ EQ5D.2 
  meaning.2 ~~ meaning.2 
'

# CLPM meaning & support ----
CLPM_support_EQ5D <- '
  # Estimate the lagged effects between the observed variables.
  support.1 + EQ5D.1 ~ support.0 + EQ5D.0 
  support.2 + EQ5D.2 ~ support.1 + EQ5D.1 
  
  # # Regression of observed variables on z1 (constrained).
  # support.0 + support.1 + support.2  ~ age.0 + female + cssa + alone # Constrained over time.
  # EQ5D.0 + EQ5D.1 + EQ5D.2 ~ age.0 + female + cssa + alone # Constrained over time.
  
  # Estimate the covariance between the observed variables at the first wave. 
  support.0 ~~ EQ5D.0 # Covariance
  
  # Estimate the covariances between the residuals of the observed variables.
  support.1 ~~ EQ5D.1
  support.2 ~~ EQ5D.2
  
  # Estimate the (residual) variance of the observed variables.
  
  support.0 ~~ support.0 # Variances
  EQ5D.0 ~~ EQ5D.0 
  support.1 ~~ support.1 # Residual variances
  EQ5D.1 ~~ EQ5D.1 
  support.2 ~~ support.2 
  EQ5D.2 ~~ EQ5D.2 
'

# CLPM fitting ----
CLPM.fit1 <- lavaan::lavaan(CLPM_meaning_support, data = dfwide, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(CLPM.fit1, standardized = TRUE)
CLPM.fit2 <- lavaan::lavaan(CLPM_support_EQ5D, data = dfwide, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(CLPM.fit2, standardized = TRUE)
CLPM.fit3 <- lavaan::lavaan(CLPM_meaning_EQ5D, data = dfwide, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(CLPM.fit3, standardized = TRUE)

labels = list(support0 = "Social support T0",
              support1 = "Social support T1",
              support2 = "Social support T2",
              meaning0 = "Meaning in life T0",
              meaning1 = "Meaning in life T1",
              meaning2 = "Meaning in life T2")

lavaanPlot::lavaanPlot(model = CLPM.fit1, 
                       # graph_options = list(rankdir = "LR"),
                       # labels = labels,
                       # node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       # covs = TRUE,
                       coefs = TRUE, stars = "regress")

lavaanPlot::lavaanPlot(model = CLPM.fit2, 
                       # graph_options = list(rankdir = "LR"),
                       # labels = labels,
                       # node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       # covs = TRUE,
                       coefs = TRUE, stars = "regress")

lavaanPlot::lavaanPlot(model = CLPM.fit3, 
                       # graph_options = list(rankdir = "LR"),
                       # labels = labels,
                       # node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       # covs = TRUE,
                       coefs = TRUE, stars = "regress")

# RI-CLPM ----
RICLPM <- '
  # Create between components (random intercepts)
  RIx =~ 1*support.0 + 1*support.1 + 1*support.2 
  RIy =~ 1*meaning.0 + 1*meaning.1 + 1*meaning.2
  
  # Create within-person centered variables
  wx0 =~ 1*support.0
  wx1 =~ 1*support.1
  wx2 =~ 1*support.2 
  wy0 =~ 1*meaning.0
  wy1 =~ 1*meaning.1
  wy2 =~ 1*meaning.2
  
  # Estimate the lagged effects between the within-person centered variables.
  wx1 + wy1 ~ wx0 + wy0
  wx2 + wy2 ~ wx1 + wy1

  # Estimate the covariance between the within-person centered variables at the first wave. 
  wx0 ~~ wy0 # Covariance
  
  # Estimate the covariances between the residuals of the within-person centered variables (the innovations).
  wx1 ~~ wy1
  wx2 ~~ wy2
  
  # Estimate the variance and covariance of the random intercepts. 
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate the (residual) variance of the within-person centered variables.
  wx0 ~~ wx0 # Variances
  wy0 ~~ wy0 
  wx1 ~~ wx1 # Residual variances
  wy1 ~~ wy1 
  wx2 ~~ wx2 
  wy2 ~~ wy2 
'
RICLPM.fit <- lavaan::lavaan(RICLPM, data = dfwide, missing = 'ML', meanstructure = T, int.ov.free = T) 
summary(RICLPM.fit, standardized = T)
lavaanPlot::lavaanPlot(model = RICLPM.fit, 
                       graph_options = list(rankdir = "LR"),
                       # labels = labels,
                       # node_options = list(shape = "box", fontname = "Helvetica"),
                       edge_options = list(color = "grey"),
                       coefs = TRUE, stars = "regress")
