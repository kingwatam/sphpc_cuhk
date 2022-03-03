rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(labelled)
library(eq5d)
library(lubridate) # interval() & duration()
library(ggplot2)
library(patchwork) # combine ggplots using "+"
library(lme4)
library(lmerTest)
library(ordinal) # clm & clmm
library(multcomp) # glht
library(rdrobust) # rdplot
library(rddtools) # rdd_data, rdd_gen_reg

# import cleaned data
setwd(sprintf("~%s/ehealth", setpath))
df <- readRDS("ehealth_data.rds")
wbs <- readRDS("wbs_data.rds")

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
") # (T0 = baseline, T1 = 6mth, T2 = 12mth, T3 = = 18mth)
val_labels(df$time) <- NULL

# pre-post results ----
df <- df[as.Date(df$ehealth_eval_timestamp) <= as.Date('2021-09-15'),]

vars <- c("Survey_centre", "wbs_survey_date", "gender", "dob",
          "Phase1_member_self_report", "DH_centre_member", "Carer", "Hypertension", "Hypertension_HA",
          "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", # "Heart_score", 
          "Stroke", "Copd", "Renal", "Disease_other", "Disease_other_indicate", "marital", "educ", "Income_oaa", "Income_oala", 
          "Income_ssa", "Income_work", "Income_saving", "Income_cssa", #"Income_cssa_score", 
          "Income_pension", "Income_child", "Income_other", "living_status", "housing_type", "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", "FS_total", # "FS_score", 
          "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC", # "AMIC_score", 
          "Self_rated_health", # "Self_rated_health_score", 
          "Satisfaction", # "Satisfaction_score",
          "Meaning_of_life", "Happiness", # "Happiness_score", 
          "Incontinence", "Hospital", "Hospital_day", # "Hospital_score", 
          "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
          "Drug_use", # "Drug_use_score", 
          "risk_score", "risk_level", "digital", "Centre", "NGO")

allVars <- c("Carer", "Hypertension", "Hypertension_HA",
          "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", # "Heart_score", 
          "Stroke", "Copd", "Renal", "Disease_other", # "married", 
          "educ", "Income_oaa", "Income_oala", 
          "Income_ssa", "Income_work", "Income_saving", "Income_cssa", # "Income_cssa_score", 
          "Income_pension", "Income_child", "Income_other", # "living_status", "housing_type", 
          "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", "FS_total", # "FS_score", 
          "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC", # "AMIC_score", 
          "Self_rated_health", # "Self_rated_health_score", 
          "Satisfaction", # "Satisfaction_score",
          "Meaning_of_life", "Happiness", # "Happiness_score", 
          "Incontinence", "Hospital", "Hospital_day", # "Hospital_score", 
          "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
          "Drug_use", # "Drug_use_score", 
          "risk_score")

wbs$housing_type <- ifelse(wbs$housing_type %in% c(4,5), 3, wbs$housing_type) # recode 4&5 to 3 (1=public rental housing, 2=subsidized-sale housing, 3=private housing, 4=Home for the Aged, 5=others)
wbs$married <- ifelse(wbs$marital %in% 2, 1, 0) # (1= single, 2=married, 3=widowed, 4=divorced/separated)

ordinalVars <- c("educ", "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5",
                 "Self_rated_health", "Satisfaction", 
                 "Meaning_of_life", "Happiness", 
                 "Incontinence", "Drug_use", 
                 "risk_score")


allVars <- c(allVars, 
             "prefrail", "sarcopenic", 
             "poor_health", "memory_complaints", 
             "incontinent", "polypharmacy", "low_wellbeing")

nominalVars <- c("marital", "living_status", "housing_type",
                 "Carer", "Hypertension", "Hypertension_HA",
                 "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", 
                 "Stroke", "Copd", "Renal", "Disease_other", # "married", 
                 "Income_ssa", "Income_work", "Income_saving", "Income_cssa", 
                 "Income_pension", "Income_child", "Income_other", 
                 "Rent", "Own", "FS1", "FS2", "FS3", "FS4", "FS5", 
                 "Hospital", 
                 "Aeservices", "SOPD", "GOPD", "Clinic", 
                 "prefrail", "sarcopenic", 
                 "poor_health", "memory_complaints", 
                 "incontinent", "polypharmacy", "low_wellbeing"
                 )

wbs$prefrail <- ifelse(wbs$FS_total %in% c(1:5), 1, 0)
wbs$sarcopenic <- ifelse(wbs$SAR_total %in% c(4:10), 1, 0)
wbs$poor_health <- ifelse(wbs$Self_rated_health %in% 1, 1, 0)
wbs$memory_complaints <- ifelse(wbs$AMIC %in% c(1:2), 1, 0)
wbs$incontinent <- ifelse(wbs$Incontinence %in% c(1:2), 1, 0)
wbs$polypharmacy <- ifelse(wbs$Drug_use %in% 3, 1, 0)
wbs$low_wellbeing <- ifelse(wbs$Satisfaction %in% c(1:3) |
                               wbs$Meaning_of_life %in% c(1:3) |
                               wbs$Happiness %in% c(0:3), 1, 0)

temp <- wbs
# # high risk at 1st round WBS & did baseline
temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level == 3], ]
temp <- temp[temp$member_id %in% df$member_id,]

# # low risk at 1st round WBS & not done baseline
# temp <- temp[as.Date(temp$ehealth_eval_timestamp) <= as.Date('2021-09-15'),]
# temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level %in% c(1, 2)], ]
# temp <- temp[temp$member_id %!in% df$member_id,]

import_func("ehealth_analysis.R")
Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(temp, id = "member_id", group = "Round", to_English = FALSE, vars = allVars, 
          # ordinalVars = ordinalVars, 
          nominalVars =  NULL, show_levels = FALSE, decimal = 4) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 

# causal diagram ----
dagitty::dagitty('
dag {
"cut-off" [exposure,pos="0.777,-1.507"]
Intervention [pos="1.037,-1.506"]
Y_t0 [adjusted,pos="0.222,-1.361"]
Y_t1 [outcome,pos="1.317,-1.357"]
latent_risk_t0 [latent,pos="0.226,-1.647"]
latent_risk_t1 [latent,pos="1.312,-1.641"]
risk_score_t0 [adjusted,pos="0.471,-1.506"]
"cut-off" -> Intervention
Intervention -> latent_risk_t1
Y_t0 -> "cut-off"
Y_t0 -> Y_t1
Y_t0 -> risk_score_t0
latent_risk_t0 -> Y_t0
latent_risk_t0 -> latent_risk_t1
latent_risk_t0 -> risk_score_t0
latent_risk_t1 -> Y_t1
risk_score_t0 -> "cut-off"
}
')  %>% plot

# RDD analysis (regression discontinuity design) ----

# temp <- wbs[wbs$risk_score>=5 & wbs$risk_score<=25,]
temp <- wbs
temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2021-09-15'),]
wbswide <-  reshape(data=temp, idvar=  "member_id",
                    sep = ".r", 
                    timevar = "Round",
                    direction="wide")

wbswide$risk_level_h.r1 <- ifelse(wbswide$risk_level.r1 == 3, 1, 0)

varlist <- t(array(c(
                     c("FS_total", "Fraility score (FRAIL)"), 
                     c("prefrail", "Frail or pre-frail (FRAIL ≥ 1)"), 
                     c("SAR_total", "Sarcopenia score (SARC-F)"), 
                     c("sarcopenic", "Sarcopenic (SARC-F ≥ 4)"), 
                     c("poor_health", "Poor self-rated health"), 
                     c("low_wellbeing", "Low subjective wellbeing"), 
                     c("memory_complaints", "Subjective memory complaint"), 
                     c("incontinent", "Urinary/fecal incontinence (incl. occasional)"), 
                     c("polypharmacy", "Polypharmacy"), 
                     c("Hospital", "Hospitalized in past year"), 
                     c("Hospital_day", "Number of times hospitalized"), 
                     c("Aeservices", "Used A&E services in past year"), 
                     c("Aeservices_day", "Number of times used A&E services"), 
                     c("GOPD", "Used GOPD in past year"), 
                     c("SOPD", "Used SOPD in past year"), 
                     c("Clinic", "Used Private clinics/hospitals in past year"), 
                     c("Hypertension", "Hypertension"), 
                     c("Diabetes", "Diabetes"), 
                     c("Cholesterol", "High cholesterol"), 
                     c("Heart", "Heart diseases"), 
                     c("Stroke", "Stroke"), 
                     c("Copd", "Chronic obstructive pulmonary disease (COPD)"), 
                     c("Renal", "Renal disease"), 
                     c("Disease_other", "Other diseases")
),
dim = c(2,24))) %>% as.data.frame()

# RDD plots ----
# install.packages("remotes")
# remotes::install_github("PRL-PRG/injectr")
library(injectr)
inject_code({return(temp_plot)}, rdplot, where = "onexit", wrap = FALSE) # inject return statement to rdplot (which only prints plot)

red <- "#db6d63"
green <- "#4f9e78"
blue <- "#6c7cd9"
yellow <- "#ffe873"

plot_rdd <- function(outcome){
  require(patchwork)
  blue <- "#6c7cd9"
  ylabel <- varlist[varlist$V1==outcome,2]
  
  ymin <- ifelse(all(wbs[[outcome]] %in% 0:1) | outcome %in% c("Stroke", "Copd", "Renal"), 0, max(0, round(min(aggregate(wbs[[outcome]], list(wbs$risk_score, wbs$Round), FUN=mean)[, 3] , na.rm=TRUE), 0)))
  ymax <- ifelse(all(wbs[[outcome]] %in% 0:1) | outcome %in% c("Stroke", "Copd", "Renal"), 1, round(max(aggregate(wbs[[outcome]], list(wbs$risk_score, wbs$Round), FUN=mean)[, 3] , na.rm=TRUE), 0))
  
  plot_t0 <- rdplot(y = wbswide[[paste0(outcome, ".r1")]], x = wbswide$risk_score.r1, c = 14
                               , p = 1
                               # , covs = wbswide[[paste0(outcome, ".r1")]]
                               # , ci	= TRUE
                               , col.dots	= blue , col.lines	= "light grey", 
                               y.label	= paste0(ylabel), x.label = "Baseline WBS risk score", title = "Baseline",
                               x.lim = c(0, 34), y.lim = c(ymin, ymax)
  ) + theme(plot.title = element_text(size = 24, hjust = 0.5),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24),
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20))
  
  plot_t1 <-  rdplot(y = wbswide[[paste0(outcome, ".r2")]], x = wbswide$risk_score.r1, c = 14
                               , p = 1
                               , covs = wbswide[[paste0(outcome, ".r1")]]
                               # , ci	= TRUE
                               , col.dots	= blue , col.lines	= "light grey",
                               y.label	= "", x.label = "Baseline WBS risk score", title = "Follow-up",
                               x.lim = c(0, 34), y.lim = c(ymin, ymax)
  ) + theme(plot.title = element_text(size = 24, hjust = 0.5),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24),
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20))
  plot <- plot_t0 + plot_t1
  return(plot)
}

plot_rdd("SAR_total")

# plot RDD in PDF
setwd(sprintf("~%s/ehealth/slides/charts", setpath))
pdf("plot_RDD_temp.pdf", height = 28/2.54/1.6, width = 50/2.54/1.6)
for (outcome in varlist[,1]){
  plot <- plot_rdd(outcome)
  print(plot)
  }
dev.off()

library(pdftools)
pdf_subset('plot_RDD_temp.pdf',
           pages = (1:pdf_length('plot_RDD_temp.pdf'))[seq(3, pdf_length('plot_RDD_temp.pdf'), 3)], 
           output = "plot_RDD_y0.pdf")

if (file.exists('plot_RDD_temp.pdf')) {
  file.remove('plot_RDD_temp.pdf')
}

# histogram
ggplot(wbswide, aes(x = risk_score.r1, fill = risk_level_h.r1)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 14) + 
  geom_vline(xintercept = 14) + 
  labs(x = "WBS Risk score", y = "Count", fill = "High risk")

# manipulation test and density plot ----
library(rddensity)
test_density <- rddensity(wbswide$risk_score.r1, c = 14, kernel = "epanechnikov")
summary(test_density)
rdplotdensity(rdd = test_density, 
              X = wbswide$risk_score.r1, 
              type = "both")

# RDD regression results ----
outcome <- "SAR_total" 
rdd_data(y = wbswide[[paste0(outcome, ".r2")]],
         x = wbswide$risk_score.r1,
         covar =  data.frame(y_t0 = wbswide[[paste0(outcome, ".r1")]]),
         # z =  wbswide$risk_level_h.r1,
         cutpoint = 14
         ) %>% 
  rdd_gen_reg(fun= glm, 
              # family=binomial(),
              family=gaussian(),
              slope = "separate",
              covariates = "y_t0"
              # , order = 3
              ) %>%
  # plot()
  # dens_test()
  summary()

table <- data.frame()
for (outcome in varlist[,1]){
  print(outcome)
  table_temp <- data.frame(matrix(ncol = 2,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  is_logistic <- FALSE
  if (all(wbs[[outcome]] %in% c(0:1, NA)) | outcome %in% c("Stroke", "Copd", "Renal")){
    is_logistic <- TRUE
    fit <- rdd_data(y = wbswide[[paste0(outcome, ".r2")]],
                    x = wbswide$risk_score.r1,
                    covar =  data.frame(y_t0 = wbswide[[paste0(outcome, ".r1")]]),
                    # z =  wbswide$risk_level_h.r1,
                    cutpoint = 14
    ) %>%   rdd_gen_reg(fun= glm, 
                        family=binomial(),
                        # family=gaussian(),
                        slope = "separate",
                        covariates = "y_t0"
                        # , order = 3
    )
  } else {
    is_logistic <- FALSE
    fit <- rdd_data(y = wbswide[[paste0(outcome, ".r2")]],
                    x = wbswide$risk_score.r1,
                    covar =  data.frame(y_t0 = wbswide[[paste0(outcome, ".r1")]]),
                    # z =  wbswide$risk_level_h.r1,
                    cutpoint = 14
                    ) %>%   rdd_gen_reg(fun= glm, 
                                        # family=binomial(),
                                        family=gaussian(),
                                        slope = "separate",
                                        covariates = "y_t0"
                                        # , order = 3
                                        )
  }
  
  for (var in row.names(summary(fit)$coef)){
    
    if (!(var %in% unlist(table_temp[1]))){
      table_temp[row_count, 1] <- var
    } else {
      row_count <- match(var, unlist(table_temp[1]))
    }
    
    colnames(table_temp)[col_count] <- outcome
    
    beta <- iferror(summary(fit, robust = lm_robust)$coef[var, 1], NA)
    # se <-  iferror(summary(fit, robust = lm_robust)$coef[var, 2], NA)
    
    # show_CI <- 0.95
    # alpha <- if(show_CI %in% c(TRUE, FALSE)) (1-0.95) else (1-show_CI)
    # lowerCI <- iferror(beta + qt(p=alpha/2, df=fit$df.residual) * se, NA)
    # upperCI <- iferror(beta + qt(p=1-(alpha/2), df=fit$df.residual) * se, NA)
    p_value <- iferror(summary(fit, robust = lm_robust)$coef[var, ncol(summary(fit)$coef)], NA)
    
    table_temp[row_count, col_count] <- ifelse(is_logistic,
                                               starred_p(p_value, decimal_places = 3, exp(beta)),
                                               starred_p(p_value, decimal_places = 3, beta))
    
    row_count <- row_count + 1
  }
  
  col_count <- col_count + 1
  
  table <- merge(table, table_temp, all = TRUE)
  if (nrow(table) == 0) table <- merge(table, table_temp, all = TRUE) # merge again first time
  table_temp <- NULL
}

rdd_data(y = wbswide[[paste0(outcome, ".r2")]] - wbswide[[paste0(outcome, ".r1")]],
         x = wbswide$risk_score.r1,
         covar =  data.frame(y_t0 = wbswide[[paste0(outcome, ".r1")]]),
         # z =  wbswide$risk_level_h.r1,
         cutpoint = 14
) %>% 
  rdd_gen_reg(fun= glm, 
              # family=binomial(),
              family=gaussian(),
              slope = "separate",
              covariates = "y_t0"
              # , order = 4
  ) %>%
  # plot()
  # dens_test()
  summary()


rdd_data(y = wbswide[[paste0(outcome, ".r2")]], x = wbswide$risk_score.r1, cutpoint = 14) %>% dens_test()
  
wbswide %>%
  select(!!rlang::sym(paste0(outcome, ".r2")), risk_score.r1) %>%
  mutate(threshold = as.factor(ifelse(risk_score.r1 >= 14, 1, 0))) %>%
  ggplot(aes(x = risk_score.r1, y = !!rlang::sym(paste0(outcome, ".r2")), color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_jitter(width = 0.2, height = 0.2) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 13.5, color = "brown",
             size = 1, linetype = "dashed") 
# +  labs(y = "", x = "")
