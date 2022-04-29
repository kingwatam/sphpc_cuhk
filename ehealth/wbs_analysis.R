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

# sample restriction and variable list ----
df <- df[as.Date(df$ehealth_eval_timestamp) <= as.Date('2022-03-31'),]

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

# wbswide sample restriction and variable list ----
# temp <- wbs[wbs$risk_score>=5 & wbs$risk_score<=25,]
temp <- wbs
temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2022-02-28'),]
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

# pre-post results ----
temp <- wbs
# # high risk at 1st round WBS & did baseline
temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2022-03-31'),]
temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level == 3], ]
temp <- temp[temp$member_id %in% df$member_id,]

# # restrict baseline risk score
# temp <-  reshape(data=temp, idvar=  "member_id",
#                  sep = ".r", 
#                  timevar = "Round",
#                  direction="wide")
# 
# vars <- temp %>% dplyr::select(ends_with(sprintf(".r%s", 1:2))) %>% colnames %>% stringi::stri_replace_last_regex(str = ., pattern = ".r1|.r2", replacement = "")  %>% unique()
# vars <- vars[order(vars)]
# for (var in vars){
#   for (t in sprintf(".r%s", 1:2)){
#     var_f <- paste0(var, t)
#     if (is.null(temp[[var_f]])){
#       temp[[var_f]] <- NA
#     }
#   }
# }
# 
# all_vars <- names(temp)[names(temp) %in% (temp %>% dplyr::select(ends_with(sprintf(".r%s", 1:2))) %>% colnames)]
# all_vars <- all_vars[order(all_vars)]
# vars_list <- rep(list(c()), length(vars)) # create 449 empty elements in list
# 
# # group variables into groups of 4 for merging
# for (i in (1:length(vars))){ # every 2nd element
#   vars_list[[i]] <- c(paste0(vars[i], ".r1"), 
#                       paste0(vars[i], ".r2")
#   )
# }
# temp <- temp %>% filter(risk_score.r1 >= 19)
# 
# temp <- reshape(temp,
#               idvar = c("member_id"), # this line is to keep variables
#               varying = vars_list,
#               sep = "", 
#               v.name = vars,
#               timevar = "Round",
#               times = 1:2,
#               direction = "long")

# # low risk at 1st round WBS & not done baseline
# temp <- wbs
# temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2022-02-28'),]
# temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level %in% c(1, 2)], ]
# temp <- temp[temp$member_id %!in% df$member_id,]

import_func("ehealth_analysis.R")
Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(temp %>% filter(), id = "member_id", group = "Round", to_English = FALSE, vars = allVars, 
          # ordinalVars = ordinalVars, 
          nominalVars =  NULL, show_levels = FALSE, p_decimal = 100) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 

# COVID waves 3-5 ----
import_func("ehealth_analysis.R")
temp <- wbs

# start defined as new cases >= 15 for 3 days, end defined as new cases < 15 for 3 days (roughly)
temp$covid_3rd <- ifelse(temp$wbs_survey_date >= as.Date('2020-07-01') & 
                           temp$wbs_survey_date <= as.Date('2020-09-15'), 1, 0) # https://www.thelancet.com/journals/lanwpc/article/PIIS2666-6065(21)00039-0/fulltext

temp$covid_4th <- ifelse(temp$wbs_survey_date >= as.Date('2020-11-01') & 
                           temp$wbs_survey_date <= as.Date('2021-03-31'), 1, 0) # https://www.sciencedirect.com/science/article/pii/S2666606521001905

temp$covid_5th <- ifelse(temp$wbs_survey_date >= as.Date('2021-12-31') # https://www.tandfonline.com/doi/full/10.1080/22221751.2022.2060137
                         # & temp$wbs_survey_date <= as.Date('2022-06-31')
                         , 1, 0)

temp$covid <- ifelse(temp$covid_3rd %in% 1 | temp$covid_4th %in% 1 | temp$covid_5th %in% 1, 1, 0)

# # # high risk at 1st round WBS & did baseline
# temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2022-03-31'),]
# temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level == 3], ]
# temp <- temp[temp$member_id %in% df$member_id,]
# temp <- temp[temp$covid %in% 0, ]

# low risk at 1st round WBS & not done baseline
temp <- temp[as.Date(temp$wbs_survey_date) <= as.Date('2022-02-28'),]
temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level %in% c(1, 2)], ]
temp <- temp[temp$member_id %!in% df$member_id,]
temp <- temp[temp$covid %in% 0, ]

import_func("ehealth_analysis.R")
Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
gen_table(temp, id = "member_id", group = "Round", to_English = FALSE, vars = allVars, 
          # ordinalVars = ordinalVars, 
          nominalVars =  NULL, show_levels = FALSE, decimal = 3) %>% clipr::write_clip()
Sys.setlocale(locale =  "eng") 


# predictors of outcome change ----
temp <- wbs
temp <- temp[temp$member_id %in% temp$member_id[temp$Round == 1 & temp$risk_level == 3], ]
temp <- temp[temp$member_id %in% df$member_id,]
temp <-  reshape(data=temp, idvar=  "member_id",
                    sep = ".r", 
                    timevar = "Round",
                    direction="wide")
table <- data.frame()
for (outcome in varlist[,1]){
  print(outcome)
  table_temp <- data.frame(matrix(ncol = 2,  nrow = 0))
  row_count <- 1
  col_count <- 2
  
  temp$y_t0 <- temp[[paste0(outcome, ".r1")]]
  terms <- c("risk_score.r1", "y_t0", "Age.r1", "gender.r1", "married.r2", "as.factor(educ.r1)", "Income_oaa.r1", "Income_oala.r1", 
             "Income_ssa.r1", "Income_work.r1", "Income_saving.r1", "Income_cssa.r1", 
             "Income_pension.r1", "Income_child.r1", "Income_other.r1", "as.factor(living_status.r1)", "as.factor(housing_type.r1)", "Rent.r1")
  is_logistic <- FALSE
  if (all(wbs[[outcome]] %in% c(0:1, NA)) | outcome %in% c("Stroke", "Copd", "Renal")){
    is_logistic <- TRUE

    fit <-  glm(reformulate(response = paste0(outcome, ".r2"), 
                            termlabels = terms)
                , data = temp, family = binomial())
  } else {
    is_logistic <- FALSE

    fit <- lm(reformulate(response = paste0(outcome, ".r2"), 
                          termlabels = terms)
              , data = temp)
  }
  
  for (var in row.names(summary(fit)$coef)){
    
    if (!(var %in% unlist(table_temp[1]))){
      table_temp[row_count, 1] <- var
    } else {
      row_count <- match(var, unlist(table_temp[1]))
    }
    
    colnames(table_temp)[col_count] <- outcome
    
    beta <- iferror(summary(fit
                            # , robust = lm_robust
                            )$coef[var, 1], NA)
    # se <-  iferror(summary(fit, robust = lm_robust)$coef[var, 2], NA)
    
    # show_CI <- 0.95
    # alpha <- if(show_CI %in% c(TRUE, FALSE)) (1-0.95) else (1-show_CI)
    # lowerCI <- iferror(beta + qt(p=alpha/2, df=fit$df.residual) * se, NA)
    # upperCI <- iferror(beta + qt(p=1-(alpha/2), df=fit$df.residual) * se, NA)
    p_value <- iferror(summary(fit
                               # , robust = lm_robust
                               )$coef[var, 4], NA)
    
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

table %>% clipr::write_clip()

# scatter plots ----
setwd(sprintf("~%s/sarcopenia", setpath))
import_func("sar_analysis.R")
setwd(sprintf("~%s/ehealth", setpath))

outcome <- "Self_rated_health"
# outcome <- varlist[5,1]
print(outcome)
wbswide$outcome_diff <- wbswide[[paste0(outcome, ".r2")]]-wbswide[[paste0(outcome, ".r1")]]

get_plot(wbswide %>% filter(member_id %in% df$member_id), 
         x = "wbs_survey_date.r2", y = "outcome_diff",
         jitter_w = 0.25, jitter_h = 0.25)

# causal diagram ----
dagitty::dagitty('
dag {
"cut-off" [exposure,pos="0.750,-1.500"]
Intervention [pos="1.000,-1.500"]
Y_t0 [adjusted,pos="0.250,-1.350"]
Y_t1 [outcome,pos="1.250,-1.350"]
latent_risk_t0 [latent,pos="0.250,-1.650"]
latent_risk_t1 [latent,pos="1.250,-1.650"]
risk_score_t0 [adjusted,pos="0.500,-1.500"]
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
# RDD plots (regression discontinuity design)  ----
# install.packages("remotes")
# remotes::install_github("PRL-PRG/injectr")
library(injectr)
inject_code({return(temp_plot)}, rdplot, where = "onexit", wrap = FALSE) # inject return statement to rdplot (which only prints plot)

red <- "#db6d63"
green <- "#4f9e78"
blue <- "#6c7cd9"
yellow <- "#ffe873"

temp <- wbswide
# temp <- wbswide %>% filter(wbswide$risk_score.r1 >= 7, wbswide$risk_score.r1 <=20)
plot_rdd <- function(outcome, data = wbswide, p = 1, y0 = FALSE){
  require(patchwork)
  blue <- "#6c7cd9"
  ylabel <- varlist[varlist$V1==outcome,2]
  
  ymin <- ifelse(all(data[[paste0(outcome, ".r1")]] %in% 0:1) | outcome %in% c("Stroke", "Copd", "Renal"), 
                 0, max(0, round(min(aggregate(data[[paste0(outcome, ".r1")]], list(data$risk_score.r1), FUN=mean)[, 2] , na.rm=TRUE), 0)))
  ymax <- ifelse(all(data[[paste0(outcome, ".r1")]] %in% 0:1) | outcome %in% c("Stroke", "Copd", "Renal"), 
                 1, round(max(aggregate(data[[paste0(outcome, ".r1")]], list(data$risk_score.r1), FUN=mean)[, 2] , na.rm=TRUE), 0))
  
  plot_t0 <- rdplot(y = data[[paste0(outcome, ".r1")]], x = data$risk_score.r1, c = 14
                               , p = p
                               # , covs = data[[paste0(outcome, ".r1")]]
                               # , ci	= TRUE
                               , col.dots	= blue , col.lines	= "light grey", 
                               y.label	= paste0(ylabel), x.label = "Baseline WBS risk score", title = "Baseline",
                               x.lim = c(0, 34), y.lim = c(ymin, ymax)
  ) + theme(plot.title = element_text(size = 24, hjust = 0.5),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24),
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20))
  
  plot_t1 <-  rdplot(y = data[[paste0(outcome, ".r2")]], x = data$risk_score.r1, c = 14
                               , p = p
                               , covs = if(y0) data[[paste0(outcome, ".r1")]] else NULL
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

plot_rdd("SAR_total", 
         data =  temp, 
         # p = 3, 
         # y0 = TRUE
         )

# plot RDD in PDF
setwd(sprintf("~%s/ehealth/slides/charts", setpath))
pdf("plot_RDD_temp.pdf", height = 28/2.54/1.6, width = 50/2.54/1.6)
for (outcome in varlist[,1]){
  plot <- plot_rdd(outcome
                   , data = temp
                   # , p = 3
                   # , y0 = TRUE
                   )
  print(plot)
  }
dev.off()

library(pdftools)
pdf_subset('plot_RDD_temp.pdf',
           pages = (1:pdf_length('plot_RDD_temp.pdf'))[seq(3, pdf_length('plot_RDD_temp.pdf'), 3)], 
           output = "plot_RDD.pdf")

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
temp <- wbswide %>% filter(wbswide$risk_score.r1 >= 7, wbswide$risk_score.r1 <=20)
outcome <- "SAR_total" 
rdd_data(y = temp[[paste0(outcome, ".r2")]],
         x = temp$risk_score.r1,
         covar =  data.frame(y_t0 = temp[[paste0(outcome, ".r1")]], 
                             Age.r1 = temp[["Age.r1"]], 
                             gender.r1 = temp[["gender.r1"]]),
         # z =  temp$risk_level_h.r1,
         cutpoint = 14
         ) %>% 
  rdd_gen_reg(fun= glm, 
              # family=binomial(),
              family=gaussian(),
              slope = "same",
              covariates = list(c("y_t0", "Age.r1", "gender.r1"))
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
    fit <- rdd_data(y = temp[[paste0(outcome, ".r2")]],
                    x = temp$risk_score.r1,
                    covar =  data.frame(y_t0 = temp[[paste0(outcome, ".r1")]], 
                                        Age.r1 = temp[["Age.r1"]], 
                                        gender.r1 = temp[["gender.r1"]]),
                    # z =  temp$risk_level_h.r1,
                    cutpoint = 14
    ) %>%   rdd_gen_reg(fun= glm, 
                        family=binomial(),
                        # family=gaussian(),
                        slope = "separate",
                        covariates = list(c("y_t0", "Age.r1", "gender.r1"))
                        # , order = 3
    )
  } else {
    is_logistic <- FALSE
    fit <- rdd_data(y = temp[[paste0(outcome, ".r2")]],
                    x = temp$risk_score.r1,
                    covar =  data.frame(y_t0 = temp[[paste0(outcome, ".r1")]], 
                                        Age.r1 = temp[["Age.r1"]], 
                                        gender.r1 = temp[["gender.r1"]]),
                    # z =  temp$risk_level_h.r1,
                    cutpoint = 14
                    ) %>%   rdd_gen_reg(fun= glm, 
                                        # family=binomial(),
                                        family=gaussian(),
                                        slope = "separate",
                                        covariates = list(c("y_t0", "Age.r1", "gender.r1"))
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
    
    beta <- iferror(summary(fit
                            # , robust = lm_robust
                            )$coef[var, 1], NA)
    # se <-  iferror(summary(fit
    #                        # , robust = lm_robust
    #                        )$coef[var, 2], NA)
    
    # show_CI <- 0.95
    # alpha <- if(show_CI %in% c(TRUE, FALSE)) (1-0.95) else (1-show_CI)
    # lowerCI <- iferror(beta + qt(p=alpha/2, df=fit$df.residual) * se, NA)
    # upperCI <- iferror(beta + qt(p=1-(alpha/2), df=fit$df.residual) * se, NA)
    p_value <- iferror(summary(fit
                               # , robust = lm_robust
                               )$coef[var, ncol(summary(fit)$coef)], NA)
    
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

table %>% clipr::write_clip()

rdd_data(y = temp[[paste0(outcome, ".r2")]] - temp[[paste0(outcome, ".r1")]],
         x = temp$risk_score.r1,
         covar =  data.frame(y_t0 = temp[[paste0(outcome, ".r1")]]),
         # z =  temp$risk_level_h.r1,
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


rdd_data(y = temp[[paste0(outcome, ".r2")]], x = temp$risk_score.r1, cutpoint = 14) %>% dens_test()
  
temp %>%
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
