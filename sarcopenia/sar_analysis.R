rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(foreign)
library(labelled)
library(dplyr)
library(lme4)
library(lmerTest) # calculate p-values in summary()
library(ggplot2)
library(patchwork) # combine ggplots using "+"
library(survival) # coxph

setwd(sprintf("~%s/multimorbidity", setpath))
df <- readRDS("t0t1t2t3_data.rds")
dfwide <- readRDS("t0t1t2t3_data_wide.rds")

df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)
df <- droplevels(df) # drop empty categories in variables

# number of chronic diseases & of medications ----
create_variables <- function(df){
  df$md_total <- rowSums(df %>% select(md1, md2, md3), na.rm=TRUE)
  df$md_total <- ifelse(df$md == 1 & df$md1 == 0 & df$md2 == 0 & df$md3 == 0, 1, df$md_total)
  df$cvd_total <- rowSums(df %>% select(cvd1, cvd2, cvd3), na.rm=TRUE)
  df$cvd_total <- ifelse(df$cvd == 1 & df$cvd1 == 0 & df$cvd2 == 0 & df$cvd3 == 0, 1, df$cvd_total)
  df$rd_total <- rowSums(df %>% select(rd1, rd2, rd3, rd4), na.rm=TRUE)
  df$rd_total <- ifelse(df$rd == 1 & df$rd1 == 0 & df$rd2 == 0 & df$rd3 == 0 & df$rd4 == 0, 1, df$rd_total)
  df$ld_total <- rowSums(df %>% select(ld1, ld2, ld3), na.rm=TRUE)
  df$ld_total <- ifelse(df$ld == 1 & df$ld1 == 0 & df$ld2 == 0 & df$ld3 == 0, 1, df$ld_total)
  df$dd_total <- rowSums(df %>% select(dd1, dd2, dd3, dd4, dd5), na.rm=TRUE)
  df$dd_total <- ifelse(df$dd == 1 & df$dd1 == 0 & df$dd2 == 0 & df$dd3 == 0 & df$dd4 & df$dd5 == 0, 1, df$dd_total)
  df$msd_total <- rowSums(df %>% select(msd1, msd2 ), na.rm=TRUE)
  df$msd_total <- ifelse(df$msd == 1 & df$msd1 == 0 & df$msd2 == 0, 1, df$msd_total)
  df$td_total <- rowSums(df %>% select(td1, td2), na.rm=TRUE)
  df$td_total <- ifelse(df$td == 1 & df$td1 == 0 & df$td2 == 0, 1, df$td_total)
  df$rrd_total <- rowSums(df %>% select(rrd1, rrd2, rrd3), na.rm=TRUE)
  df$rrd_total <- ifelse(df$rrd == 1 & df$rrd1 == 0 & df$rrd2 == 0 & df$rrd3 == 0, 1, df$rrd_total)
  df$ent_total <- rowSums(df %>% select(ent1, ent2), na.rm=TRUE)
  df$ent_total <- ifelse(df$ent == 1 & df$ent1 == 0 & df$ent2 == 0, 1, df$ent_total)
  df$ed_total <- rowSums(df %>% select(ed1, ed2, ed3, ed4), na.rm=TRUE)
  df$ed_total <- ifelse(df$ed == 1 & df$ed1 == 0 & df$ed2 == 0 & df$ed3 == 0 & df$ed4 == 0, 1, df$ed_total)
  df$sd_total <- rowSums(df %>% select(sd1, sd2), na.rm=TRUE)
  df$sd_total <- ifelse(df$sd == 1 & df$sd1 == 0 & df$sd2 == 0, 1, df$sd_total)
  df$nsd_total <- rowSums(df %>% select(nsd1, nsd2, nsd3, nsd4), na.rm=TRUE)
  df$nsd_total <- ifelse(df$nsd == 1 & df$nsd1 == 0 & df$nsd2 == 0 & df$nsd3 == 0 & df$nsd4 == 0, 1, df$nsd_total)
  df$mi_total <- rowSums(df %>% select(mi1, mi2, mi3, mi4), na.rm=TRUE)
  df$mi_total <- ifelse(df$mi == 1 & df$mi1 == 0 & df$mi2 == 0 & df$mi3 == 0 & df$mi4 == 0, 1, df$mi_total)
  
  df$cd <- rowSums(df %>% select(md_total, ca, cvd_total, rd_total , ld_total, dd_total, 
                                 msd_total, td_total, rrd_total, ent_total, ed_total, sd_total, 
                                 bd1, nsd_total, mi_total, od), na.rm=TRUE)
  df$cd <- ifelse(df$time > 0, NA, df$cd)
  var_label(df$cd) <- "number of disease, by 43+ diseases (total diseases plus od-other disease)"
  
  
  df$htm <- rowSums(df %>% select(htm1, htm2, htm3), na.rm=TRUE)
  df$dmm <- rowSums(df %>% select(dmm1, dmm2, dmm3), na.rm=TRUE)
  df$dlm <- rowSums(df %>% select(dlm1, dlm2, dlm3), na.rm=TRUE)
  df$mim <- rowSums(df %>% select(mim1, mim2, mim3), na.rm=TRUE)
  df$painm <- rowSums(df %>% select(painm1, painm2, painm3), na.rm=TRUE)
  
  df$medication <- rowSums(df %>% select(htm, hdm, dmm, dlm, mim, painm), na.rm=TRUE)
  df$medication <- ifelse(df$time > 0, NA, df$medication)
  var_label(df$medication) <- "total number of medication intake"
  
  return(subset(df, select = c(cd, medication)))
}

df <- cbind(df, create_variables(df))

# HGS asymmetry ----
df$hgs_l <- pmax(df$hgs1, df$hgs2) # better hand-grip strength of left hand
df$hgs_r <-  pmax(df$hgs3, df$hgs4) # better hand-grip strength of right hand
df$hgs_l <- ifelse(df$hgs_l == 0, NA, df$hgs_l) # set to NA if zero
df$hgs_r <- ifelse(df$hgs_r == 0, NA, df$hgs_r)# set to NA if zero

df$hgs_ratio <- pmax(df$hgs_l, df$hgs_r)/pmin(df$hgs_l, df$hgs_r)
df$hgs_asym <- ifelse(df$hgs_ratio > 1.1, 1, 0)
df$hgs_l <- NULL
df$hgs_r <- NULL

# handgrip AWGS 2019 ----
df$hgs_2 <- ifelse(is.na(df$hgs) | is.na(df$gender), NA, 
                   ifelse(df$hgs >= 28 & df$gender == "M" , 1, 
                          ifelse(df$hgs >= 18 & df$gender == "F" , 1, 0)))
# from long to wide ----
dfwide <- reshape(df,
                  idvar = c("sopd"), # this line is to keep variables
                  sep = ".", 
                  timevar = "time",
                  direction = "wide")

dfwide$sar.10 <- dfwide$sar.1 - dfwide$sar.0
dfwide$sar.21 <- dfwide$sar.2 - dfwide$sar.1
dfwide$moca.10 <- dfwide$moca.1 - dfwide$moca.0
dfwide$hgs.10 <- dfwide$hgs.1 - dfwide$hgs.0

dfwide$sar_mean.10 <- rowMeans(cbind(dfwide$sar.1, dfwide$sar.0))
dfwide$sar_mean.21 <- rowMeans(cbind(dfwide$sar.2, dfwide$sar.1))
dfwide$moca_mean.10 <- rowMeans(cbind(dfwide$moca.1,dfwide$moca.0))
dfwide$hgs_mean.10 <- rowMeans(cbind(dfwide$hgs.1,dfwide$hgs.0))

# crate variables ----
dfwide$work.0 <- car::recode(dfwide$work.0,"
1 = 'retiree';
2 = 'housewife';
3 = 'employer';
4 = 'employee';
5 = 'self-employed';
6 = 'unemployed'
")
dfwide$work.0 <- ifelse(dfwide$work.0 %in% c("employer","self-employed"), "employer/self-employed", dfwide$work.0 )

dfwide$marriage.0 <- car::recode(dfwide$marriage.0,"
1 = 'single';
2 = 'married';
3 = 'divorced';
4 = 'separated';
5 = 'widowed'
")

dfwide$ht2.0 <- ifelse(dfwide$md.0 == 0, 0, # md1 is a nested question on chronic pain for those with metabolic disease 
                       ifelse(dfwide$md1.0 >= 1, 1, 0)) 
dfwide$dyslip.0 <- ifelse(dfwide$md.0 == 0, 0, # md2 is a nested question on chronic pain for those with metabolic disease 
                          ifelse(dfwide$md2.0 >= 1, 1, 0)) 
dfwide$dm.0 <- ifelse(dfwide$md.0 == 0, 0, # md3 is a nested question on chronic pain for those with metabolic disease 
                      ifelse(dfwide$md3.0 >= 1, 1, 0)) 
dfwide$chronicpain.0 <- ifelse(dfwide$msd.0 == 0, 0, # msd1 is a nested question on chronic pain for those with musculoskeletal and connective tissue diseases 
                               ifelse(dfwide$msd1.0 >= 1, 1, 0)) 
dfwide$skeletal.0 <- ifelse(dfwide$msd.0 == 0, 0, # msd2 is a nested question on "inflammatory conditions" for those with musculoskeletal and connective tissue diseases 
                          ifelse(dfwide$msd.0 >= 1, 1, 0)) 

dfwide$female <- ifelse(dfwide$gender.0 == "F", 1,
                    ifelse(dfwide$gender.0 == "M", 0, NA))

# reorder factor levels by decreasing frequency 
xLev = names(table(dfwide$work.0))[order(table(dfwide$work.0), decreasing = TRUE)]
dfwide$work.0 = factor(dfwide$work.0, levels=xLev)
xLev = names(table(dfwide$marriage.0))[order(table(dfwide$marriage.0), decreasing = TRUE)]
dfwide$marriage.0 = factor(dfwide$marriage.0, levels=xLev)


# restrict sample and code dropouts ----
dfwide$dropout_t1 <- ifelse(!(is.na(dfwide$gender.0) |  # 477 at T1
                                is.na(dfwide$age.0) |
                                is.na(dfwide$cd.0) |
                                is.na(dfwide$edu.0) |
                                is.na(dfwide$sar.0) |
                                is.na(dfwide$sar.1) |
                                is.na(dfwide$moca.0) |
                                is.na(dfwide$moca.1) |
                                is.na(dfwide$hgs.0) |
                                is.na(dfwide$hgs.1)), 0, 
                            ifelse(!(is.na(dfwide$gender.0) | # 730 at T0
                                       is.na(dfwide$age.0) |
                                       is.na(dfwide$cd.0) |
                                       is.na(dfwide$edu.0) |
                                       is.na(dfwide$sar.0) |
                                       is.na(dfwide$moca.0) |
                                       is.na(dfwide$hgs.0)) &
                                     (is.na(dfwide$date.1)  
                                     ), 1, NA))

dfwide$dropout_t0 <- ifelse( is.na(dfwide$gender.0) |
                               is.na(dfwide$age.0) |
                               is.na(dfwide$cd.0) |
                               is.na(dfwide$edu.0) |
                               is.na(dfwide$sar.0) |
                               is.na(dfwide$moca.0) |
                               is.na(dfwide$hgs.0), 1, 0)

dfwide[is.na(dfwide$moca.0), ] %>% nrow() # 298 missing MOCA at T0
dfwide[is.na(dfwide$sar.0) & !is.na(dfwide$moca.0), ] %>% nrow() # 64 missing SARC-F at T0
dfwide[is.na(dfwide$hgs.0) & !is.na(dfwide$sar.0) & !is.na(dfwide$moca.0), ] %>% nrow() # 1 missing handgrip strength at T0
dfwide[is.na(dfwide$date.1) & dfwide$dropout_t0 == 0 , ] %>% nrow() # 239 non-responders at T1 (aka lost to follow-up)

dfwide$dropout_t0t1 <- ifelse( is.na(dfwide$gender.0) |  # 477 at T1
                                 is.na(dfwide$age.0) |
                                 is.na(dfwide$cd.0) |
                                 is.na(dfwide$edu.0) |
                                 is.na(dfwide$sar.0) |
                                 is.na(dfwide$sar.1) |
                                 is.na(dfwide$moca.0) |
                                 is.na(dfwide$moca.1) |
                                 is.na(dfwide$hgs.0) |
                                 is.na(dfwide$hgs.1), 1, 0)

dfwide0 <- dfwide
dfwide <- dfwide %>% filter(!(is.na(gender.0) |
                                is.na(age.0) |
                                is.na(cd.0) |
                                is.na(edu.0) |
                                is.na(sar.0) |
                                is.na(sar.1) |
                                is.na(moca.0) |
                                is.na(moca.1) |
                                is.na(hgs.0) |
                                is.na(hgs.1)
))

### main analysis for draft ----
# descriptive statistics ----
allVars <- c("age.0", "gender.0", "cd.0", "ht2.0", "dyslip.0", "dm.0", "skeletal.0", "chronicpain.0", "medication.0", "edu.0", "work.0", "marriage.0")
catVars <- c("gender.0",
             "ht2.0", "dyslip.0", "chronicpain.0", "dm.0", "skeletal.0",
             "work.0", "marriage.0")
tableone::CreateTableOne(data =  dfwide, 
                         # strata = c(""),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("age.0", "gender.0", "cd.0", "ht2.0", "dyslip.0", "chronicpain.0", "dm.0", "skeletal.0", "medication.0", "edu.0", "work.0", "marriage.0",
             "sar.0", "hgs.0", "moca.0")
catVars <- c("gender.0", "work.0", "marriage.0")
tableone::CreateTableOne(data =  dfwide0, 
                         strata = c("dropout_t0t1"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("sar.0", "sar.1", "sar_.0", "sar_.1", "moca.0", "moca.1", "mci.0", "mci.1", "hgs.0", "hgs.1")
catVars <- c("sar_.0", "sar_.1", "mci.0", "mci.1")
tableone::CreateTableOne(data =  dfwide, 
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

dftemp <- reshape(as.data.frame(dfwide),
                  direction = "long",
                  idvar = "case_id",
                  varying = list(c("sar.0", "sar.1"),
                                 c("sar_.0", "sar_.1"),
                                 c("moca.0", "moca.1"),
                                 c("mci.0", "mci.1"),
                                 c("hgs.0", "hgs.1")
                  ),
                  v.names=c("sar", 
                            "sar_", 
                            "moca", 
                            "mci", 
                            "hgs"),
                  timevar="time",
                  times=c("0", "1"))

allVars <- c("sar", "sar_", "moca", "mci", "hgs")
tableone::CreateTableOne(data =  dftemp, 
                         strata = c("time"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
rm(dftemp)

## charts ----
varlist <- t(array(c(c("moca.0", "Baseline HK-MoCA (higher =  better)"), 
                     c("moca.1", "Follow-up HK-MoCA (higher =  better)"),  
                     c("sar.0", "Baseline SARC-F (lower = better)"),  
                     c("sar.1", "Follow-up SARC-F in T1 (lower = better)"),  
                     c("hgs.0", "Baseline Handgrip strength (higher = better)"),  
                     c("hgs.1", "Follow-up Handgrip strength (higher = better)"), 
                     c("sar.10", "Change in SARC-F (T1 minus T0, lower = better)"),
                     c("hgs.10", "Change in handgrip strength (T1 minus T0, higher = better)"),
                     c("moca.10", "Change in MoCA (T1 minus T0, higher = better)")
),
dim = c(2,9))) %>% as.data.frame()

# MoCA vs SARC-F vs HGS histograms ----
hist1 <- ggplot(dfwide, aes(x=moca.0)) + geom_histogram() + xlab(varlist[varlist$V1=="moca.0",2])
hist4 <- ggplot(dfwide, aes(x=moca.1)) + geom_histogram() + xlab(varlist[varlist$V1=="moca.1",2])
hist2 <- ggplot(dfwide, aes(x=sar.0)) + geom_histogram() + xlab(varlist[varlist$V1=="sar.0",2])
hist5 <- ggplot(dfwide, aes(x=sar.1)) + geom_histogram() + xlab(varlist[varlist$V1=="sar.1",2])
hist3 <- ggplot(dfwide, aes(x=hgs.0)) + geom_histogram() + xlab(varlist[varlist$V1=="hgs.0",2])
hist6 <- ggplot(dfwide, aes(x=hgs.1)) + geom_histogram() + xlab(varlist[varlist$V1=="hgs.1",2])
# histograms <- hist1 + hist2 + hist3 + hist4 + hist5 + hist6 
histograms <- hist1 + hist4 + hist2 + hist5 + hist3 + hist6 + plot_layout(ncol = 2)
print(histograms)
setwd(sprintf("~%s/sarcopenia/draft/charts", setpath))
ggsave("hist.png", plot = histograms, height =  42, width =  50, units = "cm", dpi = 300)
dev.off()

# functions to generate plots ----
get_plot_main <- function(df, x, y, xlab, ylab, jitter_w = 0, jitter_h = 0, 
                          yintercept = NULL, xintercept = NULL, text_size = 4){
  plot <- ggplot(df, aes_string(x=x, y=y) ) +
    # geom_smooth(method=lm, color="black", formula = y~x) +
    geom_smooth(method="loess", formula = y ~ x, se=TRUE, color="black", na.rm=TRUE) +
    # ggpmisc::stat_poly_eq(formula = y~x, 
    #                       aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
    #                       parse = TRUE, size = text_size*1.4) +
    # geom_bin2d(bins =  50) + # stat_bin2d(bins=50, aes(fill = ..density..))  
    geom_jitter(width = jitter_w, height = jitter_h) +
    xlab(xlab) + ylab(ylab) +
    # geom_hline(yintercept = yintercept) + geom_vline(xintercept = xintercept) +
    theme(text = element_text(size=rel(text_size)),
          strip.text.x = element_text(size=rel(text_size*0.85)),
          strip.text.y = element_text(size=rel(text_size*0.85)), 
          axis.text.x = element_text(size=rel(text_size*1)), 
          axis.text.y = element_text(size=rel(text_size*1)))
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

# MoCA vs SARC-F vs HGS scatterplots (different x & y) ----
plot1 <- get_plot(dfwide, 
                  x = "moca.0", y = "sar.0",
                  jitter_w = 0.25, jitter_h = 0.25, text_size = 4.4)
plot2 <- get_plot(dfwide, 
                  x = "moca.1", y = "sar.1",
                  jitter_w = 0.25, jitter_h = 0.25, text_size = 4.4)
plot3 <- get_plot(dfwide, 
                  x = "moca.0", y = "hgs.0",
                  jitter_w = 0.25, jitter_h = 0, text_size = 4.4)
plot4 <- get_plot(dfwide, 
                  x = "moca.1", y = "hgs.1",
                  jitter_w = 0.25, jitter_h = 0, text_size = 4.4)
scatterplots <- plot1+plot2+plot3+plot4 
print(scatterplots)
setwd(sprintf("~%s/sarcopenia/draft/charts", setpath))
ggsave("scatter_final.png", plot = scatterplots, height =  28, width =  50, units = "cm", dpi = 600)
dev.off()

plot5 <- get_plot(dfwide, 
                  x = "moca.0", y = "sar.1",
                  jitter_w = 0.25, jitter_h = 0.25)
plot6 <- get_plot(dfwide, 
                  x = "moca.0", y = "hgs.1",
                  jitter_w = 0.25, jitter_h = 0)

plot1+plot2+plot5+plot3+plot4+plot6

get_plot(dfwide, 
         x = "sar.0", y = "hgs.0",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)
get_plot(dfwide, 
         x = "sar.1", y = "hgs.1",
         jitter_w = 0.25, jitter_h = 0, text_size = 4)

# correlation matrix ----
cor_matrix <- dfwide %>% select(sar.0, moca.0, hgs.0, 
                                sar.1,moca.1, hgs.1) %>% cor(method = "spearman", use = "pairwise") 
cor_matrix_p <- dfwide %>% select(sar.0, moca.0, hgs.0, 
                                  sar.1,moca.1, hgs.1) %>% rstatix::cor_pmat(method = "spearman", alternative = "two.sided") %>% as.matrix()
rownames(cor_matrix_p) <- cor_matrix_p[,1]
cor_matrix_p <- cor_matrix_p[,-1]
for (i in 1:nrow(cor_matrix)){
  for (j in 1:ncol(cor_matrix)){
    # cor_matrix[[i,j]] %>% print()
    # cor_matrix_p[[i,j]] %>% print()
    if (i!=j){
      cor_matrix[i,j] <- starred_p(p_value = as.numeric(cor_matrix_p[i,j]), decimal_places = 2,
                                   related_value = as.numeric(cor_matrix[i,j]))
    }
  }
}
cor_matrix[upper.tri(cor_matrix, diag = FALSE)]<-""
cor_matrix %>% clipr::write_clip()


# regression results ----
table <- combine_tables(NULL, 
                        # show_CI = TRUE, lm_robust = TRUE,
                        aic =  TRUE, 
                        lm(sar.0~ 1+age_group.0+female+cd.0+edu.0+moca.0, data = dfwide),
                        lm(sar.1~ 1+age_group.0+female+cd.0+edu.0+sar.0+moca.0, data = dfwide),
                        lm(sar.1~ 1+age_group.0+female+cd.0+edu.0+sar.0+moca.1, data = dfwide),
                        lm(sar.0~ 1+age_group.0+female+cd.0+edu.0+mci.0, data = dfwide),
                        lm(sar.1~ 1+age_group.0+female+cd.0+edu.0+sar.0+mci.0, data = dfwide),
                        lm(sar.1~ 1+age_group.0+female+cd.0+edu.0+sar.0+mci.1, data = dfwide)
)
# standardized
table <- combine_tables(NULL, 
                        # show_CI = TRUE,
                        decimal_places = 2, lm_robust = TRUE,
                        aic =  TRUE, 
                        lm(scale(sar.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0), data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+scale(moca.0), data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+scale(moca.1), data = dfwide),
                        lm(scale(sar.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0, data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+mci.0, data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+mci.1, data = dfwide)
)
table %>% clipr::write_clip()

table <- combine_tables(NULL,
                        # show_CI = TRUE, lm_robust = TRUE,
                        aic =  TRUE, 
                        lm(hgs.0~ 1+age_group.0+female+cd.0+edu.0+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+cd.0+edu.0+hgs.0+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+cd.0+edu.0+hgs.0+moca.1, data = dfwide),
                        lm(hgs.0~ 1+age_group.0+female+cd.0+edu.0+mci.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+cd.0+edu.0+hgs.0+mci.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+cd.0+edu.0+hgs.0+mci.1, data = dfwide)
)
# standardized
table <- combine_tables(NULL,
                        # show_CI = TRUE,
                        decimal_places = 2, lm_robust = TRUE,
                        aic =  TRUE, 
                        lm(scale(hgs.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0), data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+scale(moca.0), data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+scale(moca.1), data = dfwide),
                        lm(scale(hgs.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0, data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+mci.0, data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+mci.1, data = dfwide)
)

table <- combine_tables(NULL,
                        # show_CI = TRUE, lm_robust = TRUE,
                        exponentiate = TRUE, 
                        aic =  TRUE, 
                        lm(moca.0~ 1+age_group.0+female+cd.0+edu.0+sar.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+cd.0+edu.0+moca.0+sar.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+cd.0+edu.0+moca.0+sar.1, data = dfwide),
                        lm(moca.0~ 1+age_group.0+female+cd.0+edu.0+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+cd.0+edu.0+moca.0+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+cd.0+edu.0+moca.0+hgs.1, data = dfwide),
                        
                        glm(mci.0~1+age_group.0+female+cd.0+edu.0+sar.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+cd.0+edu.0+mci.0+sar.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+cd.0+edu.0+mci.0+sar.1, family = binomial, data =  dfwide),
                        glm(mci.0~1+age_group.0+female+cd.0+edu.0+hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+cd.0+edu.0+mci.0+hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+cd.0+edu.0+mci.0+hgs.1, family = binomial, data =  dfwide)
)
# standardized
table <- combine_tables(NULL,
                        # show_CI = TRUE,
                        exponentiate = TRUE, decimal_places = 2, lm_robust = TRUE,
                        aic =  TRUE, 
                        lm(scale(moca.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+scale(sar.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+scale(sar.1), data = dfwide),
                        lm(scale(moca.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+scale(hgs.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+scale(hgs.1), data = dfwide),
                        
                        glm(mci.0~1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(sar.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(sar.1), family = binomial, data =  dfwide),
                        glm(mci.0~1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs.1), family = binomial, data =  dfwide)
)

# cox proportional hazards regression
dfwide$duration <- as.numeric(dfwide$date.1-dfwide$date.0)
table <- combine_tables(NULL,
                        # adjusted_r2 = FALSE, show_p = TRUE, 
                        # show_CI = TRUE,
                        exponentiate = TRUE, 
                        aic =  TRUE,
                        coxph(Surv(duration, mci.1) ~1+age_group.0+female+cd.0+edu.0+mci.0+sar.0, data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+sar.1, data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+hgs.0, data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+hgs.1, data =  dfwide)
)
# standardized
table <- combine_tables(NULL,
                        # adjusted_r2 = FALSE, show_p = TRUE, 
                        # show_CI = TRUE,
                        exponentiate = TRUE,  decimal_places = 2,
                        # aic =  TRUE, 
                        coxph(Surv(duration, mci.1) ~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(sar.0), data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(sar.1), data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs.0), data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs.1), data =  dfwide)
)

# scaled Schoenfeld residuals to test proportional-hazards assumption
survminer::ggcoxzph(cox.zph(coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+sar.0, data =  dfwide)))
survminer::ggcoxzph(cox.zph(coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+sar.1, data =  dfwide)))
survminer::ggcoxzph(cox.zph(coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+hgs.0, data =  dfwide)))
survminer::ggcoxzph(cox.zph(coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+hgs.1, data =  dfwide)))

coxph(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+sar.0, data =  dfwide, robust = TRUE) %>% summary()
coxphw::coxphw(Surv(duration, mci.1)~1+age_group.0+female+cd.0+edu.0+mci.0+sar.0, template = "PH", data =  dfwide, robust = TRUE) %>% summary()

# regression with HGS asymmetry ----
# standardized
table <- combine_tables(NULL,
                        show_CI = TRUE,
                        decimal_places = 2, 
                        # lm_robust = TRUE,
                        lm(scale(sar.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+hgs_asym.0, data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+hgs_asym.0, data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+hgs_asym.1, data = dfwide),
                        lm(scale(sar.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs_ratio.0), data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+scale(hgs_ratio.0), data = dfwide),
                        lm(scale(sar.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(sar.0)+scale(hgs_ratio.1), data = dfwide)
)
# table <- rbind(table[table[[1]] %!in% "hgs_asym.0",], table[table[[1]] %in% "hgs_asym.0",])

# standardized
table <- combine_tables(NULL,
                        show_CI = TRUE,
                        decimal_places = 2, 
                        # lm_robust = TRUE,
                        lm(scale(hgs.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+hgs_asym.0, data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+hgs_asym.0, data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+hgs_asym.1, data = dfwide),
                        lm(scale(hgs.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs_ratio.0), data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+scale(hgs_ratio.0), data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs.0)+scale(hgs_ratio.1), data = dfwide)
)

# standardized
table <- combine_tables(NULL,
                        show_CI = TRUE,
                        exponentiate = TRUE, decimal_places = 2, 
                        # lm_robust = TRUE,
                        lm(scale(moca.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+hgs_asym.0, data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+hgs_asym.0, data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+hgs_asym.1, data = dfwide),
                        lm(scale(moca.0)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs_ratio.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+scale(hgs_ratio.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(moca.0)+scale(hgs_ratio.1), data = dfwide),
                        
                        glm(mci.0~1+age_group.0+female+scale(cd.0)+scale(edu.0)+hgs_asym.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+hgs_asym.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+hgs_asym.1, family = binomial, data =  dfwide),
                        glm(mci.0~1+age_group.0+female+scale(cd.0)+scale(edu.0)+scale(hgs_ratio.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs_ratio.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs_ratio.1), family = binomial, data =  dfwide)
)

# cox proportional hazards regression
dfwide$duration <- as.numeric(dfwide$date.1-dfwide$date.0)
# standardized
table <- combine_tables(NULL,
                        # adjusted_r2 = FALSE, show_p = TRUE, 
                        show_CI = TRUE,
                        exponentiate = TRUE,  decimal_places = 3,
                        coxph(Surv(duration, mci.1) ~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+hgs_asym.0, data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+hgs_asym.1, data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs_ratio.0), data =  dfwide),
                        coxph(Surv(duration, mci.1)~1+age_group.0+female+scale(cd.0)+scale(edu.0)+mci.0+scale(hgs_ratio.1), data =  dfwide)
)


# variance analysis ----
model <- lm(scale(sar.0)~ scale(moca.0), data = dfwide)
sar0 <- data.frame(error = model$residuals, var = dfwide$moca.0)

model <- lm(scale(sar.0)~ scale(moca.0), data = dfwide)
sar0_ <- data.frame(predict = model$fitted.values, sar.0 = dfwide$sar.0, moca.0 = dfwide$moca.0)
rsq <- function (x, y) cor(x, y) ^ 2

test_var <- function(data, x1, x2, groupvar, center = median, cvtest = FALSE, sign1 = "<", sign2 = ">="){
  sample1 <- eval(parse(text = sprintf("data[[x1]][data[[groupvar]] %s 22]", sign1)))
  sample2 <- eval(parse(text = sprintf("data[[x2]][data[[groupvar]] %s 22]", sign2)))
  new_data <- c(sample1 = sample1, sample2 = sample2)
  group <- as.factor(c(rep(1, length(sample1)), rep(2, length(sample2))))
  
  if (cvtest){
    sd1 <- sd(sample1, na.rm = TRUE)
    mean1 <- mean(sample1, na.rm = TRUE)
    sd2 <- sd(sample2, na.rm = TRUE)
    mean2 <- mean(sample2, na.rm = TRUE)
    cv1 <- sd1/mean1
    cv2 <- sd2/mean2
    # print(cvequality::asymptotic_test(new_data, group))
    mslr_results <- cvequality::mslr_test(nr = 10000, new_data, group, seed = 489411)
    mslr_stat <- mslr_results$MSLRT
    mslr_pval <- mslr_results$p_value
    return(c(x1=x1, mean1=mean1, sd1=sd1, cv1 = cv1, x2=x2, mean2=mean2, sd2=sd2, cv2 = cv2, mslr_stat = mslr_stat, mslr_pval = mslr_pval))
  } else {
    var1 <- var(sample1, na.rm = TRUE)
    var2 <- var(sample2, na.rm = TRUE)
    vartest_results <- car::leveneTest(new_data, group, center = center) 
    vartest_stat <- vartest_results[1, 2]
    vartest_pval <- vartest_results[1, 3]
    options(scipen = 999)
    return(c(n1 = length(sample1), n2 = length(sample2), var1 = var1, var2 = var2, vartest_stat = vartest_stat, vartest_pval = vartest_pval))
  }
}

dflong <- df %>% filter(time %in% 0:1 & sopd %in% unique(dfwide$sopd))

rbind(test_var(dflong, "sar", "sar", "moca", cvtest = TRUE) %>% t(),
      test_var(dflong, "hgs", "hgs", "moca", cvtest = TRUE) %>% t()) %>% clipr::write_clip()

rbind(test_var(dflong, "sar", "sar", "moca", center = median) %>% t(),
      test_var(dflong, "hgs", "hgs", "moca", center = median) %>% t()) %>% clipr::write_clip()

test_var(dflong, "sar", "hgs", "moca", cvtest = TRUE, sign1 = "<", sign2 = "<") %>% t() %>% clipr::write_clip()
test_var(dflong, "sar", "hgs", "moca", cvtest = TRUE, sign1 = ">=", sign2 = ">=") %>% t() %>% clipr::write_clip()

# regression 
sar0 <- lm(scale(sar.0)~ scale(moca.0), data = dfwide)
hgs0 <- lm(scale(hgs.0)~ scale(moca.0), data = dfwide)
model0 <- data.frame(sar0 = sar0$residuals^2, 
                     hgs0 = hgs0$residuals^2 , moca.0 = dfwide$moca.0)

sar1 <- lm(scale(sar.1)~ scale(moca.1), data = dfwide)
hgs1 <- lm(scale(hgs.1)~ scale(moca.1), data = dfwide)
model1 <- data.frame(sar1 = sar1$residuals^2, 
                     hgs1 = hgs1$residuals^2 , moca.1 = dfwide$moca.1)

model01 <- rbind(data.frame(sar = sar0$residuals^2, 
                            hgs = hgs0$residuals^2 , moca = dfwide$moca.0),
                 data.frame(sar = sar1$residuals^2, 
                            hgs = hgs1$residuals^2 , moca = dfwide$moca.1))

test_var(model0, "sar0", "sar0", "moca.0", center = median) %>% t() %>% clipr::write_clip()
test_var(model0, "hgs0", "hgs0", "moca.0", center = median)  %>% t() %>% clipr::write_clip()

test_var(model0, "sar0", "hgs0", "moca.0", center = median, sign1 = ">=")  %>% t() %>% clipr::write_clip()


test_var(model1, "sar1", "sar1", "moca.1", center = median) %>% t() %>% clipr::write_clip()
test_var(model1, "hgs1", "hgs1", "moca.1", center = median) %>% t() %>% clipr::write_clip()

test_var(model1, "sar1", "hgs1", "moca.1", center = median, sign1 = ">=") %>% t() %>% clipr::write_clip()


test_var(model01, "sar", "sar", "moca", center = median)  %>% t() %>% clipr::write_clip()
test_var(model01, "hgs", "hgs", "moca", center = median)  %>% t() %>% clipr::write_clip()

test_var(model01, "sar", "hgs", "moca", center = median, sign1 = "<", sign2 = "<")  %>% t() %>% clipr::write_clip()



