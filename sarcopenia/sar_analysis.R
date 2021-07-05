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
df <- readRDS("t0t1t2_data.rds")
dfwide <- readRDS("t0t1t2_data_wide.rds")

df$age_group <- recode_age(df$age, age_labels = NULL, second_group = 60, interval = 10, last_group = 80)

dfwide$sarc_f.10 <- dfwide$sarc_f.1 - dfwide$sarc_f.0
dfwide$sarc_f.21 <- dfwide$sarc_f.2 - dfwide$sarc_f.1
dfwide$moca.10 <- dfwide$moca.1 - dfwide$moca.0
dfwide$hgs.10 <- dfwide$hgs.1 - dfwide$hgs.0

dfwide$sarc_f_mean.10 <- rowMeans(cbind(dfwide$sarc_f.1, dfwide$sarc_f.0))
dfwide$sarc_f_mean.21 <- rowMeans(cbind(dfwide$sarc_f.2, dfwide$sarc_f.1))
dfwide$moca_mean.10 <- rowMeans(cbind(dfwide$moca.1,dfwide$moca.0))
dfwide$hgs_mean.10 <- rowMeans(cbind(dfwide$hgs.1,dfwide$hgs.0))

# restrict sample and code dropouts ----
dfwide$dropout_t1 <- ifelse(!(is.na(dfwide$gender) |  # 477 at T1
                                  is.na(dfwide$age.0) |
                                  is.na(dfwide$CD) |
                                  is.na(dfwide$eduf0) |
                                  is.na(dfwide$sarc_f.0) |
                                  is.na(dfwide$sarc_f.1) |
                                  is.na(dfwide$moca.0) |
                                  is.na(dfwide$moca.1) |
                                  is.na(dfwide$hgs.0) |
                                  is.na(dfwide$hgs.1)), 0, 
                            ifelse(!(is.na(dfwide$gender) | # 715 at T0
                                       is.na(dfwide$age.0) |
                                       is.na(dfwide$CD) |
                                       is.na(dfwide$eduf0) |
                                       is.na(dfwide$sarc_f.0) |
                                       is.na(dfwide$moca.0) |
                                       is.na(dfwide$hgs.0)) &
                                     (is.na(dfwide$date.1)  # 221 non-responders at T1
                                      ), 1, NA))

dfwide$dropout_t0 <- ifelse( is.na(dfwide$gender) |
                                        is.na(dfwide$age.0) |
                                        is.na(dfwide$CD) |
                                        is.na(dfwide$eduf0) |
                                        is.na(dfwide$sarc_f.0) |
                                        is.na(dfwide$moca.0) |
                                        is.na(dfwide$hgs.0), 1, 0)

dfwide$dropout_t0t1 <- ifelse( is.na(dfwide$gender) |  # 477 at T1
                                  is.na(dfwide$age.0) |
                                  is.na(dfwide$CD) |
                                  is.na(dfwide$eduf0) |
                                  is.na(dfwide$sarc_f.0) |
                                  is.na(dfwide$sarc_f.1) |
                                  is.na(dfwide$moca.0) |
                                  is.na(dfwide$moca.1) |
                                  is.na(dfwide$hgs.0) |
                                  is.na(dfwide$hgs.1), 1, 0)


dfwide <- dfwide %>% filter(!(is.na(gender) |
                                      is.na(age.0) |
                                      is.na(CD) |
                                      is.na(eduf0) |
                                      is.na(sarc_f.0) |
                                      is.na(sarc_f.1) |
                                      is.na(moca.0) |
                                      is.na(moca.1) |
                                      is.na(hgs.0) |
                                      is.na(hgs.1)
))

# dfwide <- dfwide %>% filter(!(is.na(gender) |
#                                       is.na(age.0) |
#                                       is.na(CD) |
#                                       is.na(eduf0) |
#                                       is.na(sarc_f.0) |
#                                       is.na(moca.0) |
#                                       is.na(hgs.0)
# ))

dfwide$workf0 <- to_character(dfwide$workf0)
dfwide$marriagef0 <- to_character(dfwide$marriagef0)
dfwide$workf0 <- ifelse(dfwide$workf0 %in% c("employer","self-employed"), "employer/self-employed", dfwide$workf0 )

# reorder factor levels by decreasing frequency 
xLev = names(table(dfwide$workf0))[order(table(dfwide$workf0), decreasing = TRUE)]
dfwide$workf0 = factor(dfwide$workf0, levels=xLev)
xLev = names(table(dfwide$marriagef0))[order(table(dfwide$marriagef0), decreasing = TRUE)]
dfwide$marriagef0 = factor(dfwide$marriagef0, levels=xLev)

### main analysis for draft ----
# descriptive statistics ----
allVars <- c("age.0", "gender", "CD", "htf0", "dyslipf0", "chronicpainf0", "dmf0", "inflamf0", "medicationf0", "eduf0", "workf0", "marriagef0")
catVars <- c("gender", "workf0", "marriagef0")
tableone::CreateTableOne(data =  dfwide, 
                         # strata = c(""),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("age.0", "gender", "CD", "htf0", "dyslipf0", "chronicpainf0", "dmf0", "inflamf0", "medicationf0", "eduf0", "workf0", "marriagef0", 
             "sarc_f.0", "hgs.0", "moca.0")
catVars <- c("gender", "workf0", "marriagef0")
tableone::CreateTableOne(data =  dfwide, 
                         strata = c("dropout_t0t1"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

allVars <- c("sarc_f.0", "sarc_f.1", "sar.0", "sar.1", "moca.0", "moca.1", "mci.0", "mci.1", "hgs.0", "hgs.1")
catVars <- c("sar.0", "sar.1", "mci.0", "mci.1")
tableone::CreateTableOne(data =  dfwide, 
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()

dftemp <- reshape(as.data.frame(dfwide),
              direction = "long",
              idvar = "case_id",
              varying = list(c("sarc_f.0", "sarc_f.1"),
                             c("sar.0", "sar.1"),
                             c("moca.0", "moca.1"),
                             c("mci.0", "mci.1"),
                             c("hgs.0", "hgs.1")
              ),
              v.names=c("sarc_f", 
                        "sar", 
                        "moca", 
                        "mci", 
                        "hgs"),
              timevar="time",
              times=c("0", "1"))

allVars <- c("sarc_f", "sar", "moca", "mci", "hgs")
catVars <- c("sar.0", "sar.1", "mci.0", "mci.1")
tableone::CreateTableOne(data =  dftemp, 
                         strata = c("time"),
                         vars = allVars, factorVars = catVars) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip()
rm(dftemp)

## charts ----
varlist <- t(array(c(c("moca.0", "Baseline HK-MoCA (higher =  better)"), 
                     c("moca.1", "Follow-up HK-MoCA (higher =  better)"),  
                     c("sarc_f.0", "Baseline SARC-F (lower = better)"),  
                     c("sarc_f.1", "Follow-up SARC-F in T1 (lower = better)"),  
                     c("hgs.0", "Baseline Handgrip strength (higher = better)"),  
                     c("hgs.1", "Follow-up Handgrip strength (higher = better)"), 
                     c("sarc_f.10", "Change in SARC-F (T1 minus T0, lower = better)"),
                     c("hgs.10", "Change in handgrip strength (T1 minus T0, higher = better)"),
                     c("moca.10", "Change in MoCA (T1 minus T0, higher = better)")
),
dim = c(2,9))) %>% as.data.frame()

# MoCA vs SARC-F vs HGS histograms ----
hist1 <- ggplot(dfwide, aes(x=moca.0)) + geom_histogram() + xlab(varlist[varlist$V1=="moca.0",2])
hist4 <- ggplot(dfwide, aes(x=moca.1)) + geom_histogram() + xlab(varlist[varlist$V1=="moca.1",2])
hist2 <- ggplot(dfwide, aes(x=sarc_f.0)) + geom_histogram() + xlab(varlist[varlist$V1=="sarc_f.0",2])
hist5 <- ggplot(dfwide, aes(x=sarc_f.1)) + geom_histogram() + xlab(varlist[varlist$V1=="sarc_f.1",2])
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
    geom_smooth(method=lm, color="black", formula = y~x) +
    ggpmisc::stat_poly_eq(formula = y~x, 
                          aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                          parse = TRUE, size = text_size*1.4) +
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
scatterplots <- plot1+plot2+plot3+plot4 
print(scatterplots)
setwd(sprintf("~%s/sarcopenia/draft/charts", setpath))
ggsave("scatter.png", plot = scatterplots, height =  28, width =  50, units = "cm", dpi = 600)
dev.off()

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


# correlation matrix ----
cor_matrix <- dfwide %>% select(sarc_f.0, moca.0, hgs.0, 
                                sarc_f.1,moca.1, hgs.1) %>% cor(method = "pearson", use = "pairwise") 
cor_matrix_p <- dfwide %>% select(sarc_f.0, moca.0, hgs.0, 
                  sarc_f.1,moca.1, hgs.1) %>% rstatix::cor_pmat(method = "pearson", alternative = "two.sided") %>% as.matrix()
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
                        show_CI = TRUE,
                        lm(sarc_f.0~ 1+age_group.0+female+CD+eduf0+moca.0, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.0+female+CD+eduf0+sarc_f.0+moca.0, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.0+female+CD+eduf0+sarc_f.0+moca.1, data = dfwide),
                        lm(sarc_f.0~ 1+age_group.0+female+CD+eduf0+mci.0, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.0+female+CD+eduf0+sarc_f.0+mci.0, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.0+female+CD+eduf0+sarc_f.0+mci.1, data = dfwide)
)
# standardized
table <- combine_tables(NULL, 
                        # show_CI = TRUE, 
                        decimal_places = 2,
                        lm(scale(sarc_f.0)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(moca.0), data = dfwide),
                        lm(scale(sarc_f.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(sarc_f.0)+scale(moca.0), data = dfwide),
                        lm(scale(sarc_f.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(sarc_f.0)+scale(moca.1), data = dfwide),
                        lm(scale(sarc_f.0)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0, data = dfwide),
                        lm(scale(sarc_f.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(sarc_f.0)+mci.0, data = dfwide),
                        lm(scale(sarc_f.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(sarc_f.0)+mci.1, data = dfwide)
)

table <- combine_tables(NULL,
                        show_CI = TRUE,
                        lm(hgs.0~ 1+age_group.0+female+CD+eduf0+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+CD+eduf0+hgs.0+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+CD+eduf0+hgs.0+moca.1, data = dfwide),
                        lm(hgs.0~ 1+age_group.0+female+CD+eduf0+mci.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+CD+eduf0+hgs.0+mci.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.0+female+CD+eduf0+hgs.0+mci.1, data = dfwide)
)
# standardized
table <- combine_tables(NULL,
                        show_CI = TRUE, 
                        decimal_places = 2,
                        lm(scale(hgs.0)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(moca.0), data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(hgs.0)+scale(moca.0), data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(hgs.0)+scale(moca.1), data = dfwide),
                        lm(scale(hgs.0)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0, data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(hgs.0)+mci.0, data = dfwide),
                        lm(scale(hgs.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(hgs.0)+mci.1, data = dfwide)
)

table <- combine_tables(NULL,
                        show_CI = TRUE,
                        exponentiate = TRUE, 
                        lm(moca.0~ 1+age_group.0+female+CD+eduf0+sarc_f.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+CD+eduf0+moca.0+sarc_f.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+CD+eduf0+moca.0+sarc_f.1, data = dfwide),
                        lm(moca.0~ 1+age_group.0+female+CD+eduf0+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+CD+eduf0+moca.0+hgs.0, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+CD+eduf0+moca.0+hgs.1, data = dfwide),
                        
                        glm(mci.0~1+age_group.0+female+CD+eduf0+sarc_f.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.1, family = binomial, data =  dfwide),
                        glm(mci.0~1+age_group.0+female+CD+eduf0+hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+CD+eduf0+mci.0+hgs.0, family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+CD+eduf0+mci.0+hgs.1, family = binomial, data =  dfwide)
)
# standardized
table <- combine_tables(NULL,
                        show_CI = TRUE,
                        exponentiate = TRUE, decimal_places = 2,
                        lm(scale(moca.0)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(sarc_f.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(moca.0)+scale(sarc_f.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(moca.0)+scale(sarc_f.1), data = dfwide),
                        lm(scale(moca.0)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(hgs.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(moca.0)+scale(hgs.0), data = dfwide),
                        lm(scale(moca.1)~ 1+age_group.0+female+scale(CD)+scale(eduf0)+scale(moca.0)+scale(hgs.1), data = dfwide),
                        
                        glm(mci.0~1+age_group.0+female+scale(CD)+scale(eduf0)+scale(sarc_f.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(sarc_f.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(sarc_f.1), family = binomial, data =  dfwide),
                        glm(mci.0~1+age_group.0+female+scale(CD)+scale(eduf0)+scale(hgs.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(hgs.0), family = binomial, data =  dfwide),
                        glm(mci.1~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(hgs.1), family = binomial, data =  dfwide)
)
                 
table %>% clipr::write_clip()

# cox proportional hazards regression
dfwide$time <- as.numeric(dfwide$date.1-dfwide$date.0)
table <- combine_tables(NULL,
                        # adjusted_r2 = FALSE, show_p = TRUE, 
                        show_CI = TRUE,
                        exponentiate = TRUE, 
                        coxph(Surv(time, mci.1) ~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.0, data =  dfwide),
                        coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.1, data =  dfwide),
                        coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+hgs.0, data =  dfwide),
                        coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+hgs.1, data =  dfwide)
)
# standardized
table <- combine_tables(NULL,
                        # adjusted_r2 = FALSE, show_p = TRUE, 
                        show_CI = TRUE,
                        exponentiate = TRUE,  decimal_places = 2,
                        coxph(Surv(time, mci.1) ~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(sarc_f.0), data =  dfwide),
                        coxph(Surv(time, mci.1)~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(sarc_f.1), data =  dfwide),
                        coxph(Surv(time, mci.1)~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(hgs.0), data =  dfwide),
                        coxph(Surv(time, mci.1)~1+age_group.0+female+scale(CD)+scale(eduf0)+mci.0+scale(hgs.1), data =  dfwide)
)

# scaled Schoenfeld residuals to test proportional-hazards assumption
survminer::ggcoxzph(cox.zph(coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.0, data =  dfwide)))
survminer::ggcoxzph(cox.zph(coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.1, data =  dfwide)))
survminer::ggcoxzph(cox.zph(coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+hgs.0, data =  dfwide)))
survminer::ggcoxzph(cox.zph(coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+hgs.1, data =  dfwide)))

coxph(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.0, data =  dfwide, robust = TRUE) %>% summary()
coxphw::coxphw(Surv(time, mci.1)~1+age_group.0+female+CD+eduf0+mci.0+sarc_f.0, template = "PH", data =  dfwide, robust = TRUE) %>% summary()

# variance analysis ----
model <- lm(scale(sarc_f.0)~ scale(moca.0), data = dfwide)
sarc_f0 <- data.frame(error = model$residuals, var = dfwide$moca.0)

model <- lm(scale(sarc_f.0)~ scale(moca.0), data = dfwide)
sarc_f0_ <- data.frame(predict = model$fitted.values, sarc_f.0 = dfwide$sarc_f.0, moca.0 = dfwide$moca.0)
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
dflong <-  reshape(as.data.frame(dfwide),
                               direction = "long",
                               idvar = "case_id",
                               timevar="time",
                               times=c("0", "1", "2"))
dflong <- dflong %>% filter(time %in% 0:1)

rbind(test_var(dflong, "sarc_f", "sarc_f", "moca", cvtest = TRUE) %>% t(),
      test_var(dflong, "hgs", "hgs", "moca", cvtest = TRUE) %>% t()) %>% clipr::write_clip()

test_var(dflong, "sarc_f", "hgs", "moca", cvtest = TRUE, sign1 = "<", sign2 = "<") %>% t() %>% clipr::write_clip()
test_var(dflong, "sarc_f", "hgs", "moca", cvtest = TRUE, sign1 = ">=", sign2 = ">=") %>% t() %>% clipr::write_clip()

sarc_f0 <- lm(scale(sarc_f.0)~ scale(moca.0), data = dfwide)
hgs0 <- lm(scale(hgs.0)~ scale(moca.0), data = dfwide)
model0 <- data.frame(sarc_f0 = sarc_f0$residuals^2, 
                     hgs0 = hgs0$residuals^2 , moca.0 = dfwide$moca.0)

sarc_f1 <- lm(scale(sarc_f.1)~ scale(moca.1), data = dfwide)
hgs1 <- lm(scale(hgs.1)~ scale(moca.1), data = dfwide)
model1 <- data.frame(sarc_f1 = sarc_f1$residuals^2, 
                     hgs1 = hgs1$residuals^2 , moca.1 = dfwide$moca.1)

model01 <- rbind(data.frame(sarc_f = sarc_f0$residuals^2, 
                      hgs = hgs0$residuals^2 , moca = dfwide$moca.0),
                data.frame(sarc_f = sarc_f1$residuals^2, 
                           hgs = hgs1$residuals^2 , moca = dfwide$moca.1))

test_var(model0, "sarc_f0", "sarc_f0", "moca.0", center = median) %>% t() %>% clipr::write_clip()
test_var(model0, "hgs0", "hgs0", "moca.0", center = median)  %>% t() %>% clipr::write_clip()

test_var(model0, "sarc_f0", "hgs0", "moca.0", center = median, sign1 = ">=")  %>% t() %>% clipr::write_clip()


test_var(model1, "sarc_f1", "sarc_f1", "moca.1", center = median) %>% t() %>% clipr::write_clip()
test_var(model1, "hgs1", "hgs1", "moca.1", center = median) %>% t() %>% clipr::write_clip()

test_var(model1, "sarc_f1", "hgs1", "moca.1", center = median, sign1 = ">=") %>% t() %>% clipr::write_clip()


test_var(model01, "sarc_f", "sarc_f", "moca", center = median)  %>% t() %>% clipr::write_clip()
test_var(model01, "hgs", "hgs", "moca", center = median)  %>% t() %>% clipr::write_clip()

test_var(model01, "sarc_f", "hgs", "moca", center = median, sign1 = "<", sign2 = "<")  %>% t() %>% clipr::write_clip()

### exploratory analysis ----
# baseline vs change ----
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

## regressions ----
# difference as explanatory variable ----
table <- combine_tables(NULL,
                        lm(moca.10~ 1+age_group.0+female+CD+moca.0+sarc_f.0+sarc_f.10, data = dfwide),
                        lm(moca.10~ 1+age_group.0+female+CD+sarc_f.0+sarc_f.10, data = dfwide),
                        lm(sarc_f.10~ 1+age_group.0+female+CD+sarc_f.0+moca.0+moca.10, data = dfwide),
                        lm(sarc_f.10~ 1+age_group.0+female+CD+moca.0+moca.10, data = dfwide),
                        lm(moca.1~ 1+age_group.0+female+CD+moca.0+sarc_f.0+sarc_f.10, data = dfwide),
                        lm(sarc_f.1~ 1+age_group.0+female+CD+sarc_f.0+moca.0+moca.10, data = dfwide),
                        lm(sarc_f.10~ 1+age_group.0+female+CD+sarc_f_mean.10, data = dfwide),
                        lm(moca.10~ 1+age_group.0+female+CD+sarc_f_mean.10, data = dfwide)
)

# moca as dependent variable ----
table <- combine_tables(NULL,
                        lm(moca.0~ 1+age_group.0+female+CD+sarc_f.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sarc_f.1, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+sarc_f.0, data = dfwide),
                        lm(moca.1~ 1+age_group.1+female+CD+moca.0+sarc_f.0, data = dfwide)
)

table <- combine_tables(NULL,
                        lm(moca.0~ 1+age_group.0+female+CD+hgs.0, data = dfwide),
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
table <- combine_tables(NULL,
                        lm(hgs.0~ 1+age_group.0+female+CD+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+moca.1, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+hgs.0+moca.1, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+moca.0, data = dfwide),
                        lm(hgs.1~ 1+age_group.1+female+CD+hgs.0+moca.0, data = dfwide) 
)

table <- combine_tables(NULL,
                        glm(sar_hgs.0~ 1+age_group.0+female+CD+moca.0, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+moca.1, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+sar_hgs.0+moca.1, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+moca.0, family = binomial, data = dfwide),
                        glm(sar_hgs.1~ 1+age_group.1+female+CD+sar_hgs.0+moca.0, family = binomial, data = dfwide) 
)

# sarc_f as dependent variable ----
table <- combine_tables(NULL,
                        lm(sarc_f.0~ 1+age_group.0+female+CD+moca.0, data = dfwide),
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
table <- combine_tables(NULL,
                        lmer(sarc_f~ 1+age_group+female+CD+moca+ (1| case_id) , REML = TRUE, data = df),
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


table <- combine_tables(NULL,
                        lmer(sarc_f~ 1+age_group+female +moca+ (1| case_id) , REML = TRUE, data = df),
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

table <- combine_tables(NULL,
                        lmer(sarc_f~ 1  +moca+ (1| case_id) , REML = TRUE, data = df),
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
table <- combine_tables(NULL,
                        lmer(sarc_f~ 1 + (1| case_id) , REML = TRUE, data = test),
                        glmer(sar~ 1 + (1| case_id), family = binomial, data = test),
                        lmer(hgs~ 1 + (1| case_id) , REML = TRUE, data = test),
                        glmer(sar_hgs~ 1 + (1| case_id), family = binomial, data = test),
                        lmer(moca~ 1 + (1| case_id) , REML = TRUE, data = test),
                        glmer(mci~ 1 + (1| case_id), family = binomial, data = test)
)


# binary SARC-F & binary/categorical MCI ----
table <- combine_tables(NULL,
                        glm(sar.0~1+age_group.0+female+CD+mci.0, family = binomial, data =  dfwide),
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

table <- combine_tables(NULL,
                        glm(sar.0~1+age_group.0+female+CD+mci_cat.0, family = binomial, data =  dfwide),
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
table <- combine_tables(NULL,
                        glm(mci.0~1+age_group.0+female+CD+sar.0, family = binomial, data =  dfwide),
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
table <- combine_tables(NULL,
                        lm(moca.0~ 1+age_group.0, data = dfwide),
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
table <- combine_tables(NULL,
                        glm(mci.0~ 1+age_group.0, family = binomial, data = dfwide),
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