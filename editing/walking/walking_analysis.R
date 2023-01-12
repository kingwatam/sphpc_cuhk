rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(meta) # rma & metagen
library(metafor) #rma

setwd(sprintf("~%s/editing/walking", setpath))

df <- readxl::read_excel("Walking SR20221212b.xlsx", sheet  = "table1"
) 
df2 <- readxl::read_excel("Walking SR20221212b.xlsx", sheet  = "table2"
) 
df3 <- readxl::read_excel("Walking SR20221212b.xlsx", sheet  = "table3"
) 
df4 <- readxl::read_excel("Walking SR20221212b.xlsx", sheet  = "table4"
) 
df5 <- readxl::read_excel("Walking SR20221212b.xlsx", sheet  = "table5"
) 

names(df5)[names(df5) == '...5'] = 'study'
  
meta_ouputs <- function(name, model){
  table <- data.frame()
  n <- 1
  
  table[n, 1] <- name
  
  if (class(model)[1] %in% c("metacont", "metareg")){
      if (length(model$beta) >= 2) {
      table[n, 2] <- model$k
      
      for (i in 1:length(model$beta)) {
        table[n, 3] <- row.names(model$beta)[i]
        table[n, 4] <- model$beta[i]
        table[n, 5] <- model$ci.lb[i]
        table[n, 6] <- model$ci.ub[i]
        table[n, 7] <- model$pval[i]
        table[n, 8] <- model$I2[i]
        
        n <- n + 1
      }
      model_temp <-  update(model, mods = update(model$formula.mods, ~ . + 1)) # remove intercept
      table[n - 1, 9] <- model_temp$pval[2]
      
    } else {
      table[n, 2] <- model$k.all
      table[n, 3] <- ""
      table[n, 4] <- model$TE.random
      table[n, 5] <- model$lower.random
      table[n, 6] <- model$upper.random
      table[n, 7] <- model$pval.random
      table[n, 8] <- model$I2
      table[n, 9] <- ""
    }
    
  } else if (class(model)[1] %in% "rma.uni"){
    if (length(model$beta) >= 2) {
      table[n, 2] <- model$k
      
      for (i in 1:length(model$beta)) {
        table[n, 3] <- row.names(model$beta)[i]
        table[n, 4] <- model$beta[i]
        table[n, 5] <- model$ci.lb[i]
        table[n, 6] <- model$ci.ub[i]
        table[n, 7] <- model$pval[i]
        table[n, 8] <- model$I2[i]
        
        n <- n + 1
      }
      model_temp <-
        update(model, mods = update(model$formula.mods, ~ . + 1)) # remove intercept
      table[n - 1, 9] <- model_temp$pval[2]
      
    } else {
      table[n, 2] <- model$k
      table[n, 3] <- ""
      table[n, 4] <- model$beta
      table[n, 5] <- model$ci.lb
      table[n, 6] <- model$ci.ub
      table[n, 7] <- model$pval
      table[n, 8] <- model$I2
      table[n, 9] <- ""
    }
  } else if (class(model)[1] %in% "metagen"){
    if (length(model$TE.random.w) >= 2){
      
      for (i in 1:length(model$TE.random.w)){
        table[n, 2] <- model$k.w[i]
        table[n, 3] <- row.names(as.data.frame(model$TE.random.w))[i]
        table[n, 4] <- model$TE.random.w[i]
        table[n, 5] <- model$lower.random.w[i]
        table[n, 6] <- model$upper.random.w[i]
        table[n, 7] <- model$pval.random.w[i]
        table[n, 8] <- model$I2.w[i]

        n <- n + 1
      }
      table[n-1, 9] <- model$pval.Q.b.random
      
    } else {
      table[n, 2] <- model$k
      table[n, 3] <- ""
      table[n, 4] <- model$TE.random
      table[n, 5] <- model$lower.random
      table[n, 6] <- model$upper.random
      table[n, 7] <- model$pval.random
      table[n, 8] <- model$I2
      table[n, 9] <- ""
      
    }
  }
  return(table)
}

# table 1 (depression vs inactive control) ----

meta_func_ <- function(data, type, by_group = NULL){ # helper function to shorten metagen/rma/metareg
  if (type == "metagen"){
    if(is.null(by_group)){
      metagen(yi, vi, data=escalc(measure = "SMD",
                                  n1i = tn, m1i = tmean, sd1i = tsd,
                                  n2i = cn, m2i = cmean, sd2i = csd, data = data)
              , comb.fixed=FALSE, method.tau="DL")
    } else {
      data <- data[data[[by_group]] %!in% NA, ]
      metagen(yi, vi, data=escalc(measure = "SMD",
                                  n1i = tn, m1i = tmean, sd1i = tsd,
                                  n2i = cn, m2i = cmean, sd2i = csd, data = data)
              , comb.fixed=FALSE, method.tau="DL", byvar=eval(as.symbol(by_group)))
    }
  } else if (type == "rma"){ # problematic code segment
    if(is.null(by_group)){
      rma(yi, vi, data = escalc(measure = "SMD",
                                n1i = tn, m1i = tmean, sd1i = tsd,
                                n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")
      
    } else {
      model <- rma(yi, vi, data = escalc(measure = "SMD",
                                         n1i = tn, m1i = tmean, sd1i = tsd,
                                         n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")
      update(model, mods = ~ eval(as.symbol(by_group)) -1)
    }
  } else if (type == "metareg"){ # problematic code segment
    if(is.null(by_group)){
      metacont(
        n.e = tn, mean.e = tmean, sd.e = tsd, 
        n.c = cn, mean.c = cmean, sd.c = csd, 
        data = data, sm = "SMD", method.tau = "DL") 
      
    } else {
      model <- metacont(
        n.e = tn, mean.e = tmean, sd.e = tsd, 
        n.c = cn, mean.c = cmean, sd.c = csd, 
        data = data, sm = "SMD", method.tau = "DL") 
      metareg(x = model, formula = ~ eval(as.symbol(by_group)) -1)
    }
  }
}

get_meta_results <- function(data = df, data2 = df2, data3 = df3, type = "metagen"){
  table <- data.frame()
  table[1, 1:9] <- c("", "N", "category", "TE.random", "lower.random", "upper.random", "pval.random", "I2", "pval.category")
  
  # model <- metagen(yi, vi, data=escalc(measure = "SMD",
  #                                      n1i = tn, m1i = tmean, sd1i = tsd,
  #                                      n2i = cn, m2i = cmean, sd2i = csd, data = data)
  #                  , comb.fixed=FALSE, method.tau="DL")
  table <- rbind(table, meta_ouputs("All", 
                                    meta_func_(data = data, type = type, by_group = NULL)))
    
  table <- rbind(table, meta_ouputs("Intervention duration", 
                                    meta_func_(data = data, type = type, by_group = "month")))
  
  table <- rbind(table, c("Intensity", rep(NA, 8)))
  table <- rbind(table, meta_ouputs("At least moderate intensity", 
                                    meta_func_(data = data[data$intensity %in% "2", ], type = type, by_group = NULL)))
  table <- rbind(table, meta_ouputs("Increasing intensity", 
                                    meta_func_(data = data[data$increase %in% "1", ], type = type, by_group = NULL)))
  
  table <- rbind(table, meta_ouputs("Frequency", 
                                    meta_func_(data = data, type = type, by_group = "frequency2g2")))
  table <- rbind(table, meta_ouputs("Duration of each walk", 
                                    meta_func_(data = data, type = type, by_group = "minute")))
  table <- rbind(table, meta_ouputs("Walking pace", 
                                    meta_func_(data = data, type = type, by_group = "pace")))
  table <- rbind(table, meta_ouputs("Walking format", 
                                    meta_func_(data = data, type = type, by_group = "format")))
  table <- rbind(table, meta_ouputs("Walking place", 
                                    meta_func_(data = data[data$indoor %!in% c(NA, 0), ], type = type, by_group = "indoor")))
  table <- rbind(table, meta_ouputs("Following instructions during walking", 
                                    meta_func_(data = data, type = type, by_group = "instruction")))
  table <- rbind(table, meta_ouputs("Walking training", 
                                    meta_func_(data = data, type = type, by_group = "training")))
  table <- rbind(table, meta_ouputs("Motivation (behaviour change technique)", 
                                    meta_func_(data = data2, type = type, by_group = "motivation"))) # df2/df5
  table <- rbind(table, meta_ouputs("Target/goal", 
                                    meta_func_(data = data, type = type, by_group = "target")))
  
  table <- rbind(table, c("Support person", rep(NA, 8)))
  table <- rbind(table, meta_ouputs("Instructor/coach", 
                                    meta_func_(data = data[data$instructor %in% 1, ], type = type, by_group = NULL)))
  table <- rbind(table, meta_ouputs("Researcher", 
                                    meta_func_(data = data[data$researchstaff %in% 1, ], type = type, by_group = NULL)))
  table <- rbind(table, meta_ouputs("Healthcare professional", 
                                    meta_func_(data = data2[data2$professional %in% 1, ], type = type, by_group = NULL))) # df2/df5
  table <- rbind(table, meta_ouputs("None", 
                                    meta_func_(data = data2[data2$instructor==0 & data2$researchstaff==0 & data2$professional==0, ], type = type, by_group = NULL))) # data2
  
  table <- rbind(table, meta_ouputs("Heart rate monitor", 
                                    meta_func_(data = data, type = type, by_group = "HRMonitor")))
  table <- rbind(table, meta_ouputs("Pedometer", 
                                    meta_func_(data = data, type = type, by_group = "pedometer")))
  table <- rbind(table, meta_ouputs("Treadmill", 
                                    meta_func_(data = data, type = type, by_group = "treadmill")))
  table <- rbind(table, meta_ouputs("Dropout rate", 
                                    meta_func_(data = data, type = type, by_group = "dropout")))
  table <- rbind(table, meta_ouputs("Age", 
                                    meta_func_(data = data, type = type, by_group = "age")))
  table <- rbind(table, meta_ouputs("Safety concerned (recorded adverse events)", 
                                    meta_func_(data = data, type = type, by_group = "safety")))
  table <- rbind(table, meta_ouputs("Mindful walking", 
                                    meta_func_(data = data3, type = type, by_group = "mindfulness"))) # df3/df4
  table <- rbind(table, meta_ouputs("Nordic walking", 
                                    meta_func_(data = data, type = type, by_group = "nordic")))
  return(table)
}

table <- get_meta_results(df, df2, df3, type = "rma")
table2 <- get_meta_results(df, df2, df3, type = "metareg")

table <- get_meta_results(df, df2, df3)
table <- convert2value(table, NA, '')
table %>% clipr::write_clip()

# table 2 (anxiety vs inactive control) ----
table <- get_meta_results(df4, df5, df4)
table <- convert2value(table, NA, '')
table %>% clipr::write_clip()

# tables 1 & 2 manual method ----

# # table 1
# data <- df
# data2 <- df2
# data3 <- df3

# table 2
data <- df4
data2 <- df5
data3 <- df4

table <- data.frame()
table[1, 1:9] <- c("", "N", "category", "TE.random", "lower.random", "upper.random", "pval.random", "I2", "pval.category")

model <- rma(yi, vi, data = escalc(measure = "SMD",
                          n1i = tn, m1i = tmean, sd1i = tsd,
                          n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")

table <- rbind(table, meta_ouputs("All", 
                                  model))

table <- rbind(table, meta_ouputs("Intervention duration", 
                                  update(model, mods = ~ month -1)))

table <- rbind(table, c("Intensity", rep(NA, 8)))
model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data[data$intensity %in% "2", ]), method = "DL")
table <- rbind(table, meta_ouputs("At least moderate intensity", 
                                  model))
model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data[data$increase %in% "1", ]), method = "DL")
table <- rbind(table, meta_ouputs("Increasing intensity", 
                                  model))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")
table <- rbind(table, meta_ouputs("Frequency", 
                                  update(model, mods = ~ frequency2g2 -1)))
table <- rbind(table, meta_ouputs("Duration of each walk", 
                                  update(model, mods = ~ minute -1)))

table <- rbind(table, meta_ouputs("Walking pace", 
                                  update(model, mods = ~ as.factor(pace) -1)))
table <- rbind(table, meta_ouputs("Walking format", 
                                  update(model, mods = ~ as.factor(format) -1)))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data[data$indoor %!in% c(NA, 0), ]), method = "DL")
table <- rbind(table, meta_ouputs("Walking place", 
                                  update(model, mods = ~ as.factor(indoor) -1)))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")
table <- rbind(table, meta_ouputs("Following instructions during walking", 
                                  update(model, mods = ~ as.factor(instruction) -1)))
table <- rbind(table, meta_ouputs("Walking training", 
                                  update(model, mods = ~ as.factor(training) -1)))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data2), method = "DL") # df2/df5
table <- rbind(table, meta_ouputs("Motivation (behaviour change technique)", 
                                  update(model, mods = ~ as.factor(motivation) -1)))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")
table <- rbind(table, meta_ouputs("Target/goal", 
                                  update(model, mods = ~ as.factor(target) -1)))

table <- rbind(table, c("Support person", rep(NA, 8)))
model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data[data$instructor %in% 1, ]), method = "DL")
table <- rbind(table, meta_ouputs("Instructor/coach", 
                                  model))
model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data[data$researchstaff %in% 1, ]), method = "DL")
table <- rbind(table, meta_ouputs("Researcher", 
                                  model))
model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data2[data2$professional %in% 1, ]), method = "DL") # df2/df5
table <- rbind(table, meta_ouputs("Healthcare professional", 
                                  model))
model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data2[data2$instructor==0 & data2$researchstaff==0 & data2$professional==0, ]), method = "DL") # df2/5
table <- rbind(table, meta_ouputs("None", 
                                  model))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")

table <- rbind(table, meta_ouputs("Heart rate monitor", 
                                  update(model, mods = ~ as.factor(HRMonitor) -1)))
table <- rbind(table, meta_ouputs("Pedometer", 
                                  update(model, mods = ~ as.factor(pedometer) -1)))
table <- rbind(table, meta_ouputs("Treadmill", 
                                  update(model, mods = ~ as.factor(treadmill) -1)))
table <- rbind(table, meta_ouputs("Dropout rate", 
                                  update(model, mods = ~ dropout -1)))
table <- rbind(table, meta_ouputs("Age", 
                                  update(model, mods = ~ age -1)))
table <- rbind(table, meta_ouputs("Safety concerned (recorded adverse events)", 
                                  update(model, mods = ~ as.factor(safety) -1)))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data3), method = "DL") # df3/df4
table <- rbind(table, meta_ouputs("Mindful walking", 
                                  update(model, mods = ~ as.factor(mindfulness) -1)))

model <- rma(yi, vi, data = escalc(measure = "SMD",
                                   n1i = tn, m1i = tmean, sd1i = tsd,
                                   n2i = cn, m2i = cmean, sd2i = csd, data = data), method = "DL")
table <- rbind(table, meta_ouputs("Nordic walking", 
                                  update(model, mods = ~ as.factor(nordic) -1)))

table <- convert2value(table, NA, '')
table %>% clipr::write_clip()

# misc ----
rma(yi, vi, data = escalc(measure = "SMD",
                          n1i = tn, m1i = tmean, sd1i = tsd,
                          n2i = cn, m2i = cmean, sd2i = csd, data = df), mods = ~ month, method = "DL")

# IVhet
rma(yi, vi, data = escalc(measure = "SMD",
                          n1i = tn, m1i = tmean, sd1i = tsd,
                          n2i = cn, m2i = cmean, sd2i = csd, data = df), method = "DL", weights = 1/vi)

## IVhet examples ----
library(metafor)
dat <- get(data(dat.li2007))
dat <- dat[order(dat$study),]
rownames(dat) <- 1:nrow(dat)
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat, subset=-c(19,21,22))

### standard RE model
res <- rma(yi, vi, data=dat, method="DL")
predict(res, transf=exp, digits=2)

### RE model with 1/vi weights ("IVhet")
res <- rma(yi, vi, data=dat, method="DL", weights=1/vi)

metabin(
  event.e = ai, n.e = n1i, 
  event.c = ci, n.c = n2i, subset=-c(19,21,22),
  data = dat, sm = "OR", method.tau = "DL", we) 
model

predict(res, transf=exp, digits=2)

hc(res, transf=exp, digits=2)

# metareg examples ----
model <- metacont(
  n.e = tn, mean.e = tmean, sd.e = tsd, 
  n.c = cn, mean.c = cmean, sd.c = csd, 
  data = df, sm = "SMD", method.tau = "DL") 
model

model1 <- metareg(x = model, formula = ~ month)

metacont(n.e = tn, mean.e = tmean, sd.e = tsd, 
         n.c = cn, mean.c = cmean, sd.c = csd, data = df[df$intensity %in% "2",], sm = "SMD", method.tau = "DL") 
metacont(n.e = tn, mean.e = tmean, sd.e = tsd, 
         n.c = cn, mean.c = cmean, sd.c = csd, data = df[df$increase %in% "1",], sm = "SMD", method.tau = "DL") 

# rma & metagen examples----
model  <- rma(yi, vi, data = escalc(measure = "SMD",
                                    n1i = tn, m1i = tmean, sd1i = tsd,
                                    n2i = cn, m2i = cmean, sd2i = csd, data = df), method = "DL")

update(model, subset = (month != "<3 months"))
by_group <- "month"
model <- update(model, mods = ~ eval(as.symbol(by_group)) -1)

gsub("eval", "" , "~eval(as.symbol(by_group)) - 1")

update(model, mods = update(model$formula.mods, ~ . + 1)) # remove intercept

rma.mv(yi, vi, data=escalc(measure = "SMD",
                           n1i = tn, m1i = tmean, sd1i = tsd,
                           n2i = cn, m2i = cmean, sd2i = csd, data = df[df$month %!in% NA, ]), 
       mods = ~ 0 + month, random = ~ month | outcome, struct="DIAG")

model1 <- metagen(yi, vi, data=escalc(measure = "SMD",
                                      n1i = tn, m1i = tmean, sd1i = tsd,
                                      n2i = cn, m2i = cmean, sd2i = csd, data = df[df$month %!in% NA, ])
                  , comb.fixed=FALSE, method.tau="DL", byvar=month)

model2 <- metagen(yi, vi, data=escalc(measure = "SMD",
                                      n1i = tn, m1i = tmean, sd1i = tsd,
                                      n2i = cn, m2i = cmean, sd2i = csd, data = df)
                  , comb.fixed=FALSE, method.tau="DL")

table <- rbind(table, meta_ouputs("Intervention duration", update(model, mods = ~ as.factor(month)-1)))

table1 <- meta_ouputs("Intervention duration", model1)
table2 <- meta_ouputs("Intervention duration", model2)