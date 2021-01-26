rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(eq5d)

setwd(sprintf("~%s/ehealth", setpath))
df <- xlsx::read.xlsx2("List of completed cases & incorrect data by SCHSA staff 20201214.xlsx", sheetName  = "all data by SCHSA"
                      , encoding = "UTF-8"
                      , header = FALSE
                      )
df <- df[2:nrow(df),]
names(df)[1] <- "uid"
names(df)[2] <- "timestamp"
names(df)[3] <- "interviewer"
names(df)[4] <- "member_num"
names(df)[5] <- "name"
names(df)[6] <- "time"
names(df)[7] <- "expense"

p2 <- names(df)[7:12]
names(df)[13] <- "q2"
p3 <- names(df)[14:18]
p4a <- names(df)[19:68]
p4b <- names(df)[69:76]
p5 <- names(df)[77:82]

names(df)[83] <- "patient_satisfaction"
names(df)[84] <- "ehealth_satisfaction"

eq5d(scores=55555, country="HongKong", version="5L", type="VT")