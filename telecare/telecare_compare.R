rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"

library(dplyr)
library(labelled) 

setwd(sprintf("~%s/telecare", setpath)) 

# helper functions ----
'%!in%' <- Negate('%in%')

after_char <- function(x, char) {
  return(substring(x, regexpr(paste0("[", char,"]"), x)+1, nchar(as.character(x))))
}

before_char <- function(x, char) {
  return(substring(x, 1, regexpr(paste0("[", char,"]"), x)-1))
}

# # save individual data frames from RData ----
# load("10Feb.RData") # load RData
# # saveRDS(FuS1, "FuS1.rds")
# saveRDS(allNGOs.cis, "allNGOs.cis.rds")
# saveRDS(table1, "table1.rds")

# import and clean data ----
dfraw <- readRDS("allNGOs.cis.rds")

dfraw <- dfraw %>% rename_at(names(dfraw[, 327:334]), ~ sprintf("socialsecurity%s", c("", seq(1:7))))
dfraw <-  dfraw %>% rename_at(names(dfraw[, 277:280]), ~ sprintf("ucla%s", c(seq(1:3), "")))
names(dfraw)[names(dfraw) %in% "3.1 電話關顧日期:"] <- "Surveyday"
names(dfraw)[names(dfraw) %in% "機構 :"] <- "Surveycentre"
names(dfraw)[names(dfraw) %in% "Record ID"] <- "RecordID"
names(dfraw)[names(dfraw) %in% "CUHK ID"] <- "CUHKID"

df <- readRDS("table1.rds")
df <- cbind(df, "RecordID" = dfraw[,"RecordID"])
df <- cbind(df, "CUHKID" = dfraw[,"CUHKID"])
df <- cbind(df, "Surveyday" = dfraw$Surveyday)
df <- cbind(df, "Surveycentre" = dfraw$Surveycentre)

# names(df)[names(df) == "CUHK ID"] <- 'CUHKID'
df <- cbind(df, dfraw[, 327:334]) # social security items
df <- cbind(df, dfraw[, 277:280]) # ucla loneliness items

setwd(sprintf("~%s/telecare/Joe's_files/Chapter ONE", setpath))
dfjoe <- foreign_to_labelled(haven::read_sav("Chapter One Demographic information wjz_alldata.sav", encoding = "UTF-8")) 
setwd(sprintf("~%s/telecare", setpath))
names(dfjoe)[names(dfjoe) %in% "Surveycentre"] <- "Surveycentre_num"
names(dfjoe)[names(dfjoe) %in% "機構"] <- "Surveycentre"

# by unique record IDs
df$NGO <- before_char(df$CUHKID, "-")
dfjoe$NGO <- before_char(dfjoe$CUHKID, "-")
dfraw$NGO <- before_char(dfraw$CUHKID, "-")
df$NGO <- ifelse(df$NGO %in% NA, before_char(after_char(df$Surveycentre, "("), ")") , df$NGO) 
dfraw$NGO <- ifelse(dfraw$NGO %in% NA, before_char(after_char(dfraw$Surveycentre, "("), ")") , dfraw$NGO) 
df$uid <-  paste0(df$NGO, df$RecordID) # combine NGO and record IDs
dfjoe$uid <-  paste0(dfjoe$NGO, dfjoe$RecordID) 
dfraw$uid <-  paste0(dfraw$NGO, dfraw$RecordID) 

df_uid <- df[df$uid %in% dfjoe$uid ,] # select rows with UIDs from Joe's data
dfraw_uid <- dfraw[dfraw$uid %in% dfjoe$uid ,]

# restrict data to 2022-05-18 ----
cutoff_date <- as.Date('2022-05-18')
df <- merge(df, dfjoe[, c("uid", "SurveyTimestamp")],
            by=c("uid"), all.x = TRUE)
df_may <- df[which(as.Date(df$Surveyday) <= cutoff_date 
                   # | as.Date(df$SurveyTimestamp) <= cutoff_date 
                     ),]
dfraw_may <- dfraw[which(as.Date(dfraw$Surveyday) <= cutoff_date 
                         # | as.Date(df$SurveyTimestamp) <= cutoff_date 
                         ),]

# unique UIDs ----
df_dif <- df_may[df_may$uid %!in% unique(dfjoe$uid), ]
dfraw_dif <- dfraw_may[dfraw_may$uid %!in% unique(dfjoe$uid), ]

# tabulate data ----
tableone::CreateTableOne(data =  df_uid[, 2:33], # this is only an example, it doesn't really work yet 
                         # strata = c(by_group),
                         vars = names(df_uid)
                         # , factorVars = catVars
                         ) %>% 
  print(showAllLevels = TRUE) %>% clipr::write_clip() # write results to clipper (copy data and paste it elsewhere)

# legacy code ----
dfjoe$cssa_oala <- ifelse(dfjoe$`@8.1社會保障援助請選擇可選多項choice綜援$3495$631` %in% c(1, "Chcked", "checked", "Checked") |
                            dfjoe$`@8.1社會保障援助請選擇可選多項choice高額長者生` %in% c("checked", "Checked") |
                            dfjoe$`@8.1社會保障援助請選擇可選多項choice普通長者生` %in% c("Checked", "生果金", "綜援", "綜援 "), 1, 0)

# sample restriction
temp <- df[df$CUHKID %in% dfjoe$CUHKID, ]
# temp <- temp[temp$FuS %in% c(1, 1.1, 1.2), ] 
temp <- temp %>% filter(Sex %!in% NA)

temp <- temp[temp$FuS %in% c(1, 1.1, 1.2), ]
dfjoe <- dfjoe[dfjoe$FollowupStatus %in% c(1), ]

temp$socialsecurity_ <- ifelse(
  # temp$socialsecurity1 %in% "Checked" |
                                 temp$socialsecurity2 %in% "Checked" | # 
                                 temp$socialsecurity5 %in% "Checked", 1, 0)

