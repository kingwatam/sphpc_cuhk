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

Sys.setlocale(locale =  "cht") # set locale to traditional Chinese
setwd(sprintf("~%s/ehealth", setpath))
df <- foreign_to_labelled(haven::read_sav("EHealthIIEvaluation_DATA_NOHDRS_2021-05-18_0953.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

# # load Eva's data
# XLConnect::xlcFreeMemory() # rtools is also required to be installed to avoid error
# df2 <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210426.xlsx", sheetName  = "Raw data"
#                       , encoding = "UTF-8"
#                       , header = FALSE
#                       )
# df2 <- df2[2:nrow(df2),]
# names(df2) <- c(names(df), names(df2)[87:96])

# survey data cleaning ----
temp <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210517_duplicates.xlsx", sheetName  = "duplicate record"
                         , encoding = "UTF-8"
                         , header = TRUE
)
# obtain list of duplicates
dupes <- c(1:25,
          temp$Duplicate.empty.test.wrong.ID[2:nrow(temp)],
          temp$X.[temp$X. != ""],
          1324)
rm(temp)
df <- df[!(df$record_id %in% dupes),] # remove duplicates

# hardcode missing timestamps 
df$ehealth_eval_timestamp[df$record_id %in% df$record_id[df$ehealth_eval_timestamp %in% ""]] <-
  c("2020-10-15 15:01:00", "2020-10-15 15:45:00", "2020-10-15 16:20:00", "2020-10-16 10:00:00")

# fix problematic member IDs
gsub("\t", "", "\tSAG03M223")
df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)] <- 
  gsub("\t", "", df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)]) # remove "\t"

df$ehealth_eval_timestamp <- as.Date(df$ehealth_eval_timestamp)

# # examine duplicates
# wbs %>%
#   add_count(member_id) %>%
#   filter(n >= 2) %>% View()

# scoring <- function(df){
  df %>% select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
  df %>% select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
  df %>% select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
  df %>% select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
  df %>% select(starts_with("diet_")) %>% colnames(.) -> diet_
  df %>% select(starts_with("eq5d")) %>% colnames(.) -> eq5d_
  
  # replace certain labels with NAs
  df[] <- lapply(df[], function(x){
    if (is.labelled(x) %in% TRUE){
      na_value <- val_labels(x)[which(colnames(t(val_labels(x)))  %in% c("不適用/不願意作答", "不適用/不願意作答。", "88.不清楚/不好說"))]
      if (length(na_value) == 0){
        return(x)
      }
      na_value <- na_value[[1]]
      x <- labelled_spss(x, labels = val_labels(x), na_values = na_value)
      x <- user_na_to_na(x)
      # val_label(x, na_value) <- NA
      return(x)
    } else {
      return(x)
    }
  })
  
  df <- df %>%
    mutate(
           # amic = rowSums(.[amic_], na.rm = FALSE), # na.rm = FALSE means any NA in an item results in NA for the scale
           self_efficacy = rowSums(.[self_efficacy_], na.rm = FALSE),
           pase_c = rowSums(.[pase_c_], na.rm = TRUE)
    )
# return(subset(df, select = c(amic, self_efficacy, pase_c
# )))
# }

# df <- cbind(df, scoring(df))
  
# reverse EQ5D items
df[eq5d_[1:5]] <-  (df[eq5d_[1:5]]-6)*-1

df$eq5d_score <- ifelse(is.na(df$eq5d_mobility) | is.na(df$eq5d_self_care) | is.na(df$eq5d_usual_activity) | 
                          is.na(df$eq5d_pain_discomfort) | is.na(df$eq5d_anxiety_depression),
                          NA, paste0(df$eq5d_mobility, df$eq5d_self_care, df$eq5d_usual_activity, df$eq5d_pain_discomfort, df$eq5d_anxiety_depression))
df$eq5d =  eq5d_fast(scores=df$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)

df$amic <- ifelse(df$amic_sum >= 3, 1, 0)
val_labels(df$amic) <- c("AMIC >= 3" = 1, "AMIC < 3" = 0)
df$pase_c_12_1 <- ifelse(df$pase_c_12 > 0, 1, 0) # Walking in general as binary
val_labels(df$pase_c_12_1) <- c(Yes=1, No=0)

# import WBS data ----
setwd(sprintf("~%s/ehealth/JCreport 20210409/data/Overall", setpath))
wbs <- foreign_to_labelled(haven::read_sav("EHealth_NEW_DATA_WBS_Complete_2021-01-31_Overall.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

setwd(sprintf("~%s/ehealth/fwweeklyehealthreport_20210515", setpath))
wbs2 <- xlsx::read.xlsx2("EHealth_NEW_DATA_WBS_Complete_SCHSA_2021-05-15.xlsx", sheetName  = "Raw data"
                         )
# names(wbs2)[names(wbs2)=="Memory"] <- "AMIC"
# names(wbs2)[names(wbs2)=="Memory_score"] <- "AMIC_score"
# 
# vars <- c("Uid", "Gender", "Birth_date", "Survey_date",
#           "Marital_status", "Education", "Overall_score", "Risk_level")
# 
# wbs2[,c("Marital_status", "Education")] <-
#   lapply(wbs2[, c("Marital_status", "Education")],type.convert,as.is=TRUE)
# 
# val_labels(wbs2$Gender) <- val_labels(wbs$Gender)
# val_labels(wbs2$Marital_status) <- val_labels(wbs$Marital_status)
# val_labels(wbs2$Education) <- val_labels(wbs$Education)
# 
# wbs2$Risk_level <- car::recode(wbs2$Risk_level, "
# 'L' = 1; 
# 'M' = 2;
# 'H' = 3
# ")
# val_labels(wbs2$Risk_level) <- c("L" = 1, "M" = 2, "H" = 3)
# wbs2$Birth_date <- as.Date(as.numeric(wbs2$Birth_date), origin = "1899-12-30")

for (i in c("wbs"
            , "wbs2"
            )){ # change variable names for both dfs
  df.tmp <- get(i)
  names(df.tmp)[names(df.tmp)=="Uid"] <- "member_id"
  names(df.tmp)[names(df.tmp)=="Gender"] <- "gender"
  names(df.tmp)[names(df.tmp)=="Birth_date"] <- "dob"
  names(df.tmp)[names(df.tmp)=="Survey_date"] <- "wbs_survey_date"
  names(df.tmp)[names(df.tmp)=="Marital_status"] <- "marital"
  names(df.tmp)[names(df.tmp)=="Education"] <- "educ"
  names(df.tmp)[names(df.tmp)=="Overall_score"] <- "risk_score"
  names(df.tmp)[names(df.tmp)=="Risk_level"] <- "risk_level"
  names(df.tmp)[names(df.tmp)=="Digital"] <- "digital"
  names(df.tmp)[names(df.tmp)=="Living_status"] <- "living_status"
  names(df.tmp)[names(df.tmp)=="Housing_type"] <- "housing_type"
  assign(i, df.tmp)
  rm(df.tmp)
}

vars <- c("member_id", "gender", "dob", "wbs_survey_date",
          "marital", "educ", "living_status", "housing_type",
          "risk_score", "risk_level", "digital")

# df1 <- df
# wbs2t0 <- wbs2[wbs2$Questionnaire_code=="Q1",] # baseline only
# df1 <- merge(df1, wbs2t0[, vars], # extract item matched by member ID
#              by=c("member_id"), all.x = TRUE)

wbst0 <- wbs[wbs$Questionnaire_code=="Q1",] # baseline only
df <- merge(df, wbst0[, vars], # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

df$age <- floor(interval(df$dob, df$ehealth_eval_timestamp) / duration(num = 1, units = "years")) # https://stackoverflow.com/a/27363833

df$age_group <- recode_age(df$age, age_labels = c("60-69", "70-79", "80+"))
df$age_group <- relevel(as.factor(df$age_group), ref = "60-69")

df <- df[df$ehealth_eval_complete == 2,] # keep only completed records

setwd(sprintf("~%s/ehealth", setpath))
saveRDS(df, "ehealth_data.rds")
saveRDS(wbs, "wbs_data.rds")