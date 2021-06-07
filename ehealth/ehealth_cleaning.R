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
df <- foreign_to_labelled(haven::read_sav("EHealthIIEvaluation_DATA_NOHDRS_2021-06-02_0955.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

# # load Eva's data
# XLConnect::xlcFreeMemory() # rtools is also required to be installed to avoid error
# df2 <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210426.xlsx", sheetName  = "Raw data"
#                       , encoding = "UTF-8"
#                       , header = FALSE
#                       )
# df2 <- df2[2:nrow(df2),]
# names(df2) <- c(names(df), names(df2)[87:96])

# survey data cleaning ----
temp <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210531_duplicates.xlsx", sheetName  = "duplicate record"
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
# gsub("\t", "", "\tSAG03M223")
df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)] <- 
  gsub("\t", "", df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)]) # remove "\t"

df$ehealth_eval_timestamp <- lubridate::ymd_hms(df$ehealth_eval_timestamp, tz = "Asia/Hong_Kong", quiet =  TRUE)

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

# save data ----
setwd(sprintf("~%s/ehealth", setpath))
saveRDS(df, "ehealth_data.rds")
saveRDS(wbs, "wbs_data.rds")
# extract recording duration data ----
# remotes::install_github("jmgirard/tidymedia")
# install mediainfo & ffmpeg (ref: https://github.com/jmgirard/tidymedia/)

# library(parallel)
# Sys.setlocale(locale =  "eng")
# setwd("C:/Users/tamkingwa/OneDrive - The Chinese University of Hong Kong")
# audio <- as.data.frame(list.files(pattern = "", recursive = TRUE))
# names(audio)[names(audio) == 'list.files(pattern = "", recursive = TRUE)'] <- "path"
# 
# audio$path <- gsub("AKAé¦™æ¸¯ä»”åŠæœƒ", "AKA香港仔坊會", audio$path)
# audio$path <- gsub("CARæ˜Žæ„›", "CAR明愛", audio$path)
# audio$path <- gsub("CRCç¦®è³¢æœƒ", "CRC禮賢會", audio$path)
# audio$path <- gsub("FWCå®¶ç¦æœƒ", "FWC家福會", audio$path)
# audio$path <- gsub("HOHéˆå¯¦", "HOH靈實", audio$path)
# # HUB賽馬會流金匯
# audio$path <- gsub("LMCè–å…¬æœƒéº¥ç†æµ©", "LMC聖公會麥理浩", audio$path)
# audio$path <- gsub("LSSè·¯å¾·æœƒ", "LSS路德會", audio$path)
# audio$path <- gsub("NAAé„°èˆ", "NAA鄰舍", audio$path)
# audio$path <- gsub("POHåšæ„›", "POH博愛", audio$path)
# audio$path <- gsub("SAGè€†åº·æœƒ", "SAG耆康會", audio$path)
# audio$path <- gsub("SJSè–é›…å„", "SJS聖雅各", audio$path)
# audio$path <- gsub("SKCå—è‘µæ¶Œç¤¾æœƒæœå‹™è™•", "SKC南葵涌社會服務處", audio$path)
# audio$path <- gsub("SKHè–å…¬æœƒ", "SKH聖公會", audio$path)
# audio$path <- gsub("SSYå—‡è‰²åœ’", "SSY嗇色園", audio$path)
# audio$path <- gsub("YCHä»æ¿Ÿ", "YCH仁濟", audio$path)
# audio$path <- gsub("YWCåŸºç£æ•™å¥³é’å¹´æœƒ", "YWC基督教女青年會", audio$path)
# 
# audio$path <- gsub("éœ€å†æ‰“", "需再打", audio$path)
# audio$path <- gsub("å””å¾—é–’", "唔得閒", audio$path)
# audio$path <- gsub("æœªèƒ½å®Œæˆå£é.­åŒæ„éŒ„éŸ³", "未能完成口頭同意錄音", audio$path)
# audio$path <- gsub("æŽé.†è‹±", "李順英", audio$path) # replaced problematic empty character in the middle with "." for any character in regex
# audio$path <- gsub("éœ€å†è‡´é›»", "需再致電", audio$path)
# audio$path <- gsub("å†ç´„æ™‚é–“", "再約時間", audio$path)
# audio$path <- gsub("å¥³å…’ä»£ç­”", "女兒代答", audio$path)
# 
# audio <- audio %>% filter(!grepl(".ini", audio[,1], ignore.case = TRUE)) # exclude non audio files (not mp3|wav|mov|m4a|etc)
# audio$filename <- basename(audio$path)
# audio$member_id <- before_char(audio$filename, "-|_")
# audio$member_id <- after_char(audio$member_id, ")")
# audio$member_id <- ifelse(!grepl("\\D", audio$member_id),
#                                between_char(audio$filename, "-", "-"),
#                                audio$member_id) # replace dates with member ID
# audio$member_id <- substr(audio$member_id, 1, 9) # keep only first 9 charac
# audio$filename_no_id <- stringr::str_remove_all(audio$filename, audio$member_id) # remove member ID from filename
# audio$filename_no_id <- substr(audio$filename_no_id, 1, nchar(audio$filename_no_id)-nchar(".mp3"))
# 
# cl <- makeCluster(24) # using 24 instances/clusters yield over 10x speedup (12 threads)
# audio$duration <- parSapply(cl, audio$path, tidymedia::get_duration, unit = "min")
# stopCluster(cl)
# rm(cl)
# 
# cl <- makeCluster(24) # using 24 instances/clusters yield over 10x speedup (12 threads)
# audio$file_date <- parSapply(cl, audio$path, function(x) as.character(file.info(x)$mtime))
# audio$file_date <- as.Date(audio$file_date)
# stopCluster(cl)
# rm(cl)
# 
# setwd(sprintf("~%s/ehealth", setpath))
# saveRDS(audio, "audio_duration.rds")

setwd(sprintf("~%s/ehealth", setpath))
audio <- readRDS("audio_duration.rds")

source("rename_interviewer.R", encoding="utf-8")
df$interviewer_name <- rename_interviewer(df$interviewer_name)

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
")

audio$time <- ifelse(grepl("baseline", audio$path, ignore.case = TRUE), 0, 1)

# audio$vc_duration <- ifelse(grepl("vc", audio$filename_no_id, ignore.case = TRUE) &
#                             !grepl("q", audio$filename_no_id, ignore.case = TRUE), audio$duration, NA)
# audio <- audio %>% filter(is.na(vc_duration)) # remove vc duration

audio$incomplete <- ifelse(grepl("reject|recall|partial|half|cannot reach|callagain|未能|唔得閒|需再致電|需再打|再約時間", audio$filename_no_id, ignore.case = TRUE), 1, 0) # remove incomplete interviews
audio <- audio %>% filter(incomplete == 0)

temp <- audio %>% 
  group_by(member_id, time) %>% 
  summarise(duration = sum(duration, na.rm = TRUE)) # sum duration for each ID and time point

temp <- merge(temp, df[, c("member_id", "time", "interviewer_name")], # extract item matched by case ID
              by=c("member_id", "time"), all.x = TRUE)

temp %>% 
  group_by(interviewer_name
           , time
  ) %>% 
  mutate(n_audio=n()) %>%
  group_by(interviewer_name,
           time,
           n_audio) %>%
  summarise_at(vars(duration), funs(mean = mean(., na.rm=TRUE),
                                    median = median(., na.rm=TRUE), 
                                    sd = sd(., na.rm=TRUE),
                                    min = min(., na.rm=TRUE),
                                    max = max(., na.rm=TRUE))
               ) %>% clipr::write_clip()

