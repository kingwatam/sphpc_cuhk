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
df <- foreign_to_labelled(haven::read_sav("EHealthIIEvaluation_DATA_NOHDRS_2021-10-05_1047.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

# temporary fix
# df$member_id[df$record_id == 3747] <- "YWC01M332"

# # load Eva's data
# XLConnect::xlcFreeMemory() # rtools is also required to be installed to avoid error
# df2 <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20210426.xlsx", sheetName  = "Raw data"
#                       , encoding = "UTF-8"
#                       , header = FALSE
#                       )
# df2 <- df2[2:nrow(df2),]
# names(df2) <- c(names(df), names(df2)[87:96])

# survey data cleaning ----
temp <- xlsx::read.xlsx2("Raw Data (from Oct 8) and Summary 20211004_duplicates.xlsx", sheetName  = "duplicate record"
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
# df$record_id[df$ehealth_eval_timestamp %in% ""] # show record IDs with missing timestamps
df$ehealth_eval_timestamp[df$record_id %in% c("846", "852", "857", "872")] <-
  c("2020-10-15 15:01:00", "2020-10-15 15:45:00", "2020-10-15 16:20:00", "2020-10-16 10:00:00")

# fix problematic member IDs
# gsub("\t", "", "\tSAG03M223")
df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)] <- 
  gsub("\t", "", df$member_id[grepl("\t", df$member_id, ignore.case = TRUE)]) # remove "\t"

# df$member_id <- gsub("[[:space:]]", "", df$member_id) # remove extra spaces

df$ehealth_eval_timestamp <- lubridate::ymd_hms(df$ehealth_eval_timestamp, tz = "Asia/Hong_Kong", quiet =  TRUE)

# check duplicates
df %>%
  add_count(member_id, evaluation_event) %>%
  filter(n >= 2) %>% nrow()

# scoring <- function(df){
  df %>% dplyr::select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
  df %>% dplyr::select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
  df %>% dplyr::select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
  df %>% dplyr::select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
  df %>% dplyr::select(starts_with("diet_")) %>% colnames(.) -> diet_
  df %>% dplyr::select(starts_with("eq5d")) %>% colnames(.) -> eq5d_
  
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
  
# score EQ-5D-5L
score_EQ5D <- function(df){
  df[eq5d_[1:5]] <-  (df[eq5d_[1:5]]-6)*-1 # reverse EQ5D items
  df$eq5d_score <- ifelse(is.na(df$eq5d_mobility) | is.na(df$eq5d_self_care) | is.na(df$eq5d_usual_activity) | 
                            is.na(df$eq5d_pain_discomfort) | is.na(df$eq5d_anxiety_depression),
                          NA, paste0(df$eq5d_mobility, df$eq5d_self_care, df$eq5d_usual_activity, df$eq5d_pain_discomfort, df$eq5d_anxiety_depression))
  df$eq5d =  eq5d_fast(scores=df$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)
  return(subset(df, select = c(eq5d_score, eq5d)))
}
df <- cbind(df, score_EQ5D(df))

df$amic <- ifelse(df$amic_sum >= 3, 1, 0)
val_labels(df$amic) <- c("AMIC >= 3" = 1, "AMIC < 3" = 0)
df$pase_c_12_1 <- ifelse(df$pase_c_12 > 0, 1, 0) # Walking in general as binary
val_labels(df$pase_c_12_1) <- c(Yes=1, No=0)

# import WBS data ----
setwd(sprintf("~%s/ehealth/JCreport 20210409/data/Overall", setpath))
wbs0 <- foreign_to_labelled(haven::read_sav("EHealth_NEW_DATA_WBS_Complete_2021-01-31_Overall.sav", encoding = "UTF-8")) # Sys.setlocale(category = "LC_ALL", locale = "cht")

setwd(sprintf("~%s/ehealth/wbs", setpath))
wbs <- openxlsx::read.xlsx("EHealth_NEW_DATA_WBS_Complete_IOA_2021-09-15.xlsx", sheet  = "Raw data"
) # latest WBS all data
wbs$Survey_date <- as.Date(as.numeric(wbs$Survey_date), origin = "1899-12-30")
wbs$All_forms_completed_date <- as.Date(as.numeric(wbs$All_forms_completed_date), origin = "1899-12-30")
wbs$Birth_date <- as.Date(as.numeric(wbs$Birth_date), origin = "1899-12-30")

wbs2 <- openxlsx::read.xlsx("EHealth_NEW_DATA_WBS_Complete_SCHSA_2021-09-04.xlsx", sheet  = "Raw data"
                         ) # latest WBS high-risk data
wbs2$Survey_date <- as.Date(as.numeric(wbs2$Survey_date), origin = "1899-12-30")
wbs2$All_forms_completed_date <- as.Date(as.numeric(wbs2$All_forms_completed_date), origin = "1899-12-30")
wbs2$Birth_date <- as.Date(as.numeric(wbs2$Birth_date), origin = "1899-12-30")

for (i in c("wbs0", "wbs"
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


# vars <- c("gender", "dob", "wbs_survey_date",
#           "marital", "educ", "living_status", "housing_type",
#           "risk_score", "risk_level", "digital", )


vars <- c("Survey_centre", "wbs_survey_date", "gender", "dob",
          "Phase1_member_self_report", "DH_centre_member", "Carer", "Hypertension", "Hypertension_HA",
          "Diabetes", "Diabetes_HA", "Cholesterol", "Heart", "Heart_score", "Stroke", "Copd", "Renal", 
          "Disease_other", "Disease_other_indicate", "marital", "educ", "Income_oaa", "Income_oala", 
          "Income_ssa", "Income_work", "Income_saving", "Income_cssa", "Income_cssa_score", "Income_pension",
          "Income_child", "Income_other", "living_status", "housing_type", "Rent", "Own", "FS1", "FS2", "FS3",
          "FS4", "FS5", "FS_total", "FS_score", "SAR1", "SAR2",  "SAR3", "SAR4", "SAR5", "SAR_total", "AMIC",
          "AMIC_score", "Self_rated_health", "Self_rated_health_score", "Satisfaction", "Satisfaction_score",
          "Meaning_of_life", "Happiness", "Happiness_score", "Incontinence", "Hospital", "Hospital_day", 
          "Hospital_score", "Aeservices", "Aeservices_day", "SOPD", "GOPD", "Clinic", "Elderly_centre", 
          "Drug_use", "Drug_use_score", "risk_score", "risk_level", "digital", "Centre", "NGO")

# replace outdated full WBS data with latest high-risk WBS data
wbs0 <- wbs0[wbs0$member_id %!in% wbs2$member_id, ] # keep members not found in latest high-risk WBS data
wbs0$All_forms_completed_date <- NA
names(wbs2)[names(wbs2)=="Memory"] <- "AMIC"
names(wbs2)[names(wbs2)=="Memory_score"] <- "AMIC_score"
names(wbs)[names(wbs)=="Memory"] <- "AMIC"
names(wbs)[names(wbs)=="Memory_score"] <- "AMIC_score"

wbs2$risk_level <- car::recode(wbs2$risk_level, "
'L' = 1;
'M' = 2;
'H' = 3
")

wbs$risk_level <- car::recode(wbs$risk_level, "
'L' = 1;
'M' = 2;
'H' = 3
")

# copy labels from WBS to WBS2 & wbs
for (var in names(wbs2)){
  if (class(wbs0[[var]])[1] == "haven_labelled" &
      var %!in% c("SAR_total")){
    if (typeof(wbs0[[var]])[1] == "double"){
      wbs2[[var]] <- labelled(as.numeric(wbs2[[var]]), val_labels(wbs0[[var]]))
      wbs[[var]] <- labelled(as.numeric(wbs[[var]]), val_labels(wbs0[[var]]))
    } else if (typeof(wbs0[[var]])[1] == "character"){
      wbs2[[var]] <- labelled(wbs2[[var]], val_labels(wbs0[[var]]))
      wbs[[var]] <- labelled(wbs[[var]], val_labels(wbs0[[var]]))
    }
  } else if (class(wbs0[[var]])[1] == "numeric" |
             var %in% c("SAR_total")){
    wbs2[[var]] <- as.numeric(wbs2[[var]])
    wbs[[var]] <- as.numeric(wbs[[var]])
  } 
}

# wbs2 <- wbs2[order(wbs2$wbs_survey_date),] # order by WBS survey date
# wbs2 <- distinct(wbs2, member_id, .keep_all = TRUE) # keep only first instance (i.e. remove any repeats)

# recode WBS rounds
wbs <- wbs[order(wbs$member_id, wbs$wbs_survey_date),] # order by WBS survey date
wbs <- wbs %>% group_by(member_id) %>% mutate(id_row = row_number()) # %>% add_count(member_id, Round) %>% filter(n==2) %>% dplyr::select(wbs_survey_date, member_id, Round, id_row) %>% View()
wbs$Round <- ifelse(wbs$id_row != wbs$Round, wbs$id_row, wbs$Round)
wbs <- wbs[wbs$id_row != 3, ]
wbs <- distinct(wbs, member_id, Round, .keep_all = TRUE) # keep only first instance 

wbs <- as.data.frame(wbs)
wbs <- convert2NA(wbs, "")

Sys.setlocale(locale =  "cht") 
wbs$Hospital_day <- car::recode(wbs$Hospital_day, "
c('一次', '半日', 'l') = 1;
'2次' = 2;
'三次' = 3;
'6日' = 6
")
wbs$Aeservices_day  <- car::recode(wbs$Aeservices_day, "
c('一', '一次', '一次。', '〉1', '跌傷', '1次 耳水不平衡', '1次', '1係', 'I', 'l', 'I ') = 1;
'1-2' = 1.5;
'2次' = 2;
'2-3' = 2.5;
'3次' = 3;
c('3-4次', '3-4') = 3.5;
'4次' = 4;
'超過4次' = 5;
'5-6' = 5.5;
c(' 6', '64') = 6;
'8～10' = 9
")
Sys.setlocale(locale =  "eng") 
wbs$Hospital_day <- as.numeric(wbs$Hospital_day)
wbs$Hospital_day <- ifelse(wbs$Hospital_day %in% NA, 0, wbs$Hospital_day)
wbs$Hospital <- ifelse(wbs$Hospital_day == 0, 0, wbs$Hospital)

wbs$Aeservices_day <- as.numeric(wbs$Aeservices_day)
wbs$Aeservices_day <- ifelse(wbs$Aeservices_day %in% NA, 0, wbs$Aeservices_day)
wbs$Aeservices <- ifelse(wbs$Aeservices_day == 0, 0, wbs$Aeservices)

df$member_id <- toupper(df$member_id)
df <- merge(df, wbs[wbs$Round == 1, c("member_id", vars)], # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

# # replace the few missing values from latest wbs data (dob, gender, etc)
# for (var in vars){
#   temp <- data.frame(member_id = wbs2$member_id[wbs2$member_id %in% df$member_id[is.na(df[[var]]) & df$member_id %in% wbs2$member_id]],
#                      x = wbs2[[var]][wbs2$member_id %in% df$member_id[is.na(df[[var]]) & df$member_id %in% wbs2$member_id]])
#   names(temp)[names(temp)=="x"] <- var
#   df <- merge(df, temp, # extract item matched by member ID
#               by=c("member_id"), all = TRUE)
#   df[[var]] <- if_else(is.na(df[[paste0(var, ".x")]]), df[[paste0(var, ".y")]], df[[paste0(var, ".x")]])
#   df[[paste0(var, ".x")]] <- NULL
#   df[[paste0(var, ".y")]] <- NULL
#   rm(temp)
# }

df$age <- floor(interval(df$dob, df$ehealth_eval_timestamp) / duration(num = 1, units = "years")) # https://stackoverflow.com/a/27363833

df$age_group <- recode_age(df$age, age_labels = c("60-69", "70-79", "80+"))
df$age_group <- relevel(as.factor(df$age_group), ref = "60-69")

df <- df[df$ehealth_eval_complete == 2,] # keep only completed records

# wbs2$risk_score <- wbs2$Age_score + wbs2$Heart_score + wbs2$Income_cssa_score + wbs2$FS_score + wbs2$AMIC_score + wbs2$Self_rated_health_score + wbs2$Satisfaction_score + wbs2$Happiness_score + wbs2$Hospital_score + wbs2$Drug_use_score

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

