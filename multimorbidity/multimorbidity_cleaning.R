rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(labelled) # foreign_to_labelled
library(dplyr)
library(data.table) # %like%
library(eq5d)
library(lubridate) # year()

Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese

# import T0 & T1 data ----
setwd(sprintf("~%s/multimorbidity/2016_2018_data", setpath))
df <- haven::read_dta("2016-2018 dataset.dta") # import T0 & T1 data
names(df)[names(df) == "var379"] <- "initialf1"
names(df)[names(df) == "pase6a"] <- "pase6af0"
names(df)[names(df) == "rd40"] <- "rd4f0"
names(df)[names(df) == "sit"] <- "sitf1"
names(df)[names(df) == "hcuhsp1bvf"] <- "hcuhsp1bf1"
names(df)[names(df) == "medno"] <- "mednof1"
names(df)[names(df) == "abusebodyf"] <- "abusebodyf1"
names(df)[names(df) == "abusepplof"] <- "abusepplof1"
df <- convert2NA(df, c("#N/A", "."))

# Replace Chinese characters
setwd(sprintf("~%s/multimorbidity/archive", setpath))
temp <- foreign_to_labelled(haven::read_sav("jc_acitivity_FU1 2019Mar26 v2.sav", encoding = "MS936")) # old version of T0T1 data

# names(df)[names(df) %!in% names(temp)] # different names

# replace entries with wrongly encoded Chinese characters from original entries from old dataset
for (c in (1:ncol(df))){
  if (class(df[[c]]) == "character" & !(all(is.na(df[[c]])))){ # only character class, excluding columns with all NAs
    if (grepl("^[[:digit:]]", df[[c]][complete.cases(df[[c]])]) %>% all()){ #  check if char is numeric, equiv. to varhandle::check.numeric(df[[c]], na.rm = TRUE) %>% all()
      df[[c]] <- as.numeric(df[[c]]) # convert class from character to numeric if all values are numeric 
    }
  }
  
  x <- (df[df[[c]] %like% "\\?\\?", c]) 
  if (nrow(x) != 0){
    var <- names(df)[c]
    sopds <- df$sopd[df[[c]] %like% "\\?"]
    print(x)
    print(sopds)
    for (id in sopds){
      id_list <- data.frame(
        new_ids = c("GLYH0515790O", "GYCK1810707S", "GLYH1022138V"),
        old_ids = c("GLYH05157900", "H4366567", "GYCK1620078U")
        )
      
      new_id <- id
      old_id <- ifelse(any(id_list$new_ids == id), id_list$old_ids[id_list$new_ids == id], id) # revert back to old problematic id when true
      df[df$sopd == new_id, var] <- iferror(temp[temp$sopd == old_id, var], {warning("Variable name not found!"); next()})
      print(temp[temp$sopd == id, var])
    }
    }
}
rm(temp, id_list, id, new_id, old_id, sopds, var, c, x)

df$datef0 <- as.Date(df$datef0, origin = "1899-12-30")
df$datef1 <- as.Date(df$datef1, origin = "1899-12-30")
df$phq2tf1 <- df$phq1f1 + df$phq2f1 # PHQ-2
df$gad2tf1 <- df$gad1f1  + df$gad2f1  # GAD-2

df$isitf0 <- car::recode(df$isif0, "
0:7 = 1;
8:14 = 2;
15:21 = 3;
22:hi = 4
")  # recalculate categorical ISI (fix mis-coding)

df$isitf1 <- car::recode(df$isif1, "
0:7 = 1;
8:14 = 2;
15:21 = 3;
22:hi = 4
")   # recalculate categorical ISI (fix mis-coding)

# male = 1, female = 0
df$genderf0 <- ifelse(df$genderf0 == 1, "M", "F")

scoring_t0t1 <- function(df){
  df %>% select(starts_with("ls") & ends_with("f0")) %>% colnames(.) -> q_ls_f0
  df %>% select(starts_with("ls") & ends_with("f1")) %>% colnames(.) -> q_ls_f1
  df %>% select(starts_with("eq5d") & ends_with("f0")) %>% colnames(.) -> q_eq5d_f0
  df %>% select(starts_with("eq5d") & ends_with("f1")) %>% colnames(.) -> q_eq5d_f1
  
  df[q_ls_f0[1:3]] <- sapply(df[q_ls_f0[1:3]], car::recode, "
  1:2 = 1
  ") # recode (0,1,2) to (0,1,1) for scoring
  df[q_ls_f0[4:6]] <- sapply(df[q_ls_f0[4:6]], car::recode, "
  c(2,0) = 1;
  1 = 0
  ") # recode (0,1,2) to (1,0,1) for scoring
  
  df[q_ls_f1[1:3]] <- sapply(df[q_ls_f1[1:3]], car::recode, "
  1:2 = 1
  ") # recode (0,1,2) to (0,1,1) for scoring
  df[q_ls_f1[4:6]] <- sapply(df[q_ls_f1[4:6]], car::recode, "
  c(2,0) = 1;
  1 = 0
  ") # recode (0,1,2) to (1,0,1) for scoring
  
  # reverse EQ5D items
  df[q_eq5d_f0[1:5]] <-  (df[q_eq5d_f0[1:5]]-6)*-1
  df[q_eq5d_f1[1:5]] <-  (df[q_eq5d_f1[1:5]]-6)*-1
  
  df$eq5d_scoref0 <- ifelse(is.na(df$eq5d1f0) | is.na(df$eq5d2f0) | is.na(df$eq5d3f0) | is.na(df$eq5d4f0) | is.na(df$eq5d5f0),
                            NA, paste0(df$eq5d1f0, df$eq5d2f0, df$eq5d3f0, df$eq5d4f0, df$eq5d5f0))
  df$eq5d_scoref1 <- ifelse(is.na(df$eq5d1f1) | is.na(df$eq5d2f1) | is.na(df$eq5d3f1) | is.na(df$eq5d4f1) | is.na(df$eq5d5f1),
                            NA, paste0(df$eq5d1f1, df$eq5d2f1, df$eq5d3f1, df$eq5d4f1, df$eq5d5f1))
  
  df$eq5df0 =  eq5d_fast(scores=df$eq5d_scoref0, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)
  df$eq5df1 =  eq5d_fast(scores=df$eq5d_scoref1, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)

  df <- df %>%
    mutate(
      lsf0 = rowSums(.[q_ls_f0[1:6]], na.rm = FALSE),
      ls_ef0 = rowSums(.[q_ls_f0[1:3]], na.rm = FALSE), # emotional loneliness
      ls_sf0 = rowSums(.[q_ls_f0[4:6]], na.rm = FALSE), # social loneliness
      # ls_qf0 = .[[q_ls_f0[7]]],
      lsf1 = rowSums(.[q_ls_f1[1:6]], na.rm = FALSE),
      ls_ef1 = rowSums(.[q_ls_f1[1:3]], na.rm = FALSE), # emotional loneliness
      ls_sf1 = rowSums(.[q_ls_f1[4:6]], na.rm = FALSE), # social loneliness
      # ls_qf1 = .[[q_ls_f1[7]]]
    )
  
  return(subset(df, select = c(lsf0, ls_ef0, ls_sf0, 
                               # ls_qf0, 
                               lsf1, ls_ef1, ls_sf1, 
                               # ls_qf1,
                               eq5df0, eq5df1)))
}
df <- cbind(df, scoring_t0t1(df))

t0t1 <- df
rm(df)

# data clean T2 data ----
setwd(sprintf("~%s/multimorbidity", setpath))
t2 <- xlsx::read.xlsx2("2016-2019 JC project-follow-up II (Responses)_king.xlsx", sheetName  = "Form responses 1"
                       , encoding = "UTF-8"
                       , header = FALSE
)
t2 <- t2[2:nrow(t2),] # remove header
names(t2)[1:5] <- c("Timestamp", "Case.number.", "Patient.initial.", "Age", "datef2")
t2$Timestamp <- as.Date(as.numeric(t2$Timestamp), origin = "1899-12-30") # Timestamp from Google Form
t2$datef2 <- as.Date(as.numeric(t2$datef2), origin = "1899-12-30") # date of assessment
names(t2)[names(t2) == "Case.number."] <- "sopd"
t2 <- t2[1:50] # remove last variable for encoding
# data clean T2 data
# PHQ
names(t2)[6:14] <- sprintf("phq%sf2", 1:9)
q_phq <- names(t2)[6:14]
t2[q_phq] <- as.numeric(sapply(t2[q_phq], after_char, "=") )
# GAD
names(t2)[15:21] <- sprintf("gad%sf2", 1:7)
q_gad <- names(t2)[15:21]
t2[q_gad] <- as.numeric(sapply(t2[q_gad], after_char, "=") )
# loneliness scale
names(t2)[22:28] <- sprintf("ls%sf2", 1:7)
q_ls <- names(t2)[22:28]
t2[q_ls[1:6]] <- as.numeric(sapply(t2[q_ls[1:6]], before_char, "*") )
t2[q_ls[7]] <- as.numeric(sapply(t2[q_ls[7]], before_char, "=") )
# ISI
names(t2)[29:35] <- sprintf("isi%sf2", 1:7)
q_isi <- names(t2)[29:35]
t2[q_isi] <- as.numeric(sapply(t2[q_isi], before_char, "=") )
# EQ5D
names(t2)[36:41] <- sprintf("eq5d%sf2", 1:6)
q_eq5d <- names(t2)[36:41]
t2[q_eq5d[1:5]] <- as.numeric(sapply(t2[q_eq5d[1:5]], before_char, ")") )
t2$eq5d6f2 <- ifelse(t2$eq5d6f2 == "", NA, t2$eq5d6f2)
t2$eq5d6f2 <- as.numeric(t2$eq5d6f2)
# SAR (sarcopenia)
names(t2)[42:46] <- sprintf("sar%sf2", 1:5)
q_sar <- names(t2)[42:46]
t2[q_sar] <- as.numeric(sapply(t2[q_sar], before_char, "=") )
# support
names(t2)[47] <- "efs4f2"
t2["efs4f2"] <- as.numeric(sapply(t2["efs4f2"], before_char, "=") )
# meaning of life
names(t2)[48] <- "meaningf2"
t2$meaningf2[t2$meaningf2 == ""] <- NA
t2$meaningf2 <- as.numeric(t2$meaningf2)
# Rater
names(t2)[49] <- "raterf2"
# Insomnia (single-item)
names(t2)[50] <- "sleepf2"
t2["sleepf2"] <- as.numeric(sapply(t2["sleepf2"], before_char, "=") )
t2$sleepf2[t2$sleepf2 == ""] <- NA

t2$Timestamp <- NULL # remove column

scoring_t2 <- function(t2) {
  t2[q_ls[1:3]] <- sapply(t2[q_ls[1:3]], car::recode, "
  1:2 = 1
  ") # recode (0,1,2) to (0,1,1) for scoring
  t2[q_ls[4:6]] <- sapply(t2[q_ls[4:6]], car::recode, "
  c(2,0) = 1;
  1 = 0
  ") # recode (0,1,2) to (1,0,1) for scoring
  
  # reverse EQ5D items
  t2[q_eq5d[1:5]] <-  (t2[q_eq5d[1:5]]-6)*-1
  
  t2$eq5d_score <- ifelse(is.na(t2$eq5d1f2) | is.na(t2$eq5d2f2) | is.na(t2$eq5d3f2) | is.na(t2$eq5d4f2) | is.na(t2$eq5d5f2),
                          NA, paste0(t2$eq5d1f2, t2$eq5d2f2, t2$eq5d3f2, t2$eq5d4f2, t2$eq5d5f2))
  t2$eq5df2 =  eq5d_fast(scores=t2$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)
  
  # outcome scoring
  t2 <- t2 %>%
    mutate(
      phq2tf2 =  rowSums(.[q_phq[1:2]], na.rm = FALSE), # PHQ-2
      phqf2 = rowSums(.[q_phq], na.rm = FALSE),
      gad2tf2 =  rowSums(.[q_gad[1:2]], na.rm = FALSE), # GAD-2
      gadf2 = rowSums(.[q_gad], na.rm = FALSE),
      lsf2 = rowSums(.[q_ls[1:6]], na.rm = FALSE),
      ls_ef2 = rowSums(.[q_ls[1:3]], na.rm = FALSE), # emotional loneliness
      ls_sf2 = rowSums(.[q_ls[4:6]], na.rm = FALSE), # social loneliness
      # ls_qf2 = .[[q_ls[7]]],
      isif2 = rowSums(.[q_isi], na.rm = FALSE),
      sarf2 = rowSums(.[q_sar], na.rm = FALSE)
    )
  
  t2$isitf2 <- car::recode(t2$isif2, "
  0:7 = 1;
  8:14 = 2;
  15:21 = 3;
  22:hi = 4
  ")  
  
  return(subset(t2, select = c(phq2tf2, phqf2, gad2tf2, gadf2, lsf2, ls_ef2, ls_sf2, 
                               # ls_qf2,
                               isif2, eq5df2, sarf2, isitf2
  )))
}
t2 <- cbind(t2, scoring_t2(t2))


# data clean T3 data ----
setwd(sprintf("~%s/multimorbidity", setpath))
t3 <- xlsx::read.xlsx2("COVID-19 2021.xlsx", sheetName  = "Form responses 1"
                       , encoding = "UTF-8"
                       , header = FALSE
)
t3 <- t3 %>% select(c(1:5, 7:22, 29:35, 37:56, 6, 36, 23:28)) # reorder columns to match T2's
t3 <- t3[2:nrow(t3),] # remove header
names(t3)[1:5] <- c("Timestamp", "Case.number.", "Patient.initial.", "Age", "datef3")
t3$Timestamp <- as.Date(as.numeric(t3$Timestamp), origin = "1899-12-30") # Timestamp from Google Form
t3$datef3 <- as.Date(as.numeric(t3$datef3), origin = "1899-12-30") # date of assessment
names(t3)[names(t3) == "Case.number."] <- "sopd"
# data clean t3 data
# PHQ
names(t3)[6:14] <- sprintf("phq%sf3", 1:9)
q_phq <- names(t3)[6:14]
t3[q_phq] <- as.numeric(sapply(t3[q_phq], after_char, "=") )
# GAD
names(t3)[15:21] <- sprintf("gad%sf3", 1:7)
q_gad <- names(t3)[15:21]
t3[q_gad] <- as.numeric(sapply(t3[q_gad], after_char, "=") )
# loneliness scale
names(t3)[22:28] <- sprintf("ls%sf3", 1:7)
q_ls <- names(t3)[22:28]
t3[q_ls[1:6]] <- as.numeric(sapply(t3[q_ls[1:6]], before_char, "*") )
t3[q_ls[7]] <- as.numeric(sapply(t3[q_ls[7]], before_char, "=") )
# ISI
names(t3)[29:35] <- sprintf("isi%sf3", 1:7)
q_isi <- names(t3)[29:35]
t3[q_isi] <- as.numeric(sapply(t3[q_isi], before_char, "=") )
# EQ5D
names(t3)[36:41] <- sprintf("eq5d%sf3", 1:6)
q_eq5d <- names(t3)[36:41]
t3[q_eq5d[1:5]] <- as.numeric(sapply(t3[q_eq5d[1:5]], before_char, ")") )
t3$eq5d6f3 <- ifelse(t3$eq5d6f3 %in% c("", "refuse to ans"), NA, t3$eq5d6f3)
t3$eq5d6f3 <- as.numeric(t3$eq5d6f3)
# SAR (sarcopenia)
names(t3)[42:46] <- sprintf("sar%sf3", 1:5)
q_sar <- names(t3)[42:46]
t3[q_sar] <- as.numeric(sapply(t3[q_sar], before_char, "=") )
# support
names(t3)[47] <- "efs4f3"
t3["efs4f3"] <- as.numeric(sapply(t3["efs4f3"], before_char, "=") )
# meaning of life
names(t3)[48] <- "meaningf3"
t3$meaningf3[t3$meaningf3 == ""] <- NA
t3$meaningf3 <- as.numeric(t3$meaningf3)
# Rater
names(t3)[49] <- "raterf3"
# Insomnia (single-item)
names(t3)[50] <- "sleepf3"
t3["sleepf3"] <- as.numeric(sapply(t3["sleepf3"], before_char, "=") )
t3$sleepf3[t3$sleepf3 == ""] <- NA
# Education
names(t3)[51] <- "genderf3"
names(t3)[52] <- "eduf3"
t3$eduf3 <- ifelse(t3$eduf3 %in% c("", "do not remember"), NA, 
                   ifelse(t3$eduf3 == "大學畢業", 16, t3$eduf3))
t3$eduf3 <- as.numeric(t3$eduf3)

names(t3)[53] <- "moca5m1f3" 
names(t3)[54] <- "moca5m2f3"
names(t3)[55] <- "moca5m3f3"
names(t3)[56] <- "moca5m4f3"
q_moca5m <- t3 %>% select(starts_with("moca5m")) %>% names(.)
t3[q_moca5m] <- sapply(t3[q_moca5m], FUN = function(x) ifelse(x == "", NA, x))
t3[q_moca5m] <- sapply(t3[q_moca5m], as.numeric)

t3$Timestamp <- NULL # remove column

# fix dates (survey ranged from Feb to June 2021)
year(t3$datef3) <- 2021
# # manually fixed month & day already, below code unnecessary
# t3$temp_datef3 <- t3$datef3 # temporary variable
# day(t3$datef3) <- ifelse(month(t3$datef3) == 11, month(t3$temp_datef3), day(t3$datef3)) # swap day with month
# month(t3$datef3) <- ifelse(month(t3$datef3) == 11, day(t3$temp_datef3), month(t3$datef3)) # swap month with day
# t3$temp_datef3 <- NULL

# remove duplicate "GLYH0514070U"
t3 <- t3[order(t3$datef3, decreasing = FALSE),] # order by date so only earliest observation is kept for duplicates
t3 <- distinct(t3, sopd, .keep_all = TRUE) # keep only first instance of sopd (i.e. remove any repeats)

scoring_t3 <- function(t3) {
  t3[q_ls[1:3]] <- sapply(t3[q_ls[1:3]], car::recode, "
  1:2 = 1
  ") # recode (0,1,2) to (0,1,1) for scoring
  t3[q_ls[4:6]] <- sapply(t3[q_ls[4:6]], car::recode, "
  c(2,0) = 1;
  1 = 0
  ") # recode (0,1,2) to (1,0,1) for scoring
  
  # reverse EQ5D items
  t3[q_eq5d[1:5]] <-  (t3[q_eq5d[1:5]]-6)*-1
  
  t3$eq5d_score <- ifelse(is.na(t3$eq5d1f3) | is.na(t3$eq5d2f3) | is.na(t3$eq5d3f3) | is.na(t3$eq5d4f3) | is.na(t3$eq5d5f3),
                          NA, paste0(t3$eq5d1f3, t3$eq5d2f3, t3$eq5d3f3, t3$eq5d4f3, t3$eq5d5f3))
  t3$eq5df3 =  eq5d_fast(scores=t3$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)
  
  # outcome scoring
  t3 <- t3 %>%
    mutate(
      phq2tf3 =  rowSums(.[q_phq[1:2]], na.rm = FALSE), # PHQ-2
      phqf3 = rowSums(.[q_phq], na.rm = FALSE),
      gad2tf3 =  rowSums(.[q_gad[1:2]], na.rm = FALSE), # GAD-2
      gadf3 = rowSums(.[q_gad], na.rm = FALSE),
      lsf3 = rowSums(.[q_ls[1:6]], na.rm = FALSE),
      ls_ef3 = rowSums(.[q_ls[1:3]], na.rm = FALSE), # emotional loneliness
      ls_sf3 = rowSums(.[q_ls[4:6]], na.rm = FALSE), # social loneliness
      # ls_qf3 = .[[q_ls[7]]],
      isif3 = rowSums(.[q_isi], na.rm = FALSE),
      sarf3 = rowSums(.[q_sar], na.rm = FALSE),
      moca5mf3 = rowSums(.[q_moca5m], na.rm = FALSE)
    )
  
  t3$isitf3 <- car::recode(t3$isif3, "
  0:7 = 1;
  8:14 = 2;
  15:21 = 3;
  22:hi = 4
  ")  
  
  return(subset(t3, select = c(phq2tf3, phqf3, gad2tf3, gadf3, lsf3, ls_ef3, ls_sf3, 
                               # ls_qf3,
                               isif3, eq5df3, sarf3, moca5mf3, isitf3
  )))
}
t3 <- cbind(t3, scoring_t3(t3))

# update SOPD IDs ----
setwd(sprintf("~%s/multimorbidity", setpath))
sopd <- xlsx::read.xlsx('gopc checking.xlsx', sheetName  = "Sheet1"
                       , header = TRUE) # old and updated SOPD IDs 
sopd <- as.data.frame(sapply(sopd, toupper)) # capitalize IDs
t0t1$sopd <- gsub("[[:space:]]", "", toupper(t0t1$sopd) ) # remove spaces
t2$sopd <- gsub("[[:space:]]", "", toupper(t2$sopd) )
t3$sopd <- gsub("[[:space:]]", "", toupper(t3$sopd) )

# fix incorrect SOPD IDs in T3
t3$sopd[t3$sopd == "GYCK16121669O"] <- "GYCK1621669O"
t3$sopd[t3$sopd == "GYCK151857R"] <- "GLYH0517892S"
t3$sopd[t3$sopd == "B9477445"] <- "B9447445"
t3$sopd[t3$sopd == "GYCK07162975"] <- "GYCK0716297S"
t3$sopd[t3$sopd == "FMC20616818V"] <- "PHYA1613880Q" 
t3$sopd[t3$sopd == "FMC21610186V"] <- "FMC21610186U"
t3$sopd[t3$sopd == "GLYH08246085"] <- "GLYH0824608S"
t3$sopd[t3$sopd == "FMC210118725"] <- "FMC21011872S"
t3$sopd[t3$sopd == "GLYH1022343W"] <- "GLYH1622343W"
t3$sopd[t3$sopd == "GLYH0623980Q"] <- "GLYH0623986Q"
t3$sopd[t3$sopd == "GYCK12166990"] <- "GYCK1216699O"

# update old IDs with new IDs
for (i in 1:nrow(t2)){
  if (t2$sopd[i] %!in% unique(t0t1$sopd)){
    new_id <- NA # reset variable
    old_id <- t2$sopd[i]
    new_id <- sopd$corrected[grepl(old_id, sopd$initial, ignore.case = TRUE)]
    new_id <- ifelse(is.na(new_id), sopd$corrected[grepl(old_id, sopd$follow.up, ignore.case = TRUE)], new_id)
    t2$sopd[i] <- new_id
  }
}
for (i in 1:nrow(t3)){
  if (t3$sopd[i] %!in% unique(t0t1$sopd)){  
    new_id <- NA # reset variable
    old_id <- t3$sopd[i]
    new_id <- sopd$corrected[grepl(old_id, sopd$initial, ignore.case = TRUE)]
    new_id <- ifelse(is.na(new_id), sopd$corrected[grepl(old_id, sopd$follow.up, ignore.case = TRUE)], new_id)
    # print(paste(old_id, "old_id"))
    # print(new_id)
    t3$sopd[i] <- new_id
  }
}

# merge t0t1, t2, t3 data----
names(t2)[names(t2) == "Patient.initial."] <- "initialf2"
names(t3)[names(t3) == "Patient.initial."] <- "initialf3"
names(t2)[names(t2) == "Age"] <- "agef2"
names(t3)[names(t3) == "Age"] <- "agef3"

df <- merge(t0t1, t2, # extract item matched by case ID
            by=c("sopd"), all.x = TRUE)
df <- merge(df, t3, # extract item matched by case ID
            by=c("sopd"), all.x = TRUE)

# update SOPD IDs ----
df$sopd[df$sopd == "PHYA1613880Q"] <- "FMC20616818U" # correct SOPD id for FMC20616818V/PHYA1613880Q
# fix range issues ----
df$bp2asf0 <- ifelse(df$bp2asf0 > 200, round(df$bp2asf0/10, 0), df$bp2asf0) # pulse

df[c("bp1sf1", "bp1df1", "bp1asf1", "bp2sf1", 
     "bp2df1", "bp2asf1", "heightf1", "weightf1",
     "waistf1", "bmif1", "bmi2f1",
     "weighttf1", "moca5mf1")] <- sapply(df[c("bp1sf1", "bp1df1", "bp1asf1", "bp2sf1", 
                                                   "bp2df1", "bp2asf1", "heightf1", "weightf1",
                                                   "waistf1", "bmif1", "bmi2f1",
                                                   "weighttf1", "moca5mf1")], function(x) ifelse(x == 0, NA, x))

df$bmi2f0 <- ifelse(df$bmif0 < 18.5, 1, 
                    ifelse(df$bmif0 < 23, 2, 
                           ifelse(df$bmif0 < 25, 3, 4)))

df$bmi2f1 <- ifelse(df$bmif1 < 18.5, 1, 
                    ifelse(df$bmif1 < 23, 2, 
                           ifelse(df$bmif1 < 25, 3, 4)))

df$edu1f0 <- ifelse(df$eduf0 <= 6, 1, 0) # 6 years of educ or less
df$edu1f1 <- ifelse(df$eduf1 <= 6, 1, 0)
df$eduf0 <- ifelse(df$eduf0 == 55, df$eduf1, df$eduf0) # correct it from another wave
df$eduf3 <- ifelse(df$eduf3 %in% c(74, 76), df$eduf0, df$eduf3)

df$hcuaef1 <- ifelse(df$hcuaef1 > 2, 2, df$hcuaef1)# A&E vists >= 2 coded as 2
df$agepartnerf0 <- ifelse(df$agepartnerf0 < 18, NA, df$agepartnerf0) # age of partner
df$agef2 <- as.numeric(df$agef2)
df$agef3 <- as.numeric(df$agef3)

# from wide to long ----
# create empty columns for each possibility
vars <- df %>% select(ends_with(sprintf("f%s", 0:3))) %>% colnames %>% stringi::stri_replace_last_regex(str = ., pattern = "f0|f1|f2|f3", replacement = "")  %>% unique()
vars <- vars[order(vars)]
for (var in vars){
  for (t in sprintf("f%s", 0:3)){
    var_f <- paste0(var, t)
    if (is.null(df[[var_f]])){
      df[[var_f]] <- NA
      }
  }
}

# all_vars <- sapply(vars, function(x) sprintf(paste0(x, "f%s"), 0:3)) %>% as.vector # all combinations from f0 to f3
all_vars <- names(df)[names(df) %in% (df %>% select(ends_with(sprintf("f%s", 0:3))) %>% colnames)]
all_vars <- all_vars[order(all_vars)]
vars_list <- rep(list(c()), length(vars)) # create 449 empty elements in list

# group variables into groups of 4 for merging

for (i in (1:length(vars))){ # every 4th element
  vars_list[[i]] <- c(paste0(vars[i], "f0"), 
                      paste0(vars[i], "f1"),
                      paste0(vars[i], "f2"), 
                      paste0(vars[i], "f3")
                      )
}

dfwide <- df
df <- reshape(df,
              idvar = c("sopd"), # this line is to keep variables
              varying = vars_list,
              sep = "", 
              v.name = vars,
              # timevar = "time",
              # times = 0:3,
              direction = "long")

# ----

# t3 %>% select(starts_with(c("phq", "gad", "ls", "isi", "eq5d", "sar", "efs", "meaning", "sleep"))) %>% summ() 