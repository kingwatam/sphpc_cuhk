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

Sys.setlocale(locale =  "cht") # Chinese comma isn't recognised in to_English unless locale set to Chinese
setwd(sprintf("~%s/multimorbidity/2016_2018_data", setpath))
df <- haven::read_dta("2016-2018 dataset.dta") # import T0 & T1 data
names(df)[names(df) == "var379"] <- "initial"
df <- convert2NA(df, c("#N/A", "."))


# Replace Chinese characters ----
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
df$datef1 <- as.Date(as.numeric(df$datef1), origin = "1899-12-30")

df %>% select(starts_with(c("phq", "gad", "ls", "isi", "eq5d", "sar", "efs", "meaning", "sleep"))) %>% colnames(.) 

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
t2$Timestamp <- NULL # remove column
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
# Rater
names(t2)[49] <- "raterf2"
# Insomnia (single-item)
names(t2)[50] <- "sleepf2"
t2["sleepf2"] <- as.numeric(sapply(t2["sleepf2"], before_char, "=") )
t2$sleepf2[t2$sleepf2 == ""] <- NA


scoring <- function(t2) {
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

  # outcome scoring
  t2 <- t2 %>%
    mutate(
      phqf2 = rowSums(.[q_phq], na.rm = FALSE),
      gadf2 = rowSums(.[q_gad], na.rm = FALSE),
      Loneliness_Tf2 = rowSums(.[q_ls[1:6]], na.rm = FALSE),
      Loneliness_Ef2 = rowSums(.[q_ls[1:3]], na.rm = FALSE), # emotional loneliness
      Loneliness_Sf2 = rowSums(.[q_ls[4:6]], na.rm = FALSE), # social loneliness
      Loneliness_Qf2 = .[, q_ls[7]],
      isif2 = rowSums(.[q_isi], na.rm = FALSE),
      SAR_totalf2 = rowSums(.[q_sar], na.rm = FALSE)
    )
  t2$EQ5D_HKf2 =  eq5d(scores=t2$eq5d_score, country="HongKong", version="5L", type="VT", ignore.invalid = TRUE)

  return(subset(t2, select = c(phqf2, gadf2, Loneliness_Tf2, Loneliness_Ef2, Loneliness_Sf2, Loneliness_Qf2,
                               isif2, EQ5D_HKf2, SAR_totalf2
                               )))
}
t2 <- cbind(t2, scoring(t2))

saveRDS(t2, "2020_data_cleaned.rds")

