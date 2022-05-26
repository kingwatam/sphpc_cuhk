rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# library(foreign)
library(labelled) # foreign_to_labelled
library(dplyr)

# import data & clean ----
setwd(sprintf("~%s/vaccination", setpath))

# import vaccination rates ----
temp <- read.csv("vaccination-rates-over-time-by-age.csv")
names(temp)[names(temp) == "ï..Date"] <- "date"
names(temp)[names(temp) == "Age.Group"] <- "age_cat"
names(temp)[names(temp) == "Sex"] <- "sex"

temp$age_cat <- ifelse(temp$age_cat %in% "80 and above", "80+", temp$age_cat)

temp$dose1_total <- temp$BioNTech.1st.dose + temp$Sinovac.1st.dose
temp$dose2_total <- temp$BioNTech.2nd.dose + temp$Sinovac.2nd.dose

temp <-  temp %>% group_by(age_cat, date) %>% summarise(dose1_total = sum(dose1_total),
                                               dose2_total = sum(dose2_total))

temp <- temp %>% group_by(age_cat, date) %>% 
  summarise(dose1_total = sum(dose1_total), 
            dose2_total = sum(dose2_total)) %>%
  mutate(dose1_cum = cumsum(dose1_total), 
         dose2_cum = cumsum(dose2_total)) %>% ungroup 

temp_60 <- temp %>% filter(age_cat %in% "60-69") 
temp_60$date <- as.Date(temp_60$date)
names(temp_60)[names(temp_60) == "dose1_cum"] <- "dose1_60"
names(temp_60)[names(temp_60) == "dose2_cum"] <- "dose2_60"

temp_70 <- temp %>% filter(age_cat %in% "70-79") 
temp_70$date <- as.Date(temp_70$date)
names(temp_70)[names(temp_70) == "dose1_cum"] <- "dose1_70"
names(temp_70)[names(temp_70) == "dose2_cum"] <- "dose2_70"

temp_80 <- temp %>% filter(age_cat %in% "80+") 
temp_80$date <- as.Date(temp_80$date)
names(temp_80)[names(temp_80) == "dose1_cum"] <- "dose1_80"
names(temp_80)[names(temp_80) == "dose2_cum"] <- "dose2_80"

gov <- data.frame(date = seq(min(as.Date(temp$date)), max(as.Date(temp$date)), by = "1 day"))
gov <- merge(gov, temp_60[, c("date", "dose1_60", "dose2_60")], # extract item matched by case ID
                        by=c("date"), all.x = TRUE)
gov <- merge(gov, temp_70[, c("date", "dose1_70", "dose2_70")], # extract item matched by case ID
                 by=c("date"), all.x = TRUE)
gov <- merge(gov, temp_80[, c("date", "dose1_80", "dose2_80")], # extract item matched by case ID
                 by=c("date"), all.x = TRUE)
gov$rate1_60 <- gov$dose1_60/1122100
gov$rate1_70 <- gov$dose1_70/591300
gov$rate1_80 <- gov$dose1_80/398200

gov$rate2_60 <- gov$dose2_60/1122100
gov$rate2_70 <- gov$dose2_70/591300
gov$rate2_80 <- gov$dose2_80/398200

rm(temp_60, temp_70, temp_80)

# import elderly data data ----
df <- foreign_to_labelled(haven::read_sav("Vaccine dataset 1208-0208.sav"))
df$age_cat <- recode_age(df$age, age_labels = unique(temp$age_cat))
names(df)[names(df) == "@3.1電話關顧日期"] <- "date"

df$vac <- ifelse(df[["@1b.接種新冠疫苗狀況"]] %in% "已接種新冠疫苗", 1, 
                 ifelse(df[["@1b.接種新冠疫苗狀況"]] %in% "", NA, 0))

df <- df %>% filter(age >= 60, Ques_info5x %in% 0:1, vac %!in% NA) # restrict to 60+ single/doubletons with info on vaccine status

df %>% group_by(age_cat) %>% summarise(median = median(date)) 
df %>% group_by(age_cat
                # , Ques_info5x
                ) %>% summarise(mean = mean(vac, na.rm = TRUE))  # %>% clipr::write_clip()

df <- df %>% group_by(date) %>% 
  summarise(weights = n()) 

gov <- merge(gov, df[, c("date", "weights")], # extract item matched by case ID
             by=c("date"), all.x = TRUE)

gov <- gov[gov$date <= max(df$date), ]
gov <- gov[gov$weights %!in% NA, ]

weighted.mean(gov$rate1_60, gov$weights, na.rm = TRUE)
weighted.mean(gov$rate1_70, gov$weights, na.rm = TRUE)
weighted.mean(gov$rate1_80, gov$weights, na.rm = TRUE)
