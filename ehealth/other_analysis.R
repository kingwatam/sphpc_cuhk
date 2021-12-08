rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(labelled)
library(lubridate) # interval() & duration()
library(lme4)
library(lmerTest)
library(ggplot2)
library(patchwork)
library(locfit) # locfit method for smoothing instead of loess

# import cleaned data
setwd(sprintf("~%s/ehealth", setpath))
df <- readRDS("ehealth_data.rds")
wbs <- readRDS("wbs_data.rds")

df %>% dplyr::select(starts_with("amic") & ends_with(sprintf("%s", 1:5))) %>% colnames(.) -> amic_
df %>% dplyr::select(starts_with("self_efficacy_")) %>% colnames(.) -> self_efficacy_ 
df %>% dplyr::select(starts_with("pase_c_") & ends_with("score")) %>% colnames(.) -> pase_c_
df %>% dplyr::select(starts_with("matrix_diet")) %>% colnames(.) -> matrix_diet_
df %>% dplyr::select(starts_with("diet_")) %>% colnames(.) -> diet_
df %>% dplyr::select(starts_with("eq5d")) %>% colnames(.) -> eq5d_

df$pase_c_11_1 <- ifelse(df$pase_c_11 == 0 , 0, df$pase_c_11_1)
df$pase_c_11 <- ifelse(df$pase_c_11_1 == 0 , 0, df$pase_c_11)

# total score of diet items ----
scoring <- function(df){
  reverse_matrix_diet <- matrix_diet_[c(1, 2)] # high-sugar/high-fat snacks & processed/canned foods
  df[reverse_matrix_diet] <- (df[reverse_matrix_diet]-4)*-1 # reverse (0:4) to (4:0)
  
  diet_ <- c(matrix_diet_, diet_)
  df <- df %>% 
    mutate(diet_sum = rowSums(.[diet_], na.rm = FALSE) 
    )
  
  
  return(subset(df, select = c(diet_sum
  )))
}

df <- cbind(df, scoring(df))

# restrict sample to age >= 60 ----
df <- df[as.Date(df$ehealth_eval_timestamp) <= as.Date('2021-11-15'),]
# df <- df[(df$ehealth_eval_timestamp) <= ('2021-06-28 10:00:00 HKT'),]
df <- df[which(df$age >= 60),]

df$time <- car::recode(df$evaluation_event, "
1 = 0;
2 = NA;
3 = 1;
4 = 2;
5 = 3;
6 = NA
") # (T0 = baseline, T1 = 6mth, T2 = 12mth, T3 = = 18mth)

# nursing protocol data ----
temp <- readxl::read_xlsx("nursing protocol _With Raw Data (20200201-20211031)_20211118.xlsm", sheet  = "data"
                          , col_names = TRUE, guess_max = 1220)
temp$main <- as.character(temp$main)

temp$main <- ifelse(temp$main %in% "1.1000000000000001", "1.1", temp$main)

gen_itemnum<- function(data){
  data$main <- zoo::na.locf(data$main)
  data$sub1[3:nrow(data)] <- zoo::na.locf(data$sub1[3:nrow(data)])
  data$sub2[4:nrow(data)] <- zoo::na.locf(data$sub2[4:nrow(data)])
  
  data$num <- apply(cbind(data$main, data$sub1, data$sub2, data$sub3), 1, 
                    function(x) paste(x[!is.na(x)], collapse = "."))
  data <- data %>% select(num, everything())
  
  return(subset(data, select = c(num)))
}
Sys.setlocale(locale =  "cht") # set locale to traditional Chinese
temp <- cbind(gen_itemnum(temp), temp)
Sys.setlocale(locale =  "eng") # set locale to traditional Chinese

temp$num <- ifelse(temp$main %!in% NA, temp$main, temp$num)

temp$num[nrow(temp)] <- "protocol_total"
temp <- temp %>% filter(stringr::str_count(temp$num, "\\.") <= 1) # count how many dots in string, restrict to major categories
temp$num <- ifelse(temp$num %in% "protocol_total", "protocol_total", paste0("protocol_", temp$num))

temp <- temp[c(1, 9:ncol(temp))]
temp <- as.data.frame(t(temp))
names(temp) <- temp[1,]
temp <- temp[c(2:(nrow(temp)-1)),] # remove first row
temp$member_id <- row.names(temp)
temp <- temp %>% select(member_id, everything())

temp[,2:ncol(temp)] <- sapply(temp[,2:ncol(temp)], as.numeric)

temp <- convert2value(temp, NA, 0)

# summarise protocol data (unique number of times)
temp1 <- temp
temp1[, 2:24] <- sapply(temp[, 2:24], FUN = function(x) ifelse(x>1, 1, x)) # truncate to 1

table <- data.frame(var_names = names(temp1[, 2:24]), N = sapply(temp1[, 2:24], FUN = function(x) sum(x))) 

temp1$protocol_3.1_b <- NULL # protocol 3.1 is same as protocol 3 (only one item under social health)
temp1 %>% select(starts_with("protocol"))  %>% colnames(.) -> protocol_varnames # include sub-totals and total
protocol_items <- c("protocol_1.1", "protocol_1.2", "protocol_1.3", 
                    "protocol_1.4", "protocol_1.5", "protocol_1.6", "protocol_1.7", "protocol_1.8", "protocol_1.9" ,
                    "protocol_1.10", "protocol_1.11", "protocol_1.12", "protocol_1.13", "protocol_1.14", "protocol_1.15", "protocol_1.16", 
                    "protocol_2.1", "protocol_2.2", "protocol_2.3", "protocol_3")

temp1 <- temp1 %>% mutate(protocol_total = rowSums(.[protocol_items], na.rm = FALSE))
names(temp1)[names(temp1) %in% protocol_varnames] <- paste0(names(temp1)[names(temp1) %in% protocol_varnames], "_b")

df <- merge(df, temp1[, c("member_id", paste0(protocol_varnames, "_b"))], 
            by=c("member_id"), all.x = TRUE)
rm(temp1, table)

df <- merge(df, temp[, c("member_id", 
                         "protocol_total", "protocol_1", "protocol_2", "protocol_3", protocol_items)], 
            by=c("member_id"), all.x = TRUE)

protocol <- temp
rm(temp)

# # protocol use statistics
# temp <- df
# temp <- temp[as.Date(temp$ehealth_eval_timestamp) <= as.Date('2021-10-31'),]
# temp <- temp %>% add_count(member_id, name = "n") %>% filter((n == 1 & time == 0)| (n == 2 & time == 1) | (n >= 2 & time == 2))
# 
# temp %>% group_by(gender, age_group) %>%
#   summarise_at(vars(!!!syms(paste0("protocol_total", "_b"))), funs(sum(., na.rm=TRUE))) %>% clipr::write_clip()
# 
# temp %>% group_by(gender, age_group) %>%
#   summarise_at(vars(!!!syms(paste0(c("protocol_total"), "_b"))), funs(n = n(), mean = mean(., na.rm=TRUE), sd = sd(., na.rm=TRUE))) %>% clipr::write_clip()


# nursing mode & freq data ----
temp <- xlsx::read.xlsx2("No. of nurse session_As of 20210831.xlsx", sheetIndex  = 1
                         , header = TRUE
)
temp <- convert2NA(temp, "")
names(temp)[names(temp) == "UID"] <- "member_id"
names(temp)[names(temp) == "Start.date"] <- "nursing_start_date"
names(temp)[names(temp) == "Status"] <- "status"
names(temp)[names(temp) == "No..of.Face.to.face.Session"] <- "f2f_num"
names(temp)[names(temp) == "No..of.Tele.mode.Session"] <- "telemode_num"
names(temp)[names(temp) == "Total"] <- "f2f_telemode_num"
temp$nursing_start_date <- as.Date(as.numeric(temp$nursing_start_date), origin = "1899-12-30")

temp$status <- ifelse(temp$status == "退出", 1, 0)
df <- merge(df, temp, # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

df$f2f_num <- as.numeric(df$f2f_num)
df$f2f_num <- ifelse(df$f2f_num %in% NA, 0, df$f2f_num)

df$telemode_num <- as.numeric(df$telemode_num)
df$telemode_num <- ifelse(df$telemode_num %in% NA, 0, df$telemode_num)

df$f2f_telemode_num <- as.numeric(df$f2f_telemode_num)
df$f2f_telemode_num <- df$f2f_num + df$telemode_num # seems some values in original total were miscalculated
df$f2f_pct <- df$f2f_num/df$f2f_telemode_num
df$telemode_pct <- df$telemode_num/df$f2f_telemode_num

temp <- df

temp <- temp %>% add_count(member_id, name = "n") %>% filter((n == 1 & time == 0)| (n == 2 & time == 1) | (n >= 2 & time == 2))

allVars <- c("f2f_num", "telemode_num", 
             "f2f_telemode_num", "telemode_pct", "Exercise", "Cognitive", "Nutritional", "Mental", "Social", "Others", "Total")

tableone::CreateTableOne(data =  temp,
                         strata = c("time"),
                         vars = allVars, test = FALSE) %>%
  print(showAllLevels = TRUE, nonnormal = NULL) %>% clipr::write_clip()

rm(temp)

# nursing assessment data ----
temp <- readxl::read_xlsx("Assessment_To CUHK_20211005_king.xlsx", sheet  = "20211005_"
                          , col_names = TRUE, guess_max = 3000)
temp <- temp[order(temp$`Assessed Date`, decreasing = TRUE),] # order by nursing assessment date in reverse
temp <- distinct(temp, UID, .keep_all = TRUE) # keep only latest instance (i.e. remove any repeats)

temp$ex_smoker <- ifelse(temp$smoke %in% "No(ex-smoker)", 1, 0)
temp$smoke <- ifelse(temp$smoke %in% "No(ex-smoker)", 0, temp$smoke)
temp$smoke <- as.numeric(temp$smoke)
temp$ex_drinker <- ifelse(temp$drink %in% "No (ex-drinker)", 1, 0)
temp$drink <- ifelse(temp$drink %in% "No (ex-drinker)", 0, temp$drink)
temp$drink <- as.numeric(temp$drink)
temp$either_incontinent <- ifelse(temp$bm_continent == 0 | temp$urinary_continent == 0, 1, 0)
names(temp)[names(temp)=="UID"] <- "member_id"
temp <- merge(temp, wbs[wbs$Round == 1, c("member_id", "risk_level", "Carer", "Hypertension", "Diabetes", 
                                          "Cholesterol", "Heart", "Stroke", "Copd", "Renal", "living_status", 
                                          "AMIC", "Incontinence", "SAR5")], # extract item matched by member ID
              by=c("member_id"), all.x = TRUE)
temp$live_alone_wbs <- ifelse(temp$living_status %in% 1, 1, 
                              ifelse(temp$living_status %in% NA, NA, 0))
temp$live_family_wbs <- ifelse(temp$living_status %in% c(2:4), 1,
                               ifelse(temp$living_status %in% NA, NA, 0))
temp$live_others_wbs <- ifelse(temp$living_status %in% 5, 1, 
                               ifelse(temp$living_status %in% NA, NA, 0))
temp$memory_impaired_wbs <- ifelse(temp$AMIC %in% c(1:2), 1, 
                                   ifelse(temp$AMIC %in% NA, NA, 0))
temp$memory_impaired_worry_wbs <- ifelse(temp$AMIC %in% 2, 1,
                                         ifelse(temp$AMIC %in% NA, NA, 0))
temp$incontinent_wbs <- ifelse(temp$Incontinence %in% c(1:2), 1,
                               ifelse(temp$Incontinence %in% NA, NA, 0))
temp$incontinent_occasional_wbs <- ifelse(temp$Incontinence %in% 2, 1,
                                          ifelse(temp$Incontinence %in% NA, NA, 0))

temp$fall_hist_wbs <- ifelse(temp$SAR5 %in% 0, 0,
                             ifelse(temp$Incontinence %in% NA, NA, 1)) 

# # consistency/accuracy between nursing assessment and WBS data
# temp$self_care_inverse <- ((temp$self_care)-1)*-1
# vars1 <- c("ht", "dm", "cva", "ihd", "hyperlipid", "renal", "smoke",
#            "live_alone", "live_family", "live_maid", "self_care_inverse", "fall_hist", "either_incontinent", "either_incontinent")
# vars2 <- c("Hypertension", "Diabetes", "Stroke", "Heart", "Cholesterol", "Renal", "Copd",
#            "live_alone_wbs", "live_family_wbs", "live_others_wbs", "Carer", "fall_hist_wbs", "incontinent_wbs", "incontinent_occasional_wbs")
# 
# table <- data.frame(matrix(ncol = 3,  nrow = 0))
# for (i in 1:length(vars1)){
#   var1 <- vars1[i]
#   var2 <- vars2[i]
#   table[i, 1]  <- var1
#   table[i, 2]  <- var2
#   table[i, 3]  <-  nrow(temp[(temp[[var1]] == 1 & temp[[var2]] == 1) |
#                                (temp[[var1]] == 0 & temp[[var2]] == 0),])/nrow(temp[temp[[var1]] %!in% NA & temp[[var2]] %!in% NA,])
# }
# 
# nrow(temp[(temp[[var1]] == 1 & temp[[var2]] == 1) |
#             (temp[[var1]] == 0 & temp[[var2]] == 0),])/nrow(temp[temp[[var1]] %!in% NA & temp[[var2]] %!in% NA,])

assess <- temp
rm(temp)

# EMS data ----
ems <- readxl::read_xlsx("All_event_attendence_2021-10-04.xlsx", sheet  = "Sheet 1"
                         , col_names = TRUE, guess_max = 3000)
names(ems)[names(ems)=="uid"] <- "member_id"
# temp <- readxl::read_xlsx("All_event_2021-10-04.xlsx", sheet  = "Sheet 1"
#                           , col_names = TRUE, guess_max = 3000)

ems$Total <-  ifelse(ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA , 1, 0)
ems$Mental <- ifelse(ems$Mental %in% "Yes" & (ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA), 1, 0)
ems$Exercise <- ifelse(ems$Exercise %in% "Yes" & (ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA), 1, 0)
ems$Cognitive <- ifelse(ems$Cognitive %in% "Yes" & (ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA), 1, 0)
ems$Nutritional <- ifelse(ems$Nutritional %in% "Yes" & (ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA), 1, 0)
ems$Social <- ifelse(ems$Social %in% "Yes" & (ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA), 1, 0)
ems$Others <- ifelse(ems$Others %in% "Yes" & (ems$attendance_in_time %!in% NA | ems$attendance_out_time %!in% NA), 1, 0)

ems_wide <- ems %>% 
  group_by(member_id) %>% 
  summarise(Total = sum(Total),  # sum events by person (uid)
            Mental = sum(Mental),
            Exercise = sum(Exercise),
            Cognitive = sum(Cognitive),
            Nutritional = sum(Nutritional),
            Social = sum(Social),
            Others = sum(Others),)

df <- merge(df, ems_wide[, c("member_id", "Total", "Mental", "Exercise", "Cognitive", "Nutritional", "Social", "Others")], # extract item matched by member ID
            by=c("member_id"), all.x = TRUE)

df$Total <-  ifelse(df$Total %in% NA, 0, df$Total)
df$Mental <-  ifelse(df$Mental %in% NA, 0, df$Mental)
df$Exercise <-  ifelse(df$Exercise %in% NA, 0, df$Exercise)
df$Cognitive <-  ifelse(df$Cognitive %in% NA, 0, df$Cognitive)
df$Nutritional <-  ifelse(df$Nutritional %in% NA, 0, df$Nutritional)
df$Social <-  ifelse(df$Social %in% NA, 0, df$Social)
df$Others <-  ifelse(df$Others %in% NA, 0, df$Others)

df$Total2 <- df$Mental + df$Exercise + df$Cognitive + df$Nutritional + df$Social + df$Others
df$Mental_pct <- df$Mental/df$Total2
df$Exercise_pct <- df$Exercise/df$Total2
df$Cognitive_pct <- df$Cognitive/df$Total2
df$Nutritional_pct <- df$Nutritional/df$Total2
df$Social_pct <- df$Social/df$Total2
df$Others_pct <- df$Others/df$Total2
df$Total2 <- NULL

# plot activity/nursing support by duration of participation ----
get_plot <- function(df, x, y, xlab = NULL, ylab = NULL, legendtitle = NULL, jitter_w = 0, jitter_h = 0, 
                     yintercept = NULL, xintercept = NULL, text_size = 5.5, group = NULL, legend = TRUE, scatter = FALSE, method = "loess", colour =  "darkgray"){
  plot <- ggplot(df, aes_string(x=x, y=y, fill = group, colour = group) ) +
    xlab(xlab) + ylab(ylab) + guides(fill=guide_legend(title=legendtitle), color=guide_legend(title=legendtitle))  +
    theme(text = element_text(size=rel(text_size)),
          legend.text = element_text(size=rel(text_size)),
          strip.text.x = element_text(size=rel(text_size*0.85)),
          strip.text.y = element_text(size=rel(text_size*0.85)))
  if (!is.numeric(df[[x]])) {plot <- plot + scale_x_date(date_breaks = "3 month", date_labels =  "%b %Y")}
  if (scatter) {plot <- plot + geom_jitter(width = jitter_w, height = jitter_h)}
  if (!legend) {plot <- plot + theme(legend.position="none")}
  if (is.null(group)) {plot <- plot + geom_smooth(method=method, formula = y ~ x, se=TRUE, na.rm=TRUE, color = colour, fill = colour)} 
  else {plot <- plot + geom_smooth(method=method, formula = y ~ x, se=TRUE, na.rm=TRUE)}
  return(plot)
}

# nursing support sessions
red <- "#db6d63"
green <- "#4f9e78"
blue <- "#98a2da"
yellow <- "#ffe873"

temp <- df
temp <- temp[as.Date(temp$ehealth_eval_timestamp) <= as.Date('2021-08-31'),]

dfwide <- reshape(data=df, idvar= c("member_id"),
                  timevar = "time",
                  direction="wide")
dfwide$duration <- round(as.numeric(as.Date('2021-08-31') - as.Date(dfwide$ehealth_eval_timestamp.0)), digits = 0)

temp <- merge(temp, dfwide[c("member_id", "duration", "ehealth_eval_timestamp.0")], 
              by=c("member_id"), all.x = TRUE)

temp <- temp %>% add_count(member_id, name = "n") %>% filter((n == 1 & time == 0)| (n == 2 & time == 1) | (n >= 2 & time == 2))
temp$ehealth_eval_timestamp.0 <- as.Date(temp$ehealth_eval_timestamp.0)

# nursing support target

# remove withdrawals
setwd(sprintf("~%s/ehealth/wbs", setpath))
withdrawal <- readxl::read_xlsx("data_WBS_enrol_status_2021-11-27_Overall.xlsx", sheet  = "Sheet 1"
                                        , col_names = TRUE, guess_max = 20000)
setwd(sprintf("~%s/ehealth", setpath))
withdrawal <- withdrawal %>% filter(Status == "Withdrew")
# withdrawal <- withdrawal[as.Date(withdrawal$Quit_Date) <= as.Date('2021-08-31'),]
temp <- temp[temp$member_id %!in% withdrawal$Uid,]

temp$nursing_target <- ifelse(temp$duration <= 90, floor(temp$duration/14),  NA) # 6 times weeks 1-12 (once every 2 weeks)
temp$nursing_target <- ifelse(temp$duration > 90, 6+floor((temp$duration-90)/30),  temp$nursing_target) # once every month 

temp$target_met <- ifelse(temp$f2f_telemode_num >= (temp$nursing_target-0), 1, 0)

temp %>%   group_by(gender, age_group) %>%
  summarise_at(vars(target_met), list(~mean(., na.rm=TRUE), sd = sd(., na.rm=TRUE))) %>% clipr::write_clip()

temp1 <- temp[, c("duration", "f2f_num", "age", "ehealth_eval_timestamp")]
temp1$type <- "Face-to-face"
names(temp1)[names(temp1) == "f2f_num"] <- "nursing_support"
temp2 <- temp[, c("duration", "telemode_num", "age", "ehealth_eval_timestamp")]
temp2$type <- "Telemode"
names(temp2)[names(temp2) == "telemode_num"] <- "nursing_support"
temp3 <- temp[, c("duration", "f2f_telemode_num", "age", "ehealth_eval_timestamp")]
temp3$type <- "Total"
names(temp3)[names(temp3) == "f2f_telemode_num"] <- "nursing_support"



temp4 <- data.frame(duration=seq(0,max(temp$duration)))
temp4$nursing_target <- ifelse(temp4$duration <= 90, floor(temp4$duration/14),  NA) # 6 times weeks 1-12 (once every 2 weeks)
temp4$nursing_target <- ifelse(temp4$duration > 90, 6+floor((temp4$duration-90)/30),  temp4$nursing_target) # once every month

temp4$label <- ifelse(temp4$duration==max(temp4$duration),"Target (total)", NA)
names(temp4)[names(temp4) == "nursing_target"] <- "nursing_support"

temp1 <- rbind(temp1, temp2, temp3)
rm(temp2, temp3)
temp1$type <- factor(temp1$type, levels = c("Total", "Telemode","Face-to-face"))
temp1 <- temp1[as.Date(temp1$ehealth_eval_timestamp) <= as.Date('2021-08-31'),]

(get_plot(temp1, x = "duration", xlab = "Duration in days (since baseline assessment)", 
           y = "nursing_support", ylab = "", group = "type", legendtitle = "Nursing support type") +  ggtitle('Average number of nursing support sessions (cumulative as of 31 Aug 2021)')) &
    geom_line(data=temp4, aes(x=duration, y=nursing_support, fill=NULL, color = NULL), method = "loess",  linetype="dashed" ) &
    geom_label(data=temp4, aes(label = label, fill=NULL, color = NULL), nudge_x = -10, nudge_y = 0.8, size = 7) &
 theme(plot.title = element_text(size = rel(6)))  & coord_cartesian(xlim = c(90, NA)) 


# nursing protocol types used
temp <- df
temp <- temp[as.Date(temp$ehealth_eval_timestamp) <= as.Date('2021-10-31'),]
temp <- temp %>% add_count(member_id, name = "n") %>% filter((n == 1 & time == 0)| (n == 2 & time == 1) | (n >= 2 & time == 2))


get_results_by <- function(data, grp, var){
 table <- 
   data  %>% 
   group_by(!!rlang::sym(grp)) %>% summarise_at(vars(!!rlang::sym(var)), list(~n(), ~mean(.>0, na.rm=TRUE))) %>% 
   data.frame(., var = var, wbs_var = grp) 
 names(table)[1] <- "wbs_var"
 names(table)[2] <- "n"
 names(table)[3] <- "proportion"
 table$wbs_var <- ifelse(table$wbs_var %in% NA, "NA", table$wbs_var)
 return(table)
}

table <- data.frame()
table <- rbind(table, get_results_by(temp, "Hypertension", "protocol_1.1"))
table <- rbind(table, get_results_by(temp, "Diabetes", "protocol_1.2"))
table <- rbind(table, get_results_by(temp, "Cholesterol", "protocol_1.3"))
table <- rbind(table, get_results_by(temp, "Heart", "protocol_1.14"))
table <- rbind(table, get_results_by(temp, "FS_total", "protocol_1.8"))
table <- rbind(table, get_results_by(temp, "SAR_total", "protocol_1.9"))
table <- rbind(table, get_results_by(temp, "SAR5", "protocol_1.15"))
table <- rbind(table, get_results_by(temp, "AMIC", "protocol_2.2"))
table <- rbind(table, get_results_by(temp, "amic_sum", "protocol_2.2"))
table <- rbind(table, get_results_by(temp, "Happiness", "protocol_2.1"))
table <- rbind(table, get_results_by(temp, "Meaning_of_life", "protocol_2.1"))
table <- rbind(table, get_results_by(temp, "Drug_use", "protocol_1.6"))
table <- rbind(table, get_results_by(temp, "eq5d_pain_discomfort", "protocol_1.7"))

Sys.setlocale(locale =  "cht") # set locale to traditional Chinese
temp %>% group_by(eq5d_pain_discomfort) %>%
  summarise_at(vars(protocol_1.7), list(~n(), ~mean(.>0, na.rm=TRUE))) 
Sys.setlocale(locale =  "eng") # set locale to traditional Chinese


(get_plot(temp[temp$risk_score >=14, ], x = "risk_score", xlab = "WBS risk score", y = "protocol_total_b", ylab = "", group = "gender", legendtitle = "Gender") +
    ggtitle("Number of times different protocols applied per individual  (as of 31 Oct 2021)") & theme(plot.title = element_text(size = rel(5.5)))) 

(get_plot(temp, x = "age", xlab = "Age", y = "protocol_total_b", ylab = "", group = "gender", legendtitle = "Gender") +
    ggtitle("Number of times different protocols applied per individual  (as of 31 Oct 2021)") & theme(plot.title = element_text(size = rel(5.5)))) 


(get_plot(temp, x = "age", xlab = "Age", 
          y = "protocol_1.1", ylab = "Hypertension", legend = FALSE) + ggtitle('')) +
  (get_plot(temp, x = "age", xlab = "Age", 
            y = "protocol_1.2", ylab = "Diabetes", legend = FALSE) + ggtitle(''))  +
  (get_plot(temp, x = "age", xlab = "Age", 
            y = "protocol_1.7", ylab = "Pain", legend = FALSE) +  ggtitle('')) +
  (get_plot(temp, x = "age", xlab = "Age", 
            y = "protocol_1.8", ylab = "Frailty", legend = FALSE) + ggtitle(''))  & 
  plot_annotation(
    title = 'Number of times nursing protocols applied per individual (as of 31 Oct 2021)'
  ) & theme(plot.title = element_text(size = rel(3))) & coord_cartesian(ylim = c(0, 6)) 

# EMS activities
get_plot(temp, x = "duration", xlab = "Duration in days (since baseline assessment)", y = "Total", ylab = "Average number of attended activities in total", group = "gender", legendtitle = "Gender") +
  get_plot(temp, x = "duration", xlab = "Duration in days (since baseline assessment)", y = "Total", ylab = "Average number of attended activities in total", group = "age_group", legendtitle = "Age group")


get_sum <- function(data, time = "attendance_in_time", sum_groups = c("Total", "Mental", "Exercise", "Cognitive", "Nutritional", "Social", "Others"), group = NULL){
  data[[time]] <- as.Date(data[[time]])
  if (is.null(group)){
    data <- data %>%
      group_by(
        member_id, 
        !!sym(time),) %>% 
      summarise_at(sum_groups, .funs = sum)
  } else {
    data <- data %>%
      group_by(
        member_id, 
        !!sym(time), !!rlang::sym(group)) %>% 
      summarise_at(sum_groups, .funs = sum)
  }
}

wbs$age_group <- recode_age(wbs$Age, age_labels = c("50-69", "70-79", "80+"))
wbs$age_group <- relevel(as.factor(wbs$age_group), ref = "50-69")

ems$attendance_in_time_week <- as.Date(cut(as.Date(ems$attendance_in_time), "month"))

ems_temp <- merge(ems, wbs %>% add_count(member_id, name = "n") %>% filter((n == 1 & Round == 1)| (n == 2 & Round == 2)), # restrict to lastest WBS round
                  by=c("member_id"), all.x = TRUE)

# add back those with zero activity 
new_dates <- seq.Date(from = as.Date("2020-06-01"), to =  as.Date("2021-10-02"), by = 1)
new_dates <- as.Date(cut(as.Date(new_dates), "month")) %>% unique # turn daily dates to weekly
wbs_temp <- wbs %>% add_count(member_id, name = "n") %>% filter((n == 1 & Round == 1)| (n == 2 & Round == 2)) # %>% filter(risk_level %in% 3) 
highrisk_ids <- unique(wbs_temp$member_id)

pb <- progress::progress_bar$new(total = length(new_dates)*length(highrisk_ids)) # show progress bar of computation
temp <- data.frame(attendance_in_time_week = NA, member_id = NA, gender = NA, age_group = NA)
temp[1:(length(new_dates)*length(highrisk_ids)),] <- NA
count1 <- 1
count2 <- length(highrisk_ids)

for (i in 1:length(new_dates)){
    temp$attendance_in_time_week[count1:count2] <- as.character(new_dates[i])
    temp$member_id[count1:count2] <- highrisk_ids
    count1 <- count2 + 1
    count2 <- count2 + length(highrisk_ids)
    pb$tick()
}
temp$attendance_in_time_week <- as.Date(temp$attendance_in_time_week, )


temp_ems <- get_sum(ems_temp,  time = "attendance_in_time_week", group = "risk_level")
temp_ems <- merge(temp, temp_ems, # restrict to lastest WBS round
                  by=c("attendance_in_time_week", "member_id"), all.x = TRUE)
temp_ems$Total <- ifelse(temp_ems$Total %in% NA, 0, temp_ems$Total)
temp_ems$Mental <- ifelse(temp_ems$Mental %in% NA, 0, temp_ems$Mental)
temp_ems$Exercise <- ifelse(temp_ems$Exercise %in% NA, 0, temp_ems$Exercise)
temp_ems$Cognitive <- ifelse(temp_ems$Cognitive %in% NA, 0, temp_ems$Cognitive)
temp_ems$Nutritional <- ifelse(temp_ems$Nutritional %in% NA, 0, temp_ems$Nutritional)
temp_ems$Social <- ifelse(temp_ems$Social %in% NA, 0, temp_ems$Social)
temp_ems$Others <- ifelse(temp_ems$Others %in% NA, 0, temp_ems$Others)

temp_ems <- merge(temp_ems, wbs_temp[, c("member_id", "gender", "age_group", "risk_level")], # restrict to latest WBS round
                  by=c("member_id"), all.x = TRUE)

# ems_temp <- merge(wbs %>% add_count(member_id, name = "n") %>% filter((n == 1 & Round == 1)| (n == 2 & Round == 2)),
#                   ems %>%
#                     group_by(
#                       member_id) %>%
#                     summarise_at("Total", .funs = sum), # restrict to lastest WBS round
#                   by=c("member_id"), all.x = TRUE)

# risk level
temp_ems <- get_sum(ems_temp,  time = "attendance_in_time_week", group = "risk_level")
temp_ems$risk_level <- to_character(temp_ems$risk_level)
temp_ems$risk_level <- factor(temp_ems$risk_level, levels = c("H", "M", "L"))
get_plot(temp_ems[temp_ems$risk_level %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance", y = "Total", ylab = "", 
         group = "risk_level", legendtitle = "Risk level", method="locfit", scatter = TRUE, jitter_w = 10, jitter_h = 0) + ggtitle('Average number of activities per month') & 
  theme(plot.title = element_text(size = rel(5)))


ggplot(temp_ems[temp_ems$risk_level %!in% NA,], aes(x = attendance_in_time_week, y = Total)) +
  # Set the quantiles argument to 0.5 so that only the median is shown.
  stat_quantile(alpha = 0.6, size = 2, quantiles = 0.75) 


# high-risk only
ems_temp <- ems_temp[ems_temp$risk_level %in% 3,] 

# gender
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "gender") %>% ungroup
get_plot(temp_ems[temp_ems$gender %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance", 
         y = "Total", ylab = "", group = "gender", "Gender", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(plot.title = element_text(size = rel(5))) & coord_cartesian(ylim = c(NA, NA)) 

# age group
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "age_group")
get_plot(temp_ems[temp_ems$age_group %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "age_group", legendtitle = "Age group", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(plot.title = element_text(size = rel(5)))

# digital vs non-digital centres
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "Digital")
temp_ems$Digital <- ifelse(temp_ems$Digital == 1, "Yes", "No")
get_plot(temp_ems[temp_ems$Digital %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "Digital", legendtitle = "Digital centres", method = "locfit", scatter =T) + ggtitle('Average number of activities per month') & 
  theme(plot.title = element_text(size = rel(5)))

# activity types
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = NULL)
(get_plot(temp_ems, x = "attendance_in_time_week", xlab = NULL, 
          y = "Mental", ylab = "Mental", method = "locfit", legend = FALSE) +  ggtitle('Average number of activities per month')) +
  (get_plot(temp_ems, x = "attendance_in_time_week", xlab = "", 
            y = "Exercise", ylab = "Exercise", method = "locfit", legend = FALSE) + ggtitle(''))  +
  (get_plot(temp_ems, x = "attendance_in_time_week", xlab = NULL, 
            y = "Cognitive", ylab = "Cognitive", method = "locfit", legend = FALSE) +  ggtitle('')) +
  (get_plot(temp_ems, x = "attendance_in_time_week", xlab = "", 
            y = "Nutritional", ylab = "Nutritional", method = "locfit", legend = FALSE) + ggtitle('')) +
  (get_plot(temp_ems, x = "attendance_in_time_week", xlab = "Date of attendance", 
            y = "Social", ylab = "Social", method = "locfit", legend = FALSE) + ggtitle('')) +
  (get_plot(temp_ems, x = "attendance_in_time_week", xlab = "", 
            y = "Others", ylab = "Others", method = "locfit", legend = FALSE) + ggtitle('')) & 
  theme(plot.title = element_text(size = rel(4)),
        axis.text.x = element_text(angle=20, vjust = 1, hjust = 1)) & coord_cartesian(ylim = c(0, 0.06)) 

# housing type
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "housing_type")
temp_ems$housing_type <- to_character(temp_ems$housing_type)
temp_ems$housing_type <- ifelse(temp_ems$housing_type == "Homes for the Aged", "Others (e.g. Temporary Housing)", temp_ems$housing_type)
temp_ems$housing_type <- factor(temp_ems$housing_type, levels = c("Public Housing", "Subsidized Housing",
                                                                  "Private Housing", "Others (e.g. Temporary Housing)"))
get_plot(temp_ems[temp_ems$housing_type %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "housing_type", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(legend.position="bottom", plot.title = element_text(size = rel(5))) & 
  guides(color = guide_legend(nrow = 2, title = "Housing type"),
         fill = guide_legend(nrow = 2, title = "Housing type"))

# living status
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "living_status")
temp_ems$living_status <- to_character(temp_ems$living_status)
get_plot(temp_ems[temp_ems$living_status %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "living_status", legendtitle = "Living status", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(legend.position="bottom", plot.title = element_text(size = rel(5))) & 
  guides(color = guide_legend(nrow = 3, title = "Living status"),
         fill = guide_legend(nrow = 3, title = "Living status"))

# education
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "educ")
temp_ems$educ <- to_character(temp_ems$educ)
get_plot(temp_ems[temp_ems$educ %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "educ", legendtitle = "Education", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(legend.position="bottom", plot.title = element_text(size = rel(5))) & 
  guides(color = guide_legend(nrow = 2, title = "Education"),
         fill = guide_legend(nrow = 2, title = "Education"))

# marital status
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "marital")
temp_ems$marital <- to_character(temp_ems$marital)
get_plot(temp_ems[temp_ems$marital %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "marital", legendtitle = "Marital status", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(legend.position="bottom", plot.title = element_text(size = rel(5))) & 
  guides(color = guide_legend(nrow = 2, title = "Marital status"),
         fill = guide_legend(nrow = 2, title = "Marital status"))

# CSSA income
temp_ems <- get_sum(ems_temp, time = "attendance_in_time_week", group = "Income_cssa")
temp_ems$Income_cssa <- ifelse(temp_ems$Income_cssa == 1, "Yes", "No")
get_plot(temp_ems[temp_ems$Income_cssa %!in% NA,], x = "attendance_in_time_week", xlab = "Date of attendance ", y = "Total", ylab = "", 
         group = "Income_cssa", legendtitle = "CSSA", method = "locfit") + ggtitle('Average number of activities per month') & 
  theme(plot.title = element_text(size = rel(5)))


# pre-post results with adjustments ----
temp <- df %>% filter(time %in% c(0, 1))  %>% add_count(member_id, name = "n") %>% filter(n == 2)
temp <- temp[as.Date(temp$ehealth_eval_timestamp) <= as.Date('2021-10-04'),]

tempwide <- reshape(data=temp, idvar= c("member_id"),
                    timevar = "time",
                    direction="wide")
tempwide$t1t0_duration <- as.numeric(round( as.Date(tempwide$ehealth_eval_timestamp.1) - as.Date(tempwide$ehealth_eval_timestamp.0), digits = 0))

temp <- merge(temp, tempwide[c("member_id", "t1t0_duration")], 
              by=c("member_id"), all.x = TRUE)

temp$month <- lubridate::month(temp$ehealth_eval_timestamp)

table <- combine_tables(NULL,
                        exponentiate = FALSE,
                        # show_CI = 0.83,
                        lmer(amic~ 1+gender+age_group+time*Others+(1| member_id) , REML = TRUE, data = temp),
                        # glmer(amic~ 1+gender+age_group+time*Others+ (1| member_id), family = binomial, data = temp),
                        lmer(amic_sum~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        # clmm(as.factor(amic_sum) ~ covid+int + (1 | member_id), data = df_matched, link="logit", Hess=TRUE, na.action=na.omit, nAGQ=5),
                        lmer(self_efficacy~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_1~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_2~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_3~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_4~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_5~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(eq5d~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_mobility~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_self_care~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_usual_activity~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_pain_discomfort~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_anxiety_depression~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_health~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(satisfaction_1~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(satisfaction_2~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(pase_c~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_1~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_1_2~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_11~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        # glmer(pase_c_11~ 1+gender+age_group+time*Others+ (1| member_id), family = binomial, data = temp),
                        lmer(pase_c_11_1~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_12_1~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        # glmer(pase_c_12_1~ 1+gender+age_group+time*Others+ (1| member_id), family = binomial, data = temp),
                        lmer(pase_c_12~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(matrix_diet_dh3~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(matrix_diet_dh4~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(matrix_diet_dh7~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(matrix_diet_dh8~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp1~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp3~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp4~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp5~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_sum~ 1+gender+age_group+time*Others+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(use_health_service_8~ 1+gender+age_group+time*Others+(1| member_id) , REML = TRUE, data = temp)
)
table %>% clipr::write_clip()

table <- combine_tables(NULL,
                        exponentiate = FALSE,
                        # show_CI = 0.83,
                        lmer(amic~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+(1| member_id) , REML = TRUE, data = temp),
                        # glmer(amic~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id), family = binomial, data = temp),
                        lmer(amic_sum~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        # clmm(as.factor(amic_sum) ~ covid+int + (1 | member_id), data = df_matched, link="logit", Hess=TRUE, na.action=na.omit, nAGQ=5),
                        lmer(self_efficacy~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_2~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_3~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_4~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(self_efficacy_5~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(eq5d~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_mobility~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_self_care~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_usual_activity~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_pain_discomfort~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_anxiety_depression~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(eq5d_health~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(satisfaction_1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(satisfaction_2~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(pase_c~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_1_2~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_11~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        # glmer(pase_c_11~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id), family = binomial, data = temp),
                        lmer(pase_c_11_1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(pase_c_12_1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        # glmer(pase_c_12_1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id), family = binomial, data = temp),
                        lmer(pase_c_12~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        
                        lmer(matrix_diet_dh3~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(matrix_diet_dh4~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(matrix_diet_dh7~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(matrix_diet_dh8~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp1~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp3~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp4~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_dp5~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(diet_sum~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+ (1| member_id) , REML = TRUE, data = temp),
                        lmer(use_health_service_8~ 1+gender+age_group+risk_score+time*protocol_1.1+time*protocol_1.2+(1| member_id) , REML = TRUE, data = temp)
)
table %>% clipr::write_clip()


# experimental pre-post results by duration/survey date ----
num <- 4
var <- var_names[[num,1]]
pre <- 0
post <- 1

temp <- df %>% filter(time %in% c(pre, post))  %>% add_count(member_id, name = "n") %>% filter(n == 2)
tempwide <- reshape(data=temp, idvar= c("member_id"),
                    timevar = "time",
                    direction="wide")

tempwide$var_d <- tempwide[[paste0(var, ".", post)]] - tempwide[[paste0(var, ".", pre)]]
names(tempwide)[names(tempwide)=="var_d"] <- paste0(var, "_d")
tempwide$t1t0_duration <- as.numeric(round( as.Date(tempwide[[paste0("ehealth_eval_timestamp", ".", post)]]) - as.Date(tempwide[[paste0("ehealth_eval_timestamp", ".", pre)]]), digits = 0))


tempwide[[paste0("ehealth_eval_timestamp", ".", pre)]] <-  as.Date(tempwide[[paste0("ehealth_eval_timestamp", ".", pre)]])
tempwide[[paste0("ehealth_eval_timestamp", ".", post)]] <-  as.Date(tempwide[[paste0("ehealth_eval_timestamp", ".", post)]])


get_plot(tempwide, x = paste0("ehealth_eval_timestamp", ".", pre), y=paste0(var, ".", pre), ylab = var_names[[num,2]], group = NULL) /
  get_plot(tempwide, x = paste0("ehealth_eval_timestamp", ".", post),  y=paste0(var, ".", post), ylab = var_names[[num,2]], group = NULL) /
  get_plot(tempwide, x = "t1t0_duration", y=paste0(var, "_d"), ylab = var_names[[num,2]], group = NULL) 
