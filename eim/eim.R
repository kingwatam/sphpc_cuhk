rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/CUHK"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)

setwd(sprintf("~%s/eim", setpath))
# df <- xlsx::read.xlsx2("Systolic BP RAW data.xlsx", sheetName  = "Sheet1"
#                        , header = TRUE)
# df <- convert2NA(df, "")
# 
# df$age_group <- recode_age(df$Age, age_labels = NULL, second_group = 25, interval = 5, last_group = 95)

df <- xlsx::read.xlsx2("Systolic BP RAW data + RRs.xlsx", sheetName  = "data"
                       , header = TRUE)
df <- df[1:202,]
df <- convert2NA(df, "")


df[,c(3:5, 7:31)] <- sapply(df[,c(3:5, 7:31)], as.numeric)
hist(df$Change_HHD, 100)

library(ggplot2)

plot_hist <- function(var, xtitle = ""){
  plot <- ggplot(data.frame(var), aes(var)) +    # Draw histogram
    geom_histogram(bins = 50, fill = "#666666") +                                    # Draw histogram
    geom_vline(xintercept = mean(var),        # Add line for mean
               col = "#444444",
               lwd = 1, linetype="dashed") +
    geom_vline(xintercept = median(var),        # Add line for median
               col = "#888888",
               lwd = 1, linetype="dashed") +
    # annotate("text",                        # Add text for mean
    #          x = 1,
    #          y = 8,
    #          label = paste("Proportion of those with reduced risk  =",  paste0(round((sum(var < 0, na.rm = TRUE)/sum(var %!in% NA, na.rm = TRUE))*100, 1), "%")),
    #          col = "dark green",
    #          size = 6) +
    annotate("text",                        # Add text for mean
             x = 1,
             y = 10,
             label = paste("Mean =",  paste0(round(mean(var)*100, 1), "%")),
             col = "#444444",
             size = 6) +
    annotate("text",                        # Add text for median
             x = 1,
             y = 12,
             label = paste("Median =", paste0(round(median(var)*100, 1), "%")),
             col = "#888888",
             size = 6) +
    xlab(paste("% Change in risk level of", xtitle)) + ylab("Frequency")
  return(plot)
}

plot_hist(na.omit(df$Change_IHD), xtitle = "ischaemic heart disease")
plot_hist(na.omit(df$Change_IS), xtitle = "ischaemic stroke")
plot_hist(na.omit(df$Change_IH), xtitle = "intracerebral hemorrhage")
plot_hist(na.omit(df$Change_SH), xtitle = "subarachnoid hemorrhage")
plot_hist(na.omit(df$Change_HHD), xtitle = "hypertensive heart disease")


df$age_group2 <- recode_age(df$Age, age_labels = NULL, second_group = 55, interval = 5, last_group = 70)
vars <- c("Change_IHD", "Change_IS", "Change_IH", "Change_SH", "Change_HHD")

tableone::CreateTableOne(data = df,
                         vars = vars 
                         # , strata = c("age_group2")
                         ) %>%
  print(showAllLevels = TRUE, nonnormal = vars, digits = 4) %>% clipr::write_clip()