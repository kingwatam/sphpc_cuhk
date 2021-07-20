rm(list=ls())
graphics.off()
# par(mar=c(0,0,0,0)) # set plot margins to 0
setpath <- "/MEGAsync/Work/CUHK/"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)

setwd(sprintf("~%s/ehealth", setpath))

# import COVID data ---- 
# source: https://rpruim.github.io/ds303/S20/hw/covid-19/covid-19.html (adapted for importing CSSE data)
library(tidyverse)

base_url <- 
  paste0('https://raw.githubusercontent.com/',
         'CSSEGISandData/COVID-19/master/',
         'csse_covid_19_data/csse_covid_19_time_series/')

filename <- 
  paste0('time_series_covid19_', c('confirmed', 'deaths', 'recovered'), '_global.csv')

url <- paste0(base_url, filename)

confirmed <- url[1] %>% 
  readr::read_csv(col_types = cols(
    .default = col_double(),
    `Province/State` = col_character(),
    `Country/Region` = col_character()
  )) %>%
  rename(country_or_region = `Country/Region`,
         province_or_state = `Province/State`) 

deaths <- url[2] %>% 
  readr::read_csv(col_types = cols(
    .default = col_double(),
    `Province/State` = col_character(),
    `Country/Region` = col_character()
  )) %>%
  rename(country_or_region = `Country/Region`,
         province_or_state = `Province/State`) 

recovered <- url[3] %>%
  readr::read_csv(col_types = cols(
    .default = col_double(),
    `Province/State` = col_character(),
    `Country/Region` = col_character()
  )) %>%
  rename(country_or_region = `Country/Region`,
         province_or_state = `Province/State`)

# transform to long format, subset to HK, and merge data ----
confirmed_long_hk <-
  confirmed %>% 
  pivot_longer(
    -(1:4),                        # the first 4 columns are not part of the pivot
    names_to = "date",             # names of the remaining columns will be put into a date column
    values_to = "confirmed") %>%   # values will be put into a column called confirmed
  mutate(date = lubridate::parse_date_time(date, "%m/%d/%y!*")) %>% # convert to date objects
  filter(province_or_state =="Hong Kong")

deaths_long_hk <-
  deaths %>% 
  pivot_longer(
    -(1:4),                        # the first 4 columns are not part of the pivot
    names_to = "date",             # names of the remaining columns will be put into a date column
    values_to = "deaths") %>%   # values will be put into a column called confirmed
  mutate(date = lubridate::parse_date_time(date, "%m/%d/%y!*")) %>% # convert to date objects
  filter(province_or_state =="Hong Kong")

recovered_long_hk <-
  recovered %>% 
  pivot_longer(
    -(1:4),                        # the first 4 columns are not part of the pivot
    names_to = "date",             # names of the remaining columns will be put into a date column
    values_to = "recovered") %>%   # values will be put into a column called confirmed
  mutate(date = lubridate::parse_date_time(date, "%m/%d/%y!*")) %>% # convert to date objects
  filter(province_or_state =="Hong Kong")

df <-  merge(confirmed_long_hk, deaths_long_hk[c("province_or_state", "date", "deaths")], 
             by=c("province_or_state", "date"), all.x = TRUE)
df <-  merge(df, recovered_long_hk[c("province_or_state", "date", "recovered")], 
             by=c("province_or_state", "date"), all.x = TRUE)
rm(confirmed, deaths, recovered, confirmed_long_hk, deaths_long_hk, recovered_long_hk)

# create daily new cases ----
df$new_confirmed <- df$confirmed - lag(df$confirmed, k = 1)
df$new_confirmed[1] <- 0

df$new_deaths <- df$deaths - lag(df$deaths, k = 1)
df$new_deaths[1] <- 0

df$new_recovered <- df$recovered - lag(df$recovered, k = 1)
df$new_recovered[1] <- 0

# save data ----
setwd(sprintf("~%s/ehealth", setpath))
saveRDS(df, "covid_data_hk.rds")

# plot confirmed & deaths ----
library(ggplot2)
ggplot(df, aes(x=as.Date(date), y = new_confirmed)) + 
  geom_bar(stat='identity', color = "grey", alpha = 0.2) +
  geom_bar(aes(x=as.Date(date), y = new_deaths), stat='identity', color = "dark grey") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels="%b %Y") +
  theme(axis.text.x=element_text(angle=50, vjust = 1, hjust = 1))