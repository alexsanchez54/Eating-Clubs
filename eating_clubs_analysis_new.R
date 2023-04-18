# Clear workspace
rm(list = ls())

# Load relevant libraries
library(tidyverse)
library(lubridate)
library(estimatr)

# To-do:
# plot Sexual Misconduct
# include Spring break indicator in the regression

# Load data
alldata <- read.csv("eclub_all_AD.csv")

#Dean's Date variable
deans_date_dates <- as.Date(c(
  "2008-05-13", "2009-05-12", "2010-05-11", "2011-05-10", "2012-05-15",
  "2013-05-14", "2014-05-13", "2015-05-12", "2016-05-10", "2017-05-16", 
  "2018-05-15", "2019-05-14", "2020-05-12"))

#Spring break variable
spring_break_dates <- c(
  seq(as.Date("2008-03-15"), as.Date("2008-03-23"), by="day"),
  seq(as.Date("2009-03-14"), as.Date("2009-03-22"), by="day"),
  seq(as.Date("2010-03-13"), as.Date("2010-03-21"), by="day"),
  seq(as.Date("2011-03-12"), as.Date("2011-03-20"), by="day"),
  seq(as.Date("2012-03-17"), as.Date("2012-03-25"), by="day"),
  seq(as.Date("2013-03-16"), as.Date("2013-03-24"), by="day"),
  seq(as.Date("2014-03-15"), as.Date("2014-03-23"), by="day"),
  seq(as.Date("2015-03-14"), as.Date("2015-03-22"), by="day"),
  seq(as.Date("2016-03-12"), as.Date("2016-03-20"), by="day"),
  seq(as.Date("2017-03-18"), as.Date("2017-03-26"), by="day"),
  seq(as.Date("2018-03-17"), as.Date("2018-03-25"), by="day"),
  seq(as.Date("2019-03-16"), as.Date("2019-03-24"), by="day"),
  seq(as.Date("2020-03-14"), as.Date("2020-03-22"), by="day"))

#More variables
alldata_clean <- alldata |> 
  mutate(null = if_else(sex == 0 & harm == 0 & party == 0, 1, 0), #control
         infr_date = mdy(infr_date), #standardize
         sexual_misconduct = ifelse(primary_cat == "Sexual Misconduct", 1, 0)) |>  #Title IX cases (and another potential control if 0)
  arrange(infr_date) |> 
  mutate_at(vars(harm, sex, party, null), factor) |> #factorize
  select(- year) #remove column with redundant/incorrect values

# Save cleaned data
# write.csv(alldata_clean, "alldata_clean.csv")

#### Pre-process data
alldata_summary <- alldata_clean |> 
  group_by(infr_date) |> 
  dplyr::summarize(
    all = n(),
    sex = sum(sex == 1),
    harm = sum(harm == 1),
    sex_harm = sum(sex == 1 & harm == 1),
    party = sum(party == 1),
    null = sum(null == 1),
    sexual_misconduct = sum(sexual_misconduct == 1)
  )
  
#make a dataframe with the date range 2007-2022
date_range <- c(seq(as.Date("2007-01-01"), as.Date("2020-12-31"), by = "days"))
date_df <- data.frame(infr_date = date_range)

#merge datasets
alldata_summary <- left_join(date_df, alldata_summary, by = "infr_date")
alldata_summary[is.na(alldata_summary)] <- 0
  
alldata_summary <- alldata_summary |> 
  mutate(deans_date = if_else(infr_date %in% deans_date_dates, 1, 0), #Spring dean's date indicator)
         spring_break = if_else(infr_date %in% spring_break_dates, 1, 0), #Spring break indicator
         pledge = ifelse(infr_date == as.Date("2018-05-15"), 1, 0), #intervention date indicator
         semester = case_when( 
           month(infr_date) %in% c(1, 2, 3, 4, 5) ~ "Spring",
           month(infr_date) %in% c(6, 7, 8) ~ "Summer",
    TRUE ~ "Fall"))
