# Clear workspace
rm(list = ls())

# Load relevant libraries
library(tidyverse)
library(lubridate)
library(estimatr)
library(readxl)
library(lemon)
library(plotly)
library(scales)
library(gridExtra)
library(htmltools)

# To-do:
# regress sex_harm or sexual_misconduct
# Do Jonathan-style robustness checks
# Decide if we want Randy to give us all the incident dates (check how many infractions there are compared to PADR data)

# Load data
alldata <- read.csv("eclub_all_AD.csv")

#Add variables
alldata_clean <- alldata |> 
  mutate(null = if_else(sex == 0 & harm == 0 & party == 0, 1, 0), #control
         infr_date = mdy(infr_date), #standardize
         sexual_misconduct = ifelse(primary_cat == "Sexual Misconduct", 1, 0)) |>  #Title IX cases (and another potential control if 0)
  arrange(infr_date) |> 
  mutate_at(vars(harm, sex, party, null), factor) |> #factorize
  select(- year, -X) #remove columns with redundant/incorrect values

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

alldata_summary <- alldata_summary |> 
  mutate(deans_date = if_else(infr_date %in% deans_date_dates, 1, 0), #Spring dean's date indicator)
         spring_break = if_else(infr_date %in% spring_break_dates, 1, 0), #Spring break indicator
         pledge = ifelse(infr_date == as.Date("2018-05-15"), 1, 0), #intervention date indicator
         treated_days = if_else(infr_date >= as.Date("2018-05-15"), 1, 0), #identify days after the intervention date
         year = year(infr_date),
         semester = case_when( 
           month(infr_date) %in% c(1, 2, 3, 4, 5) ~ "Spring",
           month(infr_date) %in% c(6, 7, 8) ~ "Summer",
           TRUE ~ "Fall"),
  )

# Create a days since Spring Dean's Date column, subsetted by year
for (i in 2008:2020) {
  alldata_summary$dayssince_deansdate_byyear[year(alldata_summary$infr_date) == i] <- as.Date(alldata_summary$infr_date[year(alldata_summary$infr_date) == i]) - as.Date(deans_date_dates[i - 2007])
}

#Create a variable identifying yearly pre and post Dean's Dates
alldata_summary <- alldata_summary |> 
  mutate(pre_deansdate_byyear = ifelse(dayssince_deansdate_byyear > -1, 0, 1)) |> 
  mutate_at(vars(deans_date, spring_break, pledge, semester, 
                 treated_days, pre_deansdate_byyear), as.factor) #factorize

#Subset to only Spring semesters 2008-2019
alldata_summary <- alldata_summary |> 
  filter(year(infr_date) > 2007 & year(infr_date) < 2020 & semester == "Spring") |> 
  droplevels()

# Save summarized data
write.csv(alldata_summary, "alldata_summary.csv")

#Additional subset to only April and May
alldata_summary_AprMay <- alldata_summary |> 
  filter(month(infr_date) %in% c(4,5)) |> 
  droplevels()

#Tidy data
alldata_tidy <- alldata_summary |> 
  pivot_longer(cols = c(all, sex, harm, sex_harm, party, null, sexual_misconduct),
               names_to = "infr_type",
               values_to = "total")

# Save tidy data
write.csv(alldata_tidy, "alldata_tidy.csv")

### Analyses ###

#regression for all of Spring semester
summary(lm(all ~ deans_date + pledge + treated_days + pre_deansdate_byyear + spring_break, 
           data = alldata_summary))

#regression for only April and May
summary(lm(all ~ deans_date + pledge + treated_days + pre_deansdate_byyear, 
           data = alldata_summary_AprMay))