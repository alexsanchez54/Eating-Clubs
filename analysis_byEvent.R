rm(list = ls())

#loading relevant libraries
library(tidyverse)
library(lubridate)
library(plotly)
library(plyr)
library(lemon)
library(ggforce)
library(tufte)
library(writexl)
library(estimatr)

#load data
alldata <- read.csv("eclub_all_AD.csv")

#create a null variable
alldata <- alldata |> 
  mutate(null = if_else(alldata$sex == 0 & alldata$harm == 0 & alldata$party == 0, 1, 0))

#standardize the dates
alldata <- alldata |> 
  mutate(infr_date = mdy(infr_date))

#create a Dean's Date variable
deans_date_dates <- as.Date(c("2008-05-13", "2009-05-12", "2010-05-11", "2011-05-10", 
                              "2012-05-15", "2013-05-14", "2014-05-13", "2015-05-12", 
                              "2016-05-10", "2017-05-16", "2018-05-15", "2019-05-14", "2020-05-12"))

deans_date_df <- data.frame(infr_date = deans_date_dates)

alldata <- alldata |> 
  mutate(deans_date = case_when(
    alldata$infr_date %in% deans_date_dates ~ 1,
    TRUE ~ 0
  ))

#correct the values for the year variable
alldata <- alldata |> 
  mutate(year = str_sub(infr_date, 1, 4))

#create a month variable
alldata <- alldata |> 
  mutate(month = month(infr_date, label = TRUE))

#create a semester variable
alldata <- alldata |> 
  mutate(semester = case_when(
    month %in% c("Jan", "Feb", "Mar", "Apr", "May") ~ "Spring",
    month %in% c("Jun", "Jul", "Aug") ~ "Summer",
    TRUE ~ "Fall"
  ))
unique(alldata$semester)

#create a variable for semester + year
alldata <-  alldata |> 
  mutate(term = str_c(semester, year, sep = " "))
unique(alldata$term)

#create a variable for month + year
alldata <-  alldata |> 
  mutate(month_year = str_c(month, year, sep = " "))
unique(alldata$month_year)

#factorize
alldata$harm <- factor(alldata$harm)
alldata$sex <- factor(alldata$sex)
alldata$party <- factor(alldata$party)
alldata$term <- factor(alldata$term)
alldata$null <- factor(alldata$null)
alldata$deans_date <- factor(alldata$deans_date)

levels(alldata$harm)
levels(alldata$sex)
levels(alldata$party)
levels(alldata$term)
levels(alldata$null)
levels(alldata$deans_date)

glimpse(alldata)

#arrange by date
alldata <- alldata |> 
  arrange(infr_date)

alldata$term <- reorder(alldata$term, alldata$infr_date)
alldata$month_year <- reorder(alldata$month_year, alldata$infr_date)

#remove 1 data point from 2006
alldata <- alldata |> 
  filter(year != 2006)
unique(alldata$year)

#remove Summer semester
alldata <- alldata |> 
  filter(semester != "Summer") |> 
  droplevels()
levels(alldata$term)

#scrap
table(alldata$dup)
length(alldata$event_unique_id)
length(unique(alldata$event_unique_id))

alldatatest <- alldata |> 
  filter(alldata$dup == 0)

dim(alldatatest[duplicated(alldatatest$event_unique_id),])[1]

n_occur <- data.frame(table(vocabulary$id))

#remove duplicate events
table(alldata$dup)
length(alldata$event_unique_id)
length(unique(alldata$event_unique_id))

alldata <- alldata %>% 
  filter(!duplicated(event_unique_id))

alldata_clean <- write.csv(alldata,"alldata_clean.csv")

#### Look at data for every day in May for 2008-2019

#filter filler data
filler <- c(seq(as.Date("2008-01-01"), as.Date("2008-05-31"), by = "days"),
            seq(as.Date("2009-01-01"), as.Date("2009-05-31"), by = "days"),
            seq(as.Date("2010-01-01"), as.Date("2010-05-31"), by = "days"),
            seq(as.Date("2011-01-01"), as.Date("2011-05-31"), by = "days"),
            seq(as.Date("2012-01-01"), as.Date("2012-05-31"), by = "days"),
            seq(as.Date("2013-01-01"), as.Date("2013-05-31"), by = "days"),
            seq(as.Date("2014-01-01"), as.Date("2014-05-31"), by = "days"),
            seq(as.Date("2015-01-01"), as.Date("2015-05-31"), by = "days"),
            seq(as.Date("2016-01-01"), as.Date("2016-05-31"), by = "days"),
            seq(as.Date("2017-01-01"), as.Date("2017-05-31"), by = "days"),
            seq(as.Date("2018-01-01"), as.Date("2018-05-31"), by = "days"),
            seq(as.Date("2019-01-01"), as.Date("2019-05-31"), by = "days"))

filler_data <- data.frame(infr_date = filler)

table(filler %in% alldata$infr_date)

#make a table with total # of infractions by type
all <- alldata |> 
  group_by(infr_date) |> 
  dplyr::summarise(all = n())

all <- merge(all, filler_data, by = "infr_date", all.x = TRUE, all.y = TRUE)

sex <- alldata[alldata$sex == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(sex = n())

sex <- merge(sex, filler_data, by = "infr_date", all.x = TRUE, all.y = TRUE)

harm <- alldata[alldata$harm == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(harm = n())

harm <- merge(harm, filler_data, by = "infr_date", all.x = TRUE, all.y = TRUE)

sex_harm <- alldata[alldata$sex == 1 & alldata$harm == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(sex_harm = n())

sex_harm <- merge(sex_harm, filler_data, by = "infr_date", all.x = TRUE, all.y = TRUE)

party <- alldata[alldata$party == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(party = n())

party <- merge(party, filler_data, by = "infr_date", all.x = TRUE, all.y = TRUE)

null <- alldata[alldata$null == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(null = n())

null <- merge(null, filler_data, by = "infr_date", all.x = TRUE, all.y = TRUE)

everything <- full_join(all, sex, by = "infr_date")
everything <- full_join(everything, harm, by = "infr_date")
everything <- full_join(everything, sex_harm, by = "infr_date")
everything <- full_join(everything, party, by = "infr_date")
everything <- full_join(everything, null, by = "infr_date")

#convert NAs to 0s
everything[is.na(everything)] <- 0

#convert into tidy format
everything_tidy <- everything |> 
  pivot_longer(names_to = "infr_type",
               values_to = "total",
               cols = -infr_date)

#add a column for year
everything_tidy <- everything_tidy |> 
  mutate(year = year(infr_date))

#add a column for month
everything_tidy <- everything_tidy |> 
  mutate(month = month(infr_date, label = TRUE))

#add a column for day
everything_tidy <- everything_tidy |> 
  mutate(day = day(infr_date))

#create a semester variable
everything_tidy <- everything_tidy |> 
  mutate(semester = case_when(
    month %in% c("Jan", "Feb", "Mar", "Apr", "May") ~ "Spring",
    month %in% c("Jun", "Jul", "Aug") ~ "Summer",
    TRUE ~ "Fall"
  ))
unique(everything_tidy$semester)

#remove 2020
everything_tidy <- everything_tidy |> 
  filter(year != 2020)

#select only Apr & May
everything_tidy <- everything_tidy |> 
  filter(month == "Apr" | month == "May") |> 
  droplevels()

#look only at total # of infractions each day
everything_tidy <- everything_tidy |> 
  filter(infr_type == "all") |> 
  droplevels()

#create a Dean's Date variable
everything_tidy <- everything_tidy |> 
  mutate(deans_date = if_else(infr_date %in% deans_date_dates, 1, 0))

#create an intervention date variable
everything_tidy <- everything_tidy |> 
  mutate(consent_pledge = if_else(infr_date == "2018-05-15", 1, 0))

#create a yearly days since pledge variable
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2008] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2008]) - as.Date(deans_date_dates[1])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2009] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2009]) - as.Date(deans_date_dates[2])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2010] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2010]) - as.Date(deans_date_dates[3])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2011] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2011]) - as.Date(deans_date_dates[4])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2012] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2012]) - as.Date(deans_date_dates[5])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2013] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2013]) - as.Date(deans_date_dates[6])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2014] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2014]) - as.Date(deans_date_dates[7])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2015] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2015]) - as.Date(deans_date_dates[8])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2016] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2016]) - as.Date(deans_date_dates[9])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2017] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2017]) - as.Date(deans_date_dates[10])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2018] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2018]) - as.Date(deans_date_dates[11])
everything_tidy$yearly_dayssince_pledge[everything_tidy$year == 2019] = as.Date(everything_tidy$infr_date[everything_tidy$year == 2019]) - as.Date(deans_date_dates[12])

#rename variable
everything_tidy <- everything_tidy |> 
  dplyr::rename(infr_count = total)

#create a variable identifying the treated days (2018 Dean's Date and subsequent days) 
everything_tidy <- everything_tidy |> 
  mutate(pledge_period = ifelse(yearly_dayssince_pledge >-1 & year == 2018, 1, 0))

#create a variable identifying yearly pre and post Dean's Date
everything_tidy <- everything_tidy |> 
  mutate(pre_deansdate = ifelse(yearly_dayssince_pledge >-1, 0, 1))

#create a unique ID for each Dean's Date
# everything_tidy <- everything_tidy |>
#   mutate(deansdate_uniqueID = case_when(
#     everything_tidy$infr_date == as.Date(deans_date_dates[1]) ~ 1,
#     everything_tidy$infr_date == as.Date(deans_date_dates[2]) ~ 2,
#     everything_tidy$infr_date == as.Date(deans_date_dates[3]) ~ 3,
#     everything_tidy$infr_date == as.Date(deans_date_dates[4]) ~ 4,
#     everything_tidy$infr_date == as.Date(deans_date_dates[5]) ~ 5,
#     everything_tidy$infr_date == as.Date(deans_date_dates[6]) ~ 6,
#     everything_tidy$infr_date == as.Date(deans_date_dates[7]) ~ 7,
#     everything_tidy$infr_date == as.Date(deans_date_dates[8]) ~ 8,
#     everything_tidy$infr_date == as.Date(deans_date_dates[9]) ~ 9,
#     everything_tidy$infr_date == as.Date(deans_date_dates[10]) ~ 10,
#     everything_tidy$infr_date == as.Date(deans_date_dates[11]) ~ 11,
#     everything_tidy$infr_date == as.Date(deans_date_dates[12]) ~ 12,
#     TRUE ~ 0
#   ))

everything_tidy <- everything_tidy |> 
  mutate(deansdate_uniqueID = case_when(
    everything_tidy$deans_date == 1 ~ everything_tidy$year,
    TRUE ~ 0
  ))

everything_tidy$deansdate_uniqueID <- factor(everything_tidy$deansdate_uniqueID)

#regression
summary(lm(infr_count ~ deans_date + consent_pledge + pledge_period + pre_deansdate + deansdate_uniqueID, data = everything_tidy))


ggplot(everything_tidy, aes(infr_count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_y_continuous(breaks = seq(0, 450, by = 50))

table(everything_tidy$infr_count)
mean(everything_tidy$infr_count)
mean(everything_tidy$infr_count[everything_tidy$infr_count < 8])


####### store dv's  #########
dvs <- c('personal_dobbs', 'personal_roe', 
         'courtlegit_composite',
         'approve_court', 'courtsize', 'courtterms',
         'courtideology')

dvs2 <- c('norm', 'moral_norm', 'opinion', 
          'moral_opinion', 'fetal_scale')

dv_last <- c('lagged_personal_dobbs', 
             'lagged_personal_roe',
             'lagged_courtlegit_composite',
             'lagged_approve_court',
             'lagged_courtsize', 'lagged_courtterms', 
             'lagged_courtideology')










