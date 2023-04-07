#cleaning environment:
rm(list = ls())

#loading relevant libraries
library(tidyverse)
library(lubridate)
library(plotly)
library(plyr)
library(lemon)
library(ggforce)

#load data
alldata <- read.csv("eclub_all_AD.csv")

#create a null variable
alldata <- alldata |> 
  mutate(null = if_else(alldata$sex == 0 & alldata$harm == 0 & alldata$party == 0, 1, 0))

#standardize the dates
alldata <- alldata |> 
  mutate(infr_date = mdy(infr_date))

###make filler data for missing days in May (Dean's Date month)
filler <- c(seq(as.Date("2008-05-01"), as.Date("2008-05-31"), by = "days"),
            seq(as.Date("2009-05-01"), as.Date("2009-05-31"), by = "days"),
            seq(as.Date("2010-05-01"), as.Date("2010-05-31"), by = "days"),
            seq(as.Date("2011-05-01"), as.Date("2011-05-31"), by = "days"),
            seq(as.Date("2012-05-01"), as.Date("2012-05-31"), by = "days"),
            seq(as.Date("2013-05-01"), as.Date("2013-05-31"), by = "days"),
            seq(as.Date("2014-05-01"), as.Date("2014-05-31"), by = "days"),
            seq(as.Date("2015-05-01"), as.Date("2015-05-31"), by = "days"),
            seq(as.Date("2016-05-01"), as.Date("2016-05-31"), by = "days"),
            seq(as.Date("2017-05-01"), as.Date("2017-05-31"), by = "days"),
            seq(as.Date("2018-05-01"), as.Date("2018-05-31"), by = "days"),
            seq(as.Date("2019-05-01"), as.Date("2019-05-31"), by = "days"),
            seq(as.Date("2020-05-01"), as.Date("2020-05-31"), by = "days"))

filler_data <- data.frame(infr_date = filler)

#check how many May 2008-2020 days are missing from the dataset
table(filler %in% alldata$infr_date)

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

####Look at data by term

#make a table with total # of infractions by type
all <- alldata |> 
  group_by(term) |> 
  dplyr::summarise(all = n())

sex <- alldata[alldata$sex == 1, ] |> 
  group_by(term) |> 
  dplyr::summarise(sex = n())

harm <- alldata[alldata$harm == 1, ] |> 
  group_by(term) |> 
  dplyr::summarise(harm = n())

sex_harm <- alldata[alldata$sex == 1 & alldata$harm == 1, ] |> 
  group_by(term) |> 
  dplyr::summarise(sex_harm = n())

party <- alldata[alldata$party == 1, ] |> 
  group_by(term) |> 
  dplyr::summarise(party = n())

null <- alldata[alldata$null == 1, ] |> 
  group_by(term) |> 
  dplyr::summarise(null = n())

everything <- full_join(all, sex, by = "term")
everything <- full_join(everything, harm, by = "term")
everything <- full_join(everything, sex_harm, by = "term")
everything <- full_join(everything, party, by = "term")
everything <- full_join(everything, null, by = "term")

#convert NAs to 0s
everything[is.na(everything)] <- 0

#convert into tidy format
everything_tidy <- everything |> 
  pivot_longer(names_to = "infr_type",
               values_to = "total",
               cols = -term)

#plot the data
plot_term <- everything_tidy |> 
  ggplot(aes(y = total, x = term, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 200, by = 20)) +
  geom_vline(xintercept = (22), linetype = "dashed") +
  labs(title = "Campus police data 2007-2020 by school term",
       x = "School term", 
       y = "Number of infractions", 
       color = "Infraction type")
plot_term <- ggplotly(plot_term)
plot_term

####Look at data by month + only Spring semester

#remove Fall semester
alldata <- alldata |> 
  filter(semester != "Fall") |> 
  droplevels()
levels(alldata$term)

#make a table with total # of infractions by type
all <- alldata |> 
  group_by(month_year) |> 
  dplyr::summarise(all = n())

sex <- alldata[alldata$sex == 1, ] |> 
  group_by(month_year) |> 
  dplyr::summarise(sex = n())

harm <- alldata[alldata$harm == 1, ] |> 
  group_by(month_year) |> 
  dplyr::summarise(harm = n())

sex_harm <- alldata[alldata$sex == 1 & alldata$harm == 1, ] |> 
  group_by(month_year) |> 
  dplyr::summarise(sex_harm = n())

party <- alldata[alldata$party == 1, ] |> 
  group_by(month_year) |> 
  dplyr::summarise(party = n())

null <- alldata[alldata$null == 1, ] |> 
  group_by(month_year) |> 
  dplyr::summarise(null = n())

everything <- full_join(all, sex, by = "month_year")
everything <- full_join(everything, harm, by = "month_year")
everything <- full_join(everything, sex_harm, by = "month_year")
everything <- full_join(everything, party, by = "month_year")
everything <- full_join(everything, null, by = "month_year")

#convert NAs to 0s
everything[is.na(everything)] <- 0

#convert into tidy format
everything_tidy <- everything |> 
  pivot_longer(names_to = "infr_type",
               values_to = "total",
               cols = -month_year)

#plot the data
plot_month <- everything_tidy |> 
  ggplot(aes(y = total, x = month_year, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = (55), linetype = "dashed") +
  labs(title = "Campus police data 2007-2020: Spring months",
       x = "Month and year", 
       y = "Number of infractions", 
       color = "Infraction type")
plot_month <- ggplotly(plot_month)
plot_month

#plot the data for only the month of May
everything_tidy_May <- everything_tidy |>
  filter(str_detect(month_year, pattern = "May")) |> 
  droplevels()

plot_month_May <- everything_tidy_May |> 
  ggplot(aes(y = total, x = month_year, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = (11), linetype = "dashed") +
  labs(title = "Campus police data 2007-2020: only May",
       x = "Month and year", 
       y = "Number of infractions", 
       color = "Infraction type")
plot_month_May <- ggplotly(plot_month_May)
plot_month_May

#plot the data for only the months of Apr & May
everything_tidy_Apr_May <- everything_tidy |>
  filter(str_detect(month_year, pattern = "May|Apr")) |> 
  droplevels()

plot_month_Apr_May <- everything_tidy_Apr_May |> 
  ggplot(aes(y = total, x = month_year, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = (22), linetype = "dashed") +
  labs(title = "Campus police data 2007-2020: only Apr & May",
       x = "Month and year", 
       y = "Number of infractions", 
       color = "Infraction type")
plot_month_Apr_May <- ggplotly(plot_month_Apr_May)
plot_month_Apr_May

#### Look at data for Dean's Date

#remove Fall semester
alldata <- alldata |> 
  filter(semester != "Fall") |> 
  droplevels()
levels(alldata$term)

#isolate Dean's date
alldata <- alldata |> 
  filter(deans_date == 1) |> 
  droplevels()
levels(alldata$deans_date)

#make a table with total # of infractions by type
all <- alldata |> 
  group_by(infr_date) |> 
  dplyr::summarise(all = n())

all <- merge(all, deans_date_df, by = "infr_date", all.x = TRUE, all.y = TRUE)

sex <- alldata[alldata$sex == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(sex = n())

sex <- merge(sex, deans_date_df, by = "infr_date", all.x = TRUE, all.y = TRUE)

harm <- alldata[alldata$harm == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(harm = n())

harm <- merge(harm, deans_date_df, by = "infr_date", all.x = TRUE, all.y = TRUE)

sex_harm <- alldata[alldata$sex == 1 & alldata$harm == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(sex_harm = n())

sex_harm <- merge(sex_harm, deans_date_df, by = "infr_date", all.x = TRUE, all.y = TRUE)

party <- alldata[alldata$party == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(party = n())

party <- merge(party, deans_date_df, by = "infr_date", all.x = TRUE, all.y = TRUE)

null <- alldata[alldata$null == 1, ] |> 
  group_by(infr_date) |> 
  dplyr::summarise(null = n())

null <- merge(null, deans_date_df, by = "infr_date", all.x = TRUE, all.y = TRUE)

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

#plot the data
plot_month <- everything_tidy |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = (55), linetype = "dashed") +
  labs(title = "Campus police data 2007-2020: Spring Dean's Date",
       x = "Dean's Date", 
       y = "Number of infractions", 
       color = "Infraction type")
plot_month <- ggplotly(plot_month)
plot_month

#### Look at data for every day in May for 2008-2019

#isolate May days
alldata <- alldata |> 
  filter(month == "May") |> 
  droplevels()
levels(alldata$month)

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

#add a column for day
everything_tidy <- everything_tidy |> 
  mutate(day = day(infr_date))

#remove 2020
everything_tidy <- everything_tidy |> 
  filter(year != 2020)

#plot the data
plot_month <- everything_tidy |> 
  ggplot(aes(y = total, x = day, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  geom_point() +
  geom_line() +
  facet_rep_wrap(~ year, repeat.tick.labels = TRUE) +
  scale_y_continuous(breaks = seq(0, 20, by = 1)) +
  scale_x_continuous(breaks = seq(0, 31, by = 1)) +
  labs(title = "Campus police data 2007-2019: month of May by day",
       x = "Day of Month", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates. The red dashed line highlights the 2018 university-wide consent pledge intervention.",
       color = "Infraction type")

plot_month <- plot_month + 
  geom_vline(data = filter(everything_tidy, year == 2008), aes(xintercept=13), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2009), aes(xintercept=12), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2010), aes(xintercept=11), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2011), aes(xintercept=10), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2012), aes(xintercept=15), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2013), aes(xintercept=14), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2014), aes(xintercept=13), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2015), aes(xintercept=12), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2016), aes(xintercept=10), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2017), aes(xintercept=16), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2018), aes(xintercept=15), linetype = "dashed", colour = "red") +
  geom_vline(data = filter(everything_tidy, year == 2019), aes(xintercept=14), linetype = "dashed")
  
plot_month

plotly_month <- ggplotly(plot_month)
plotly_month

### Plot every day for Spring 2008- Spring 2019

#remove 2020
alldata <- alldata |> 
  filter(year != 2020) |> 
  droplevels()
unique(alldata$year)

#remove Fall semester
alldata <- alldata |> 
  filter(semester == "Spring") |> 
  droplevels()
levels(alldata$term)

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

#add a column for day
everything_tidy <- everything_tidy |> 
  mutate(day = day(infr_date))

#identify Dean's date row number
which(everything_tidy$infr_date == "2008-05-13")
which(everything_tidy$infr_date == "2009-05-12")
which(everything_tidy$infr_date == "2010-05-11")
which(everything_tidy$infr_date == "2011-05-10")
which(everything_tidy$infr_date == "2012-05-15")
which(everything_tidy$infr_date == "2013-05-14")
which(everything_tidy$infr_date == "2014-05-13")
which(everything_tidy$infr_date == "2015-05-12")
which(everything_tidy$infr_date == "2016-05-10")
which(everything_tidy$infr_date == "2017-05-16")
which(everything_tidy$infr_date == "2018-05-15")
which(everything_tidy$infr_date == "2019-05-14")

#plot the data
plot_month1 <- everything_tidy |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  #facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  facet_wrap_paginate(~ year, nrow = 12, ncol = 1, scales = "free_x", page = 1) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2008, 2009, & 2010",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.",
       color = "Infraction type")

plot_month1 <- plot_month1 + 
  geom_vline(data = filter(everything_tidy, year == 2008), aes(xintercept=as.numeric(everything_tidy$infr_date[799])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2009), aes(xintercept=as.numeric(everything_tidy$infr_date[1699])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2010), aes(xintercept=as.numeric(everything_tidy$infr_date[2599])), linetype = "dashed")

plot_month1

plotly_month1 <- ggplotly(plot_month1)
plotly_month1

#plot the data
plot_month2 <- everything_tidy |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  #facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 2) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2011, 2012, & 2013",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.",
       color = "Infraction type")

plot_month2 <- plot_month2 + 
  geom_vline(data = filter(everything_tidy, year == 2011), aes(xintercept=as.numeric(everything_tidy$infr_date[3499])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2012), aes(xintercept=as.numeric(everything_tidy$infr_date[4441])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2013), aes(xintercept=as.numeric(everything_tidy$infr_date[5341])), linetype = "dashed")

plot_month2

plotly_month2 <- ggplotly(plot_month2)
plotly_month2

#plot the data
plot_month3 <- everything_tidy |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  #facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 3) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2014, 2015, & 2016",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.",
       color = "Infraction type")

plot_month3 <- plot_month3 + 
  geom_vline(data = filter(everything_tidy, year == 2014), aes(xintercept=as.numeric(everything_tidy$infr_date[6241])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2015), aes(xintercept=as.numeric(everything_tidy$infr_date[7141])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2016), aes(xintercept=as.numeric(everything_tidy$infr_date[8041])), linetype = "dashed")

plot_month3

plotly_month3 <- ggplotly(plot_month3)
plotly_month3


#plot the data
plot_month4 <- everything_tidy |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  #facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 4) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2017, 2018, & 2019",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.  The red dashed line highlights the 2018 university-wide consent pledge intervention.",
       color = "Infraction type")

plot_month4 <- plot_month4 + 
  geom_vline(data = filter(everything_tidy, year == 2017), aes(xintercept=as.numeric(everything_tidy$infr_date[8983])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2018), aes(xintercept=as.numeric(everything_tidy$infr_date[9883])), linetype = "dashed", colour = "red") +
  geom_vline(data = filter(everything_tidy, year == 2019), aes(xintercept=as.numeric(everything_tidy$infr_date[10783])), linetype = "dashed")

plot_month4

plotly_month4 <- ggplotly(plot_month4)
plotly_month4

# interactive 

#plot the data
plot_month1int <- everything_tidy[everything_tidy$year == 2008 | everything_tidy$year == 2009 | everything_tidy$year == 2010 | everything_tidy$year == 2011 | everything_tidy$year == 2012,] |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 5, ncol = 1, scales = "free_x") +
  #facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 1) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2008, 2009, & 2010",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.",
       color = "Infraction type")

plot_month1int <- plot_month1int + 
  geom_vline(data = filter(everything_tidy, year == 2008), aes(xintercept=as.numeric(everything_tidy$infr_date[799])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2009), aes(xintercept=as.numeric(everything_tidy$infr_date[1699])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2010), aes(xintercept=as.numeric(everything_tidy$infr_date[2599])), linetype = "dashed")

plot_month1int

plotly_month1int <- ggplotly(plot_month1int)
plotly_month1int

#plot the data
plot_month2int <- everything_tidy[everything_tidy$year == 2011 | everything_tidy$year == 2012 | everything_tidy$year == 2013, ] |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  #facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 2) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2011, 2012, & 2013",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.",
       color = "Infraction type")

plot_month2int <- plot_month2int + 
  geom_vline(data = filter(everything_tidy, year == 2011), aes(xintercept=as.numeric(everything_tidy$infr_date[3499])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2012), aes(xintercept=as.numeric(everything_tidy$infr_date[4441])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2013), aes(xintercept=as.numeric(everything_tidy$infr_date[5341])), linetype = "dashed")

plot_month2int

plotly_month2int <- ggplotly(plot_month2int)
plotly_month2int

#plot the data
plot_month3int <- everything_tidy[everything_tidy$year == 2014 | everything_tidy$year == 2015 | everything_tidy$year == 2016, ] |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  #facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 3) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2014, 2015, & 2016",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.",
       color = "Infraction type")

plot_month3int <- plot_month3int + 
  geom_vline(data = filter(everything_tidy, year == 2014), aes(xintercept=as.numeric(everything_tidy$infr_date[6241])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2015), aes(xintercept=as.numeric(everything_tidy$infr_date[7141])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2016), aes(xintercept=as.numeric(everything_tidy$infr_date[8041])), linetype = "dashed")

plot_month3int

plotly_month3int <- ggplotly(plot_month3int)
plotly_month3int


#plot the data
plot_month4int <- everything_tidy[everything_tidy$year == 2017 | everything_tidy$year == 2018 | everything_tidy$year == 2019, ] |> 
  ggplot(aes(y = total, x = infr_date, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point() +
  geom_line() +
  facet_rep_wrap(~ year, repeat.tick.labels = TRUE, nrow = 3, ncol = 1, scales = "free_x") +
  #facet_wrap_paginate(~ year, nrow = 3, ncol = 1, scales = "free_x", page = 4) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 40, by = 1)) +
  labs(title = "Campus police data: Spring 2017, 2018, & 2019",
       x = "Day", 
       y = "Number of infractions",
       caption = "Vertical dashed lines highlight Spring semester Dean's Dates.  The red dashed line highlights the 2018 university-wide consent pledge intervention.",
       color = "Infraction type")

plot_month4int <- plot_month4int + 
  geom_vline(data = filter(everything_tidy, year == 2017), aes(xintercept=as.numeric(everything_tidy$infr_date[8983])), linetype = "dashed") +
  geom_vline(data = filter(everything_tidy, year == 2018), aes(xintercept=as.numeric(everything_tidy$infr_date[9883])), linetype = "dashed", colour = "red") +
  geom_vline(data = filter(everything_tidy, year == 2019), aes(xintercept=as.numeric(everything_tidy$infr_date[10783])), linetype = "dashed")

plot_month4int

plotly_month4int <- ggplotly(plot_month4int)
plotly_month4int