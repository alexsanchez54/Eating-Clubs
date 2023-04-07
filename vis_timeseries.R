#cleaning environment:
rm(list = ls())

#loading relevant libraries
library(tidyverse)
library(lubridate)

#load data
alldata <- read.csv("eclub_all_AD.csv")

#standardize the dates
alldata <- alldata |> 
  mutate(infr_date = mdy(infr_date))

#correct the values for the year variable
alldata <- alldata |> 
  mutate(year = str_sub(event_unique_id, 1, 4))

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

levels(alldata$harm)
levels(alldata$sex)
levels(alldata$party)
levels(alldata$term)

glimpse(alldata)

#arrange by date
alldata <- alldata |> 
  arrange(infr_date)

alldata$term <- reorder(alldata$term, alldata$infr_date)
alldata$month_year <- reorder(alldata$month_year, alldata$infr_date)

OGdata <- alldata

#remove 1 data point from 2006
alldata <- alldata |> 
  filter(year != 2006)
unique(alldata$year)

##compute the avg amount of infractions per month

#remove years that don't have data for the entire year
monthdata <-  alldata |>
  filter(year != 2007 & year != 2020)
unique(monthdata$year)

#compute avg per month
monthdata |> 
  group_by(month) |> 
  dplyr::summarise(mean_per_year = n()/12)

monthdata |> 
  group_by(month) |> 
  dplyr::summarise(total = n())

#compute avg per semester per year
alldata$semester <- factor(alldata$semester, levels = c("Summer", "Fall", "Spring"))
levels(alldata$semester)

alldata |> 
  group_by(semester) |> 
  dplyr::summarise(total = n())

alldata |> 
  group_by(semester) |> 
  dplyr::summarise(mean_per_year = n()/12)

#filter for variables of interest
alldata <- alldata |> 
  select(infr_date, harm, sex, party, year, month, semester, term, month_year)

#remove Summer semester
alldata <- alldata |> 
  filter(semester != "Summer") |> 
  droplevels()
levels(alldata$term)

#remove Fall semester
alldata <- alldata |> 
  filter(semester != "Fall") |> 
  droplevels()
levels(alldata$term)

#calculate total # of infractions by type
table(alldata$sex)
sum(alldata$sex == 1)/length(alldata$sex)*100

table(alldata$harm)
sum(alldata$harm == 1)/length(alldata$harm)*100

table(alldata[alldata$sex == 1, ]$harm)
sum(alldata[alldata$sex == 1, ]$harm == 1)/length(alldata$harm)*100

table(alldata$party)
sum(alldata$party == 1)/length(alldata$party)*100

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

null <- alldata[alldata$sex == 0 & alldata$harm == 0 & alldata$party == 0, ] |> 
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

library(plotly)
library(plyr)

#plot the data
plot <- everything_tidy |> 
  ggplot(aes(y = total, x = month_year, color = infr_type, group = infr_type)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  geom_vline(xintercept = (55), linetype = "dashed") +
  labs(title = "Campus police data 2007-2020",
       x = "Month and year", 
       y = "Number of infractions", 
       color = "Infraction type")
plot <- ggplotly(plot)
plot
