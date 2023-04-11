rm(list = ls())

#loading relevant libraries
library(tidyverse)
library(lubridate)
library(plotly)
library(plyr)
library(dplyr)
library(lemon)
library(ggforce)
library(tufte)
library(writexl)
library(estimatr)
library(readxl)
library(scales)

titleIXreports <- read_xlsx("Title IX reports 2016-2022.xlsx")
alldata_clean <- read.csv("alldata_clean.csv")

#rename variables
titleIXreports <- titleIXreports |> 
  dplyr::rename(affiliation = "Affiliation") |> 
  dplyr::rename(report_date = "Date of Report") |> 
  dplyr::rename(academic_year = "Academic Year")

titleIXreports <- titleIXreports |> 
  mutate(report_date = as.Date(report_date)) |> 
  arrange((report_date))

#change from character to factor variable
titleIXreports$affiliation <- factor(titleIXreports$affiliation)

#only undergrads
table(titleIXreports$affiliation)

titleIXreportsUG <- titleIXreports |> 
  filter(affiliation == "UG")

#remove 1 incidence from 2014
titleIXreportsUG <- titleIXreportsUG |> 
  filter(year(report_date) > 2014)

####plot data

#aggregate the count of reports for each date
titleIXreportsUG_agg <- titleIXreportsUG |> 
  group_by(report_date) |> 
  dplyr::summarise(count = n())

#make a dataframe with the date frame 2015-2022
date_range <- seq.Date(from = as.Date("2015-01-01"), 
                       to = as.Date("2022-12-31"),
                       by = "day")

date_df <- data.frame(report_date = date_range)

#merge datasets
date_counts <- left_join(date_df, titleIXreportsUG_agg, by = "report_date")
date_counts$count[is.na(date_counts$count)] <- 0

#create a Dean's Date indicator
deans_date_dates <- as.Date(c("2015-05-12", "2016-05-10", "2017-05-16", "2018-05-15", 
                              "2019-05-14", "2020-05-12", "2021-05-10", "2022-05-03"))

date_counts <- date_counts |> 
  mutate(deans_date = if_else(report_date %in% deans_date_dates, 1, 0))


p <- 
  date_counts |> 
  filter(year(report_date) > 2020) |> 
  ggplot(aes(x = report_date, y = count)) +
  geom_point(size = 1) +
  geom_line() +
  geom_vline(data = filter(date_counts, deans_date == 1 & year(report_date) > 2020), aes(xintercept = as.numeric(report_date)), linetype = "dashed") +
  scale_x_date(date_labels = "%d %b", date_breaks = "2 day", expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 5)) +
  facet_rep_wrap(~ year(report_date), repeat.tick.labels = TRUE, scales = "free_x", nrow = 3) +
  labs(x = "Day of the Year", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 5))




#group reports by year
titleIXreportsUG <- 
titleIXreports |> 
  filter(affiliation == "UG" & year(report_date) > 2014 & year(report_date) < 2021) |> 
  group_by(year(report_date)) |> 
  dplyr::summarise(num_reports = n())

#compare with other dataset
alldata_clean$infr_date <- as.Date(alldata_clean$infr_date)

alldata_clean |> 
  filter(year(infr_date) > 2014) |> 
  group_by(year(infr_date)) |> 
  dplyr::summarise(num_reports = n())

#select only incidences where sex = 1
alldata_clean_sex <- 
  alldata_clean |> 
  filter(year(infr_date) > 2014 & sex == 1) |> 
  group_by(year(infr_date)) |> 
  dplyr::summarise(num_reports = n())

#select only incidences where sex = 1 AND harm = 1
alldata_clean_sex_harm <- 
alldata_clean |> 
  filter(year(infr_date) > 2014 & sex == 1 & harm == 1) |> 
  group_by(year(infr_date)) |> 
  dplyr::summarise(num_reports = n())

#select only incidences where sex = 1 and harm = 1 AND remove duplicate unique event ids
alldata_clean_sex_harm_unique <- 
  alldata_clean |> 
  filter(year(infr_date) > 2014 & sex == 1 & harm == 1) |> 
  distinct(event_unique_id, .keep_all = TRUE) |> 
  group_by(year(infr_date)) |> 
  dplyr::summarise(num_reports = n())


#Notes
# they're report dates not date of incidents
# affiliation is of the person making the report, not the accused
# not sure if there's duplicates in the Title IX data
# PADR data is all undergrads right?

