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
library(lsr)
library(effsize)
library(plotly)
library(lemon)

# To-do:
# Do Mummolo-style placebo checks

# Load data
alldata <- read.csv("eclub_all_AD.csv")

#Add variables
alldata_clean <- alldata |> 
  mutate(null = if_else(sex == 0 & harm == 0 & party == 0, 1, 0), #control
         not_null = if_else(sex == 1 | harm == 1 | party == 1, 1, 0), #treatment (harm, sex, or party infraction)
         infr_date = mdy(infr_date), #standardize
         sexual_misconduct = ifelse(primary_cat == "Sexual Misconduct", 1, 0)) |>  #Title IX cases (and another potential control if 0)
  arrange(infr_date) |> 
  mutate_at(vars(harm, sex, party, null), factor) |> #factorize
  select(- year, -X) #remove columns with redundant/incorrect values

# Save cleaned data
# write.csv(alldata_clean, "alldata_clean.csv")

#### Pre-process data ####
alldata_summary <- alldata_clean |> 
  group_by(infr_date) |> 
  dplyr::summarize(
    all = n(),
    sex = sum(sex == 1),
    harm = sum(harm == 1),
    sex_harm = sum(sex == 1 & harm == 1),
    party = sum(party == 1),
    null = sum(null == 1),
    not_null = sum(not_null == 1),
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
         treated_days = if_else(infr_date >=as.Date("2018-05-15") & infr_date <= as.Date("2018-05-31"), 1, 0), #identify days after the intervention date for 2018 only
         year = year(infr_date),
         month = month(infr_date),
         day = day(infr_date),
         semester = case_when( 
           month(infr_date) %in% c(1, 2, 3, 4, 5) ~ "Spring",
           month(infr_date) %in% c(6, 7, 8) ~ "Summer",
           TRUE ~ "Fall"),
  )

# Create a days since Spring Dean's Date column, subsetted by year
for (i in 2008:2020) {
  alldata_summary$dayssince_deansdate_byyear[year(alldata_summary$infr_date) == i] <- as.Date(alldata_summary$infr_date[year(alldata_summary$infr_date) == i]) - as.Date(deans_date_dates[i - 2007])
}

#Create a variable that indicates the number of days in May following dean's date each year
for (i in 2008:2020) {
  alldata_summary$days_after_deans_date[year(alldata_summary$infr_date) == i] <- as.Date(paste(i, "-05-31", sep="")) - as.Date(deans_date_dates[i - 2007])
}

#Create a variable identifying yearly period after Dean's Dates
alldata_summary <- alldata_summary |> 
  mutate(post_deansdate = ifelse(dayssince_deansdate_byyear > -1, 1, 0)) |> 
  mutate_at(vars(deans_date, spring_break, pledge, semester, 
                 post_deansdate, treated_days), as.factor) #factorize

#Subset to only Spring semesters 2008-2019
alldata_summary <- alldata_summary |> 
  filter(year(infr_date) > 2007 & year(infr_date) < 2020 & semester == "Spring") |> 
  droplevels()

# Save summarized data
write.csv(alldata_summary, "alldata_summary.csv")

#Tidy data
alldata_tidy <- alldata_summary |> 
  pivot_longer(cols = c(all, sex, harm, sex_harm, party, null, not_null, sexual_misconduct),
               names_to = "infr_type",
               values_to = "total")

# Save tidy data
write.csv(alldata_tidy, "alldata_tidy.csv")

#### Analyses ####

sum(alldata_summary$all)

alldata_summary |> 
  group_by(year) |> 
  summarize(count = sum(all),
            not_null = sum(not_null),
            null = sum(null))

alldata_summary |> 
  group_by(month) |> 
  summarize(count = sum(all),
            not_null = sum(not_null),
            null = sum(null))

alldata_summary |> 
  group_by(day) |> 
  summarize(count = sum(all),
            not_null = sum(not_null),
            null = sum(null))

# minimal sexual harm data so doesn't make sense to regress sex_harm or sexual_misconduct

######### #Regressions #########
# not_null: infractions that were classified as harm, sex, or party (treatment outcome)
# null: infractions that weren't harm, sex, or party (control outcome)
# deans_date: Spring semester deans date indicator (12 total)
# pledge: indicator for day of the pledge (May 15 2018)
# treated_days: what we consider the treated days, based on assumptions of how long the treatment effects persist
# post_deansdate: indicator for days after dean's date each year
# days_after_deans_date: indicates the number of days in May following dean's date each year
# don't include days_after_deans_date for now, BLP will check on this

alldata_summary |> 
  group_by(treated_days) |> 
  summarize(avg = mean(not_null))

######## Initial regressions #####

#May
summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate,
           data = alldata_summary |> filter(month == 5)))

summary(lm(not_null ~ deans_date + pledge + treated_days + spring_break + post_deansdate,
           data = alldata_summary |> filter(month == 5)))

#mid-April through end of May
summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate, 
           data = alldata_summary |> filter(month(infr_date) %in% c(4, 5) & 
                                              (month(infr_date) != 4 | day(infr_date) >= 15))))

#April and May
summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate,
           data = alldata_summary |> filter(month(infr_date) %in% c(4, 5))))

#mid-March through end of May
summary(lm(not_null ~ deans_date + pledge + treated_days + spring_break + post_deansdate, 
           data = alldata_summary |> filter(month(infr_date) %in% c(3, 4, 5) & 
                                              (month(infr_date) != 3 | day(infr_date) >= 15))))

#March - May (includes Spring Break control)
summary(lm(not_null ~ deans_date + pledge + treated_days + spring_break + post_deansdate,
           data = alldata_summary |> filter(month(infr_date) %in% c(3, 4, 5))))

summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate,
           data = alldata_summary |> filter(month(infr_date) %in% c(3, 4, 5))))

#Feb - May (includes Spring Break control)
summary(lm(not_null ~ deans_date + pledge + treated_days + spring_break + post_deansdate,
           data = alldata_summary |> filter(month(infr_date) %in% c(2, 3, 4, 5))))

summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate,
           data = alldata_summary |> filter(month(infr_date) %in% c(2, 3, 4, 5))))

#Jan - May (all of Spring semester; includes Spring Break control)
summary(lm(not_null ~  deans_date + pledge + treated_days + spring_break + post_deansdate, 
           data = alldata_summary))

###### 
#
summary(lm(not_null ~  deans_date + pledge + treated_days + spring_break + post_deansdate, 
           data = alldata_summary))

summary(lm(null ~  deans_date + pledge + treated_days + spring_break + post_deansdate, 
           data = alldata_summary))


### Compute effect size

cohensD(not_null ~ treated_days, data = alldata_summary)

cohensD(not_null ~ treated_days, data = droplevels(subset(alldata_summary, month(infr_date) %in% 5)))

# standard deviation
sd(alldata_summary$not_null)

####### Placebo Checks #######

#remove leap days for now for simplicity (Alex fix this later)
alldata_summary$leap_days <- ifelse(month(alldata_summary$infr_date) == 2 
                                    & day(alldata_summary$infr_date) == 29, 
                                    1, 0)

alldata_summary <- alldata_summary |> 
  filter(leap_days != 1)

#create a variable of sequence of days by year
alldata_summary <- alldata_summary |>
  group_by(year) |> 
  mutate(day_by_year = 1:n())

summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate + spring_break, 
           data = alldata_summary |> filter(day_by_year >= (1))))

###### Plot p values for not_null #####

# Initialize an empty dataframe to store results
results_df <- data.frame()

# Date range
dates <- seq.Date(as.Date("2018-05-14"), length.out = 135, by = "-1 day")

# Loop over 1 to 135
for (i in 1:135) {
  
  # Filter the data
  filtered_data <- alldata_summary |> filter(day_by_year >= (135 - i))
  
  # include spring_break as a control when appropriate
  formula <- if (length(unique(filtered_data$spring_break)) == 2) {
    not_null ~ deans_date + pledge + treated_days + post_deansdate + spring_break
  } else {
    not_null ~ deans_date + pledge + treated_days + post_deansdate
  }
  
  # Fit the model
  fit <- lm(formula, data = filtered_data)
  
  # Get the coefficients
  coef_summary <- summary(fit)$coefficients
  
  # Get the p-value for treated_days
  p_value <- coef_summary["treated_days", "Pr(>|t|)"]
  
  # Calculate Cohen's D
  cohens_d <- cohensD(not_null ~ treated_days, data = filtered_data)
  
  # Transpose the result and convert it to a dataframe
  df <- data.frame(p_value, cohens_d)
  
  # Add a column to indicate the iteration
  df$pre_pledge_cutoff <- dates[i]
  
  # Append the results to results_df
  results_df <- rbind(results_df, df)
}

#remove extra dates
results_df <- results_df |> 
  filter(year(pre_pledge_cutoff) == 2018)

###

plot <- 
  results_df |> 
  ggplot(aes(x = pre_pledge_cutoff, y = p_value)) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point(aes(alpha = cohens_d), size = 2) +
  geom_hline(yintercept = 0.05, linetype = 2, color = "red") +
  #facet_rep_wrap(~ post_pledge_cutoff, repeat.tick.labels = TRUE, ncol = 1, scales = "free_x") +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  scale_y_continuous(limits = c(0.025, 0.125), breaks = seq(0.005, 0.125, by = 0.005)) +
  labs(title = "P values for the effect of the pledge on subsequent # of infractions through the rest of Spring semester 2018, by pre-pledge cutoff date",       x = "Pre-pledge cutoff date",
       y = "P value",
       alpha = "Cohen's D")

plotly <- ggplotly(plot)

####Include p values for null ####

# Initialize an empty dataframe to store results
results_df <- data.frame()

# Date range
dates <- seq.Date(as.Date("2018-05-14"), length.out = 135, by = "-1 day")

# Loop over 1 to 135
for (i in 1:135) {
  
  # Filter the data
  filtered_data <- alldata_summary |> filter(day_by_year >= (135 - i))
  
  # Determine the formula to use based on the number of levels of spring_break in the filtered data
  formula_not_null <- if (length(unique(filtered_data$spring_break)) == 2) {
    not_null ~ deans_date + pledge + treated_days + post_deansdate + spring_break
  } else {
    not_null ~ deans_date + pledge + treated_days + post_deansdate
  }
  
  formula_null <- if (length(unique(filtered_data$spring_break)) == 2) {
    null ~ deans_date + pledge + treated_days + post_deansdate + spring_break
  } else {
    null ~ deans_date + pledge + treated_days + post_deansdate
  }
  
  # Fit the models
  fit_not_null <- lm(formula_not_null, data = filtered_data)
  fit_null <- lm(formula_null, data = filtered_data)
  
  # Get the coefficients
  coef_summary_not_null <- summary(fit_not_null)$coefficients
  coef_summary_null <- summary(fit_null)$coefficients
  
  # Get the p-value for treated_days
  p_value_not_null <- coef_summary_not_null["treated_days", "Pr(>|t|)"]
  p_value_null <- coef_summary_null["treated_days", "Pr(>|t|)"]
  
  # Calculate Cohen's D
  cohens_d_not_null <- cohensD(not_null ~ treated_days, data = filtered_data)
  cohens_d_null <- cohensD(null ~ treated_days, data = filtered_data)
  
  # Transpose the result and convert it to a dataframe
  df <- data.frame(p_value, cohens_d)
  
  # Create a dataframe for the results
  df_not_null <- data.frame(p_value = p_value_not_null, cohens_d = cohens_d_not_null, outcome = "not_null", pre_pledge_cutoff = dates[i])
  df_null <- data.frame(p_value = p_value_null, cohens_d = cohens_d_null, outcome = "null", pre_pledge_cutoff = dates[i])
  
  # Append the results to results_df
  results_df <- rbind(results_df, df_not_null, df_null)
}

plot <- 
  results_df |> 
  ggplot(aes(x = pre_pledge_cutoff, y = p_value, color = outcome)) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point(aes(alpha = cohens_d), size = 2) +
  geom_hline(yintercept = 0.05, linetype = 2, color = "red") +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  labs(title = "P values for the effect of the pledge on subsequent # of infractions through the rest of Spring semester 2018, by pre-pledge cutoff date",
       caption = "different colors indicate type of infraction (those we expect to be influenced by the treatment vs not)",
       x = "Pre-pledge cutoff date",
       y = "P value")

plotly1 <- ggplotly(plot)

###

##### Plot p values for not_null at different bandwidths ####

# Initialize an empty dataframe to store results
results_df <- data.frame()

# Date range
dates_pre <- seq.Date(as.Date("2018-05-14"), length.out = 135, by = "-1 day")
dates_post <- c("May 31 2018: 16 days after pledge (rest of Spring semester)", 
                "2018-05-26: 11 days after pledge", 
                "2018-05-21: 6 days after pledge")

# Outer loop
for (j in 1:3) {
  # Alter treated_days based on j
  alldata_summary <- alldata_summary |> 
    mutate(treated_days = if_else(year == 2018 & day_by_year >= 135 & day_by_year <= (156 - 5*j), 1, 0))
  
  # Inner loop
  for (i in 1:135) {
    
    # Filter the data
    filtered_data <- alldata_summary |> filter(day_by_year >= (135 - i))
    
    # include spring_break as a control when appropriate
    formula <- if (length(unique(filtered_data$spring_break)) == 2) {
      not_null ~ deans_date + pledge + treated_days + post_deansdate + spring_break
    } else {
      not_null ~ deans_date + pledge + treated_days + post_deansdate
    }
    
    # Fit the model
    fit <- lm(formula, data = filtered_data)
    
    # Get the coefficients
    coef_summary <- summary(fit)$coefficients
    
    # Get the p-value for treated_days
    p_value <- coef_summary["treated_days", "Pr(>|t|)"]
    
    # Calculate Cohen's D
    cohens_d <- cohensD(not_null ~ treated_days, data = filtered_data)
    
    # Transpose the result and convert it to a dataframe
    df <- data.frame(p_value, cohens_d)
    
    # Add a column to indicate the iteration of i
    df$pre_pledge_cutoff <- dates_pre[i]
    
    # Add a column to indicate the iteration of j
    df$post_pledge_cutoff <- dates_post[j]
    
    # Append the results to results_df
    results_df <- rbind(results_df, df)
  }
}

#remove extra dates
results_df <- results_df |> 
  filter(year(pre_pledge_cutoff) == 2018)

results_df <- results_df |> 
  mutate(post_pledge_cutoff = factor(post_pledge_cutoff, 
                                     levels = c(
                                       "May 31 2018: 16 days after pledge (rest of Spring semester)", 
                                       "2018-05-26: 11 days after pledge", 
                                       "2018-05-21: 6 days after pledge")))

plot <- 
  results_df |> 
  ggplot(aes(x = pre_pledge_cutoff, y = p_value)) +
  theme(axis.text.x = element_text(angle = 90, size = 5)) +
  theme(axis.text.y = element_text(size = 5)) +
  geom_point(aes(alpha = cohens_d), size = 2) +
  geom_hline(yintercept = 0.05, linetype = 2, color = "red") +
  facet_rep_wrap(~ post_pledge_cutoff, repeat.tick.labels = TRUE, ncol = 1, scales = "free_x") +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day", expand = c(0,0)) +
  #scale_y_continuous(limits = c(0.025, 0.125), breaks = seq(0.005, 0.125, by = 0.005)) +
  labs(title = "P values for the effect of the pledge on subsequent # of infractions at different post-pledge cutoff dates, by pre-pledge cutoff date",
       caption = "The more opaque the points, the larger the Cohen's d effect size",
       x = "Pre-pledge cutoff date",
       y = "P value",
       alpha = "Cohen's D")

plotly2 <- ggplotly(plot)


######


summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate + spring_break,
           data = alldata_summary))

summary(lm(not_null ~ deans_date + pledge + treated_days + post_deansdate + spring_break,
           data = alldata_summary |> filter(month %in% c(3, 4, 5))))
