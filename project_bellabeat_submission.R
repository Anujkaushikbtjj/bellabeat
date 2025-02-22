## Introduction and background ##

## Upload your CSV files to R 

# https://www.kaggle.com/arashnic/fitbit

## Loading common packages and libraries 

# Load required libraries
library(tidyverse)  # For data manipulation
library(lubridate)  # For handling date/time data
library(janitor)    # For cleaning column names
library(dplyr)


## Load your CSV files 
daily_activity <- data.frame(read.csv("~/practice.datasets/google.data.analytics/bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv"))

sleep_day <- data.frame(read.csv("~/practice.datasets/google.data.analytics/bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv"))

weight_log <- data.frame(read.csv("~/practice.datasets/google.data.analytics/bellabeat/archive/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv"))

# Take a look at the data.
head(daily_activity)
head(sleep_day)
head(weight_log)

# Identify all the columns 
colnames(daily_activity)
colnames(sleep_day)
colnames(weight_log)

# How many unique participants are there in each dataframe? 
# It looks like there may be more participants in the daily activity 
# dataset than the sleep dataset.

n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log$Id)

# How many observations are there in each dataframe?
nrow(daily_activity)
nrow(sleep_day)
nrow(weight_log)

# What are some quick summary statistics 
# For the daily activity dataframe:
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         VeryActiveDistance,
         ModeratelyActiveDistance,
         LightActiveDistance,
         SedentaryMinutes, 
         Calories) %>%
  summary()

# For the sleep dataframe:
sleep_day %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

# For weight_log dataframe:
weight_log %>%
  select(WeightKg,
         Fat,
         BMI) %>%
  summary()

## It looks like the values in TotalDistance and TrackerDistance are same. 
## Lets verify and count any mismatch
daily_activity_mismatch_distance <- daily_activity %>%
  filter(TotalDistance != TrackerDistance)
nrow(daily_activity_mismatch_distance)
view(daily_activity_mismatch_distance)

# Q1: How is TotalSteps related with calories
cor(daily_activity$TotalSteps, daily_activity$Calories, use = "complete.obs") # "complete.obs" means that only rows where both variables have non-missing values will be used in the correlation calculation.
# [1] 0.5915681
# A correlation coefficient of 0.59 is a good positive correlation. 
# This means that more calories burned with more steps taken as expected.

ggplot(data = daily_activity, aes(x=TotalSteps, y= Calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) + # add trend line with linear model. se = TRUE shows the confidence interval
  labs(title = "Total steps vs calories burned",
       x = "Total steps",
       y = "Calories burned") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Q2: It is clear that total steps taken has an impact on calories burned. 
# What kind of activity has more impact? This information would require the calories burned during each activity which is not available.
# We would rather take an indirect approach. Distance vs activity type as we know more steps are directly correlated with more calories burned.
# To achieve we would have to show all the three activities in one scatter plot with trend line.
# We would have to convert the current table in long form using pivot.

# First convert the activity minutes in long format
activity_types_minutes <- daily_activity %>%
  select(Id, ActivityDate, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  pivot_longer(cols = c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes),
               names_to = "Activity_type",
               values_to = "Minutes")
view(activity_types_minutes)

# Now convert the activity distance in long format
activity_types_distance <- daily_activity %>%
  select(Id, ActivityDate, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance) %>%
  pivot_longer(cols = c(VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance),
               names_to = "Activity_type",
               values_to = "Distance")
view(activity_types_distance)

# Rename Activity_type in activity_types_distance to match the names in activity_types_minutes
activity_types_distance <- activity_types_distance %>%
  mutate(Activity_type = case_when(
    Activity_type == "VeryActiveDistance" ~ "VeryActiveMinutes",
    Activity_type == "ModeratelyActiveDistance" ~ "FairlyActiveMinutes",
    Activity_type == "LightActiveDistance" ~ "LightlyActiveMinutes",
    TRUE ~ Activity_type
  ))

# Perform the join on Id, ActivityDate, and the mapped Activity_type
activity_combined <- activity_types_minutes %>%
  left_join(activity_types_distance, by = c("Id", "ActivityDate", "Activity_type"))

# View the result
view(activity_combined)

ggplot(data = activity_combined, aes(x = Minutes, y = Distance, colour = Activity_type)) +
  geom_point(alpha =0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Activity time vs distance",
       x = "Time (minutes)",
       y = "Distance (km)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the correlation coefficients for each activity type and calories burnt
cor(
  activity_combined %>% filter(activity_combined$Activity_type == "VeryActiveMinutes") %>% pull(Minutes), 
  activity_combined %>% filter(activity_combined$Activity_type == "VeryActiveMinutes") %>% pull(Distance), 
  use = "complete.obs")
# [1] 0.8266815

cor(
  activity_combined %>% filter(activity_combined$Activity_type == "FairlyActiveMinutes") %>% pull(Minutes), 
  activity_combined %>% filter(activity_combined$Activity_type == "FairlyActiveMinutes") %>% pull(Distance), 
  use = "complete.obs")
# [1] 0.946934

cor(
  activity_combined %>% filter(activity_combined$Activity_type == "LightlyActiveMinutes") %>% pull(Minutes), 
  activity_combined %>% filter(activity_combined$Activity_type == "LightlyActiveMinutes") %>% pull(Distance), 
  use = "complete.obs")
# [1] 0.8856971

# As expected there is a strong correlation between individual activity and the calories burnt.
# But the slopes are significantly different. The slope is highest in case of VeryActiveMinutes 
# followed by FairlyActiveMinutes and LightlyActiveMinutes.
# This suggest that VeryActiveMinutes lead to more calories burnt compared to other and also the
# higher distance travelled.

# Q3: What is the average time spent by each user in different activities and calories burnt.
avg_activity_time <- daily_activity %>%
  group_by(Id) %>%
  summarise(
    avg_step = mean(TotalSteps),
    avg_distance = mean(TotalDistance),
    avg_very_active_distance = mean(VeryActiveDistance),
    avg_moderate_active_distance = mean(ModeratelyActiveDistance),
    avg_lightly_active_distance = mean(LightActiveDistance),
    avg_very_active_minutes = mean(VeryActiveMinutes),
    avg_moderate_active_minutes = mean(FairlyActiveMinutes),
    avg_lightly_active_minutes = mean(LightlyActiveMinutes),
    avg_sedentary_minutes = mean(SedentaryMinutes),
    avg_calories = mean(Calories)
  ) %>%
  select(Id, avg_step, avg_distance, avg_very_active_distance, avg_moderate_active_distance,
         avg_lightly_active_distance, avg_very_active_minutes, avg_moderate_active_minutes, 
         avg_lightly_active_minutes, avg_sedentary_minutes, avg_calories)

view(avg_activity_time)
# This generates a summary of different activities time by each user

# Q4: What is the average time spent by each user in sleep.
avg_sleep <- sleep_day %>%
  group_by(Id) %>%
  mutate(sleep_perc = (TotalMinutesAsleep / TotalTimeInBed)*100) %>%
  summarise(
    avg_sleep= mean(TotalMinutesAsleep), 
    avg_time_in_bed = mean(TotalTimeInBed),
    avg_sleep_perc =mean(sleep_perc)
  ) %>%
  select(Id, avg_sleep, avg_time_in_bed, avg_sleep_perc)

view(avg_sleep)

summary(avg_sleep$avg_sleep_perc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 63.37   91.40   93.97   91.31   94.88   98.49 
# As we see the majority of users spent maximum time bed time in sleep.

summary(avg_sleep$avg_time_in_bed)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 69.0   377.1   447.9   420.1   485.3   961.0 
# A median of 447.9 minutes (between 7 and 8hrs) spent in bed

summary(avg_sleep$avg_sleep)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 61.0   336.3   419.1   377.6   449.3   652.0 
# A median of 419.1minutes (almost 7hrs) spent in sleep

# Q4: What's the relationship between steps taken in a day and sedentary minutes? 
# How could this help inform the customer segments that we can market to? 
# E.g. position this more as a way to get started in walking more? 
# Or to measure steps that you're already taking?

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Relationship Between Sdentary Minutes and Steps",
       x = "Total Steps",
       y = "Sedentary Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

cor(daily_activity$SedentaryMinutes, daily_activity$TotalSteps, use = "complete.obs")

# [1] -0.3274835
# The graph shows a trend line with negative corelation. But this may not be true as we do not
# see many data point on the line. 
# Interestingly there seems to be 2 different clusters: one above 1000 sedentary minutes and another
# below 1000 sedentary minutes. These two clusters can indicate days with highly sedentary (above 1000)
# and less sedentary (below 1000) lifestyle.


# Q5: We observe two different clusters above and below 1000 SedentaryMinutes.
# We would like to highlight such clusters and try a manual approach.
# The K means clustering somehow did not perform well in this case.

# Creating a new column Cluster based on the SedentaryMinutes above or below 1000
daily_activity$Cluster <- 
  ifelse(daily_activity$SedentaryMinutes >= 1000, "High Sedentary", "Low Sedentary")

head(daily_activity)

# Convert to factor for ggplot
daily_activity$Cluster <- as.factor(daily_activity$Cluster)

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, color=Cluster)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Relationship Between Sedentary Minutes and Steps with Clusters",
       x = "Total Steps",
       y = "Sedentary Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Lets identify the median for TotalSteps
summary(daily_activity$TotalSteps)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0    3790    7406    7638   10727   36019 

# With the median of 7406 we would further cluster our dataset
daily_activity <- daily_activity %>%
  mutate(
    Cluster = case_when(
      SedentaryMinutes >= 1000 & TotalSteps >= 7406 ~ "High Sedentary High Active",
      SedentaryMinutes >= 1000 & TotalSteps < 7406 ~ "High Sedentary Less Active",
      SedentaryMinutes < 1000 & TotalSteps >= 7406 ~ "Less Sedentary High Active",
      SedentaryMinutes < 1000 & TotalSteps <= 7406 ~ "Less Sedentary Less Active"
    )
  )
head(daily_activity)

# Follow the previous steps
daily_activity$Cluster <- as.factor(daily_activity$Cluster)

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, color=Cluster)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Relationship Between Sedentary Minutes and Steps with Clusters",
       x = "Total Steps",
       y = "Sedentary Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
