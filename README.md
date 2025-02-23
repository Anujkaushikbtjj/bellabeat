# Bellabeat Data Analysis Case Study

## Description

Bellabeat is a small high-tech company manufacturing health products for women. Urška Sršen, co-founder and Chief Creative Officer of Bellabeat, believes that analyzing smart device data can help Bellabeat grow into a larger company.

## Objective

The goal of this project is to analyze non-Bellabeat smart device data and offer insights to improve Bellabeat's marketing strategy.

## Data Source

The dataset used for this analysis is publicly available Fitbit Fitness Tracker Data provided by Mobius on Kaggle: [Fitbit Dataset](https://www.kaggle.com/arashnic/fitbit).

## Data Processing and Analysis

### Required Libraries

```r
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
```

### Load Data

```r
daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight_log <- read.csv("weightLogInfo_merged.csv")
```

### Explore Data

```r
head(daily_activity)
head(sleep_day)
head(weight_log)

colnames(daily_activity)
colnames(sleep_day)
colnames(weight_log)

n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log$Id)

nrow(daily_activity)
nrow(sleep_day)
nrow(weight_log)
```

### Summary Statistics

```r
daily_activity %>% select(TotalSteps, TotalDistance, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, SedentaryMinutes, Calories) %>% summary()
sleep_day %>% select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% summary()
weight_log %>% select(WeightKg, Fat, BMI) %>% summary()
```

### Data Validation

```r
daily_activity_mismatch_distance <- daily_activity %>% filter(TotalDistance != TrackerDistance)
nrow(daily_activity_mismatch_distance)
```

## Key Insights

### Relationship Between Total Steps and Calories Burned

```r
cor(daily_activity$TotalSteps, daily_activity$Calories, use = "complete.obs")

ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Total Steps vs Calories Burned", x = "Total Steps", y = "Calories Burned") +
  theme_minimal()
```
A correlation coefficient of 0.59 is a good positive correlation. This means that more calories burned with more steps taken as expected.  
![totalsteps_vs_caloriesburned](https://github.com/user-attachments/assets/50ba9f5d-df05-47ae-91b4-1b20f52cc12e)

### Activity Type Analysis

```r
activity_types_minutes <- daily_activity %>%
  select(Id, ActivityDate, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  pivot_longer(cols = c(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes),
               names_to = "Activity_type",
               values_to = "Minutes")

activity_types_distance <- daily_activity %>%
  select(Id, ActivityDate, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance) %>%
  pivot_longer(cols = c(VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance),
               names_to = "Activity_type",
               values_to = "Distance")

activity_types_distance <- activity_types_distance %>%
  mutate(Activity_type = case_when(
    Activity_type == "VeryActiveDistance" ~ "VeryActiveMinutes",
    Activity_type == "ModeratelyActiveDistance" ~ "FairlyActiveMinutes",
    Activity_type == "LightActiveDistance" ~ "LightlyActiveMinutes",
    TRUE ~ Activity_type
  ))

activity_combined <- activity_types_minutes %>%
  left_join(activity_types_distance, by = c("Id", "ActivityDate", "Activity_type"))

# Calculate the correlation coefficients for each activity type and calories burnt
cor(
  activity_combined %>% filter(activity_combined$Activity_type == "VeryActiveMinutes") %>% pull(Minutes), 
  activity_combined %>% filter(activity_combined$Activity_type == "VeryActiveMinutes") %>% pull(Distance), 
  use = "complete.obs")

cor(
  activity_combined %>% filter(activity_combined$Activity_type == "FairlyActiveMinutes") %>% pull(Minutes), 
  activity_combined %>% filter(activity_combined$Activity_type == "FairlyActiveMinutes") %>% pull(Distance), 
  use = "complete.obs")

cor(
  activity_combined %>% filter(activity_combined$Activity_type == "LightlyActiveMinutes") %>% pull(Minutes), 
  activity_combined %>% filter(activity_combined$Activity_type == "LightlyActiveMinutes") %>% pull(Distance), 
  use = "complete.obs")
```
### Plot Activity Type vs Distance
```r
ggplot(data = activity_combined, aes(x = Minutes, y = Distance, colour = Activity_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Activity Time vs Distance", x = "Time (Minutes)", y = "Distance (km)") +
  theme_minimal()
```
![activitytime_vs_distance](https://github.com/user-attachments/assets/c7f6bb6f-2886-4f2b-8713-c97e09f002ba)

As expected there is a strong correlation between individual activity and the calories burnt. But the slopes are significantly different. The slope is highest in case of VeryActiveMinutes 
followed by FairlyActiveMinutes and LightlyActiveMinutes. This suggest that VeryActiveMinutes lead to more calories burnt compared to other and also the higher distance travelled.

### Average time spent in different activity
```r
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
```
This generates a summary of different activities time by each user.

### Average time spent in sleep
```r
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
summary(avg_sleep$avg_time_in_bed)
summary(avg_sleep$avg_sleep)
```
As we see the majority of users spent maximum time in bed sleeping. A median of 447.9 minutes (between 7 and 8hrs) spent in bed. A median of 419.1minutes (almost 7hrs) spent in sleep.

### Relation between steps taken and sedentary minutes
```r
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") + 
  labs(title = "Relationship Between Sdentary Minutes and Steps",
       x = "Total Steps",
       y = "Sedentary Minutes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

cor(daily_activity$SedentaryMinutes, daily_activity$TotalSteps, use = "complete.obs")
```
The graph shows a trend line with negative corelation. But this may not be true as we do not see many data point on the line. Interestingly there seems to be 2 different clusters: one above 1000 sedentary minutes and another below 1000 sedentary minutes. These two clusters can indicate days with highly sedentary (above 1000) and less sedentary (below 1000) lifestyle.
![sedentaryminutes_vs_steps](https://github.com/user-attachments/assets/b4975fea-7f1c-4df2-b5f3-f3e0af7cfd40)

### Clustering Based on Sedentary Minutes
We observe two different clusters above and below 1000 SedentaryMinutes. We would like to highlight such clusters and try a manual approach.
```r
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
```
![sedentaryminutes_vs_steps_clusters](https://github.com/user-attachments/assets/fcd726a2-83aa-4e9c-b8b2-d45fff98a373)

```r
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
```
![sedentaryminutes_vs_steps_final_clusters](https://github.com/user-attachments/assets/6d8f543a-49fd-41cf-938c-23886550acfc)


## Conclusion

1. **Steps and Calories:** There is a positive correlation between TotalSteps and Calories burned.
2. **Activity Type Impact:** VeryActiveMinutes have the highest impact on Calories burned, followed by FairlyActive and LightlyActiveMinutes.
3. **Sleep behaviour:** Most users spent their bed time in sleep with approximately 7hrs of sleep daily.
4. **Sedentary Behaviour:** Clustering analysis highlights two segments – day with high and low sedentary activites, which could help target marketing strategies for Bellabeat.

## Future Recommendations

- **Data Enrichment:** Additional data such as calories burnt during each activity can improve the analysis. Information such as age and gender can further help to identify the user behaviour.
- **User Segmentation:** More granular clustering could help identify specific user behaviors and tailor Bellabeat's products accordingly.
- **Marketing Strategy:** Insights from sedentary and active users could be leveraged for targeted marketing campaigns.

## Disclaimer

This analysis was conducted as part of the Bellabeat Data Analytics Case Study for the Google Data Analytics Capstone Project.

## About the author

This project was created by Anuj, a certified data analyst and scientist (biologist). Feel free to connect via  
LinkedIn: https://www.linkedin.com/in/anujanujbioanalyst  
Email: anuj.kaushik.btjj@gmail.com  

