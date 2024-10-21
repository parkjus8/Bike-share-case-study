library(tidyverse)
library(maps)
library(geosphere)
library(leaflet)
library(gridExtra)
library(grid)
library(cowplot)
library(data.table)
setwd("~/Bike_share_CS_data")
oct <- fread("202310-divvy-tripdata.csv")
nov <- fread("202311-divvy-tripdata.csv")
dec <- fread("202312-divvy-tripdata.csv")
jan <- fread("202401_divvy_tripdata.csv")
feb <- fread("202402_divvy_tripdata.csv")
mar <- fread("202403_divvy_tripdata.csv")
apr <- fread("202404_divvy_tripdata.csv")
may <- fread("202405-divvy-tripdata.csv")
jun <- fread("202406-divvy-tripdata.csv")
jul <- fread("202407-divvy-tripdata.csv")
aug <- fread("202408-divvy-tripdata.csv")
sep <- fread("202409-divvy-tripdata.csv")

# Combine datasets with rbindlist (more efficient than rbind)
combined_list <- rbindlist(list(oct, nov, dec, jan, feb, mar, apr, may, jun, jul, aug, sep))

str(combined_list)

# Capitalizing member and casual
combined_list[, member_casual := recode(member_casual, 
                                              "member" = "Member", 
                                              "casual" = "Casual")]

# making ride time column and casting it as numeric data type
combined_list[, ride_time := as.numeric(difftime(ended_at, started_at, 
                                                                 units = "mins"))]


# Creating distance between points column
combined_list[, ride_dist := distHaversine(matrix(c(start_lng, start_lat), ncol = 2), 
                                                  matrix(c(end_lng, end_lat), ncol = 2)) / 1000] 

# Creating start_hour, start_day, and start_month columns
combined_list[, start_hour := as.numeric(format(started_at, "%H"))]

combined_list[, start_day := weekdays(started_at)]
combined_list[, start_day := factor(start_day, 
              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
              ordered = TRUE)]
combined_list[, start_month := format(started_at, "%B")]
combined_list[, start_month := factor(start_month, 
                                      levels = c("January", "February", "March", "April", 
                                                 "May", "June", "July", "August", 
                                                 "September", "October", "November", "December"),
                                      ordered = TRUE)]

# DATA CLEANING
str(combined_list)

sapply(combined_list, function(x) sum(is.na(x))) # checking for na values
combined_list <- na.omit(combined_list) # removing na values

combined_list[duplicated(combined_list), ] # checking for duplicates
combined_list <- unique(combined_list) # removing duplicates

# checking distribution of numeric data points
boxplot(combined_list$ride_time) 
combined_list <- combined_list[ride_time > 0] # removing negative ride times

boxplot(combined_list$ride_dist)
boxplot(combined_list$start_hour)
str(combined_list)

# Monthly Visuals
monthly_uses <- combined_list %>%
  group_by(start_month, member_casual) %>%
  summarize(uses_per_day = n(), .group = 'drop') %>%
  ggplot(aes(x = start_month, y = uses_per_day, color = member_casual, group = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Month", 
       y = "Volume of Riders", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
monthly_uses

monthly_mean_ride_time <- combined_list %>%
  group_by(start_month, member_casual) %>%
  summarize(mean_time = mean(ride_time), .group = "drop") %>%
  ggplot(aes(x = start_month, y = mean_time, color = member_casual, group = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Month", 
       y = "Average Time Spent Riding in Minutes", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
monthly_mean_ride_time

monthly_mean_ride_dist <- combined_list %>%
  group_by(start_month, member_casual) %>%
  summarize(mean_dist = mean(ride_dist), .group = "drop") %>%
  ggplot(aes(x = start_month, y = mean_dist, color = member_casual, group = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Month", 
       y = "Average Ride Distance in Kilometers", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
monthly_mean_ride_dist

# Weekly visuals
weekly_uses <- combined_list %>%
  group_by(start_day, member_casual) %>%
  summarize(uses_per_day = n(), .group = "drop") %>%
  ggplot(aes(x = start_day, y = uses_per_day, color = member_casual, group = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Day", 
       y = "Volume of Riders", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
weekly_uses

weekly_mean_ride_time <- combined_list %>%
  group_by(start_day, member_casual) %>%
  summarize(mean_time = mean(ride_time), .group = "drop") %>%
  ggplot(aes(x = start_day, y = mean_time, color = member_casual, group = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Day", 
       y = "Average Time Spent Riding in Minutes", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
weekly_mean_ride_time

weekly_mean_ride_dist <- combined_list %>%
  group_by(start_day, member_casual) %>%
  summarize(mean_dist = mean(ride_dist), .group = "drop") %>%
  ggplot(aes(x = start_day, y = mean_dist, color = member_casual, group = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Day", 
       y = "Average Ride Distance in Kilometers", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
weekly_mean_ride_dist

# Hourly visuals
hourly_mean_ride_time <- combined_list %>%
  group_by(start_hour, member_casual) %>%
  summarize(mean_time = mean(ride_time), .group = "drop") %>%
  ggplot(aes(x = start_hour, y = mean_time, color = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Hour", 
       y = "Average Time Spent Riding in Minutes", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
hourly_mean_ride_time

hourly_uses <- combined_list %>%
  group_by(start_hour, member_casual) %>%
  summarize(uses_per_day = n(), .group = "drop") %>%
  ggplot(aes(x = start_hour, y = uses_per_day, color = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Hour", 
       y = "Volume of Riders", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 0:23,
                     labels = sprintf("%02d:00", 0:23)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
hourly_uses

hourly_mean_ride_dist <- combined_list %>%
  group_by(start_hour, member_casual) %>%
  summarize(mean_dist = mean(ride_dist), .group = "drop") %>%
  ggplot(aes(x = start_hour, y = mean_dist, color = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Hour", 
       y = "Average Ride Distance in Kilometers", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
hourly_mean_ride_dist

# weekend and weekday hourly uses stuff
weekday_uses <- combined_list %>%
  filter(start_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  group_by(start_hour, member_casual) %>%
  summarize(uses_per_day = n(), .group = "drop") %>%
  ggplot(aes(x = start_hour, y = uses_per_day, color = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Hour", 
       y = "Volume of Riders", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 0:23,
                     labels = sprintf("%02d:00", 0:23)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
weekday_uses

weekend_uses <- combined_list %>%
  filter(start_day %in% c("Saturday", "Sunday")) %>%
  group_by(start_hour, member_casual) %>%
  summarize(uses_per_day = n(), .group = "drop") %>%
  ggplot(aes(x = start_hour, y = uses_per_day, color = member_casual)) + 
  geom_line(size = 1.2) +
  labs(x = "Hour", 
       y = "Volume of Riders", color = "Rider Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 0:23,
                     labels = sprintf("%02d:00", 0:23)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
weekend_uses

grid.arrange(weekday_uses + theme(legend.position = "none") +
               labs(title = "Weekdays"),
             weekend_uses + labs(title = "Weekends"),
             ncol = 2,
             widths = c(2, 3))

# comparing monthly, weekly, and hourly
grid.arrange(monthly_uses + theme(legend.position = "none"), 
             weekly_uses,
             ncol = 2,
             widths = c(2, 3))

# comparing weekly average ride time vs ride distance
casual_mean_ride_time <- combined_list[member_casual == "Casual", 
                                       mean(ride_time)]
member_mean_ride_time <- combined_list[member_casual == "Member", 
                                       mean(ride_time)]
casual_mean_ride_dist <- combined_list[member_casual == "Casual", 
                                       mean(ride_dist)]
member_mean_ride_dist <- combined_list[member_casual == "Member", 
                                       mean(ride_dist)]

grid.arrange(weekly_mean_ride_time + 
               theme(legend.position = "none") +
               geom_hline(yintercept = casual_mean_ride_time, linetype = 'dotted') +
               geom_hline(yintercept = member_mean_ride_time, linetype = 'dotted'),
             weekly_mean_ride_dist +
               geom_hline(yintercept = casual_mean_ride_dist, linetype = 'dotted') +
               geom_hline(yintercept = member_mean_ride_dist, linetype = 'dotted'),
             ncol = 2,
             widths = c(2, 3))
c(casual_mean_ride_time, member_mean_ride_time)
c(casual_mean_ride_dist, member_mean_ride_dist)
# Proportional Bike usage by rider type
bike_usage <- combined_list %>% 
  group_by(member_casual, rideable_type) %>%
  summarize(count = n()) %>% 
  ungroup() %>%
  group_by(member_casual) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(bike_usage, aes(x = 1, y = percentage, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~member_casual) +
  coord_polar("y", start = 0) +              # Transform bar chart into pie chart
  theme_void() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +                                     # Clean, void theme for pie chart
  labs(title = "Proportional Bike Usage by Rider Type",
       fill = "Rideable Type")
str(combined_list)
