#IMPORTING PACKAGES
library(tidyverse) 
# Use the conflicted package to manage conflicts
library(conflicted)
# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(dplyr)
library(data.table)
#Importing data set
q1_2022 <- read_csv("Q1 2022.csv")
q1_2023 <- read_csv("Q1 2023.csv")
q2_2022 <- read_csv("Q2 2022.csv")
q2_2023 <- read_csv("Q2 2023.csv")
q3_2022 <- read_csv("Q3 2022.csv")
q3_2023 <- read_csv("Q3 2023.csv")

#Wrangling data and combing datasets into 1 dataset
#checking to see all have same column names
colnames(q1_2022)
colnames(q1_2023)
colnames(q2_2022)
colnames(q2_2023)
colnames(q3_2022)
colnames(q3_2023)

#Inspecting the dataframes for discrepancies
str(q1_2022)
str(q1_2023)
str(q2_2022)
str(q2_2023)
str(q3_2022)
str(q3_2023)

#combining datasets
all_cyclists<-bind_rows(q1_2022, q1_2023,q2_2022, q2_2023, q3_2022, q3_2023)

#checking GPS location
# Assuming all_cyclists is your data frame
result <- all_cyclists %>%
  filter(!is.na(start_station_name)) %>%  # Filter out rows where start_station_name is NULL
  group_by(start_station_name, start_lat) %>%  # Group by start_station_name and start_lat
  summarise(count = n()) %>%  # Count the number of rows for each group
  arrange(start_station_name)  # Arrange by start_station_name 
# View the result
print(result)

result1 <- all_cyclists %>%
  filter(!is.na(end_station_name)) %>%  # Filter out rows where start_station_name is NULL
  group_by(end_station_name, end_lat) %>%  # Group by start_station_name and start_lat
  summarise(count = n()) %>%  # Count the number of rows for each group
  arrange(end_station_name)  # Arrange by start_station_name 
# View the result
print(result1)


all_cyclists %>%
  mutate(
    started_at = as.POSIXct(start_time),
    ended_at = as.POSIXct(end_time),
    ride_duration_min = round(difftime(end_time, start_time, units = "mins"))
) 



df <- all_cyclists
cat('Total rows before dropping blanks: ', nrow(df))

df <- df %>%
  drop_na()
cat('Total rows after dropping blanks: ', nrow(df))
 

str(df)

df$ride_duration_min = round(difftime(df$end_time, df$start_time, units = "mins"))
print(df)

df_min_max_time <- df %>% 
  select(member_casual, ride_duration_min) %>% 
  group_by(member_casual) %>%
  summarize("max_ride_ride_duration (min)" = max(ride_duration_min), "min_ride_duration (min)" = min(ride_duration_min))

df_min_max_time

df_day_passes <- df %>% 
  select(member_casual, ride_duration_min) %>% 
  group_by(member_casual) %>% 
  filter(ride_duration_min >= 1440) %>% 
  summarize(day_passes = n())

df_day_passes

df_avg_time <- df %>% drop_na() %>%
  group_by(member_casual) %>%
  summarize(average_ride_duration_mins = mean(ride_duration_min))

df_avg_time

ggplot(data = df_avg_time) + geom_col(mapping = aes(x = member_casual, y = average_ride_duration_mins, fill = member_casual)) + 
  labs(title = "Average Ride Duration", 
       subtitle = "Average ride durations by different members", 
       x = "Membership types", 
       y = "Average ride duration (min)") + 
  theme_classic()

df_monthly_count <- df %>% 
  group_by(month = lubridate::month(start_time, label = TRUE), member_casual) %>% 
  summarize(no_of_rides = n())

monthly_ride_plot <- ggplot(df_monthly_count) +
  geom_col(mapping = aes(x = month, y = no_of_rides),
           position = "dodge") + labs(title = "Monthly Ride Count", subtitle = "Monthly rides made by different membership types", x = "No. of rides", y = "Months") + 
  theme_classic()

monthly_ride_plot

# Popular start stations for member types
df_routes_started <- df %>% 
  count(member_casual, start_station_name, sort = TRUE) %>% 
  head(10)

routes_plot_start <- ggplot(df_routes_started) + geom_bar(mapping = aes(y = start_station_name, x = n, fill = member_casual), stat = "identity") + 
  theme_light() + labs(x = "No. of trips ended", y = "start station name")

routes_plot_start

# Popular end stations for member types
df_routes_ended <- df %>% 
  count(member_casual, end_station_name, sort = TRUE) %>% 
  head(10)

routes_plot_end <- ggplot(df_routes_ended) + geom_bar(mapping = aes(y = end_station_name, x = n, fill = member_casual), stat = "identity") + 
  theme_light() + labs(x = "No. of trips ended", y = "end station name")

routes_plot_end

df_bike_choice <- df %>% 
  select(member_casual, rideable_type) %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(count_of_bike = n())

ggplot(data = df_bike_choice) + geom_col(mapping = aes(x = rideable_type, y = count_of_bike,
                                                       fill = member_casual), position = "dodge") + 
  theme_classic() + labs(title = "Choice of Bikes", 
                       subtitle = "Choice of bike types between rider types",
                       x = "Type of bikes", y = "No. of rider types using bike")

#cleaning and transforming data
## converting ridelength into seconds
all_cyclists$ride_length <- difftime(all_cyclists$end_time,all_cyclists$start_time)
##checking new columns
str(all_cyclists)

#PERFORMING CALCULATIONS

##calculating no. of stations
all_cyclists %>% count(start_station_name,start_station_id) 
#calculating no. of bike_types
all_cyclists %>% count(rideable_type)
#Riders from different stations
all_cyclists %>% count(ride_id, start_station_id) %>% filter(n<2)
#calculating no. of casual and members
all_cyclists %>% count(member_casual) 
#Do riders drop of at the starting station?
all_cyclists[all_cyclists$start_station_name == all_cyclists$end_station_name, c("ride_id","start_station_id","end_station_id")]
#repeated customers
all_cyclists %>%count(all_cyclists$ride_id)
all_cyclists %>%n_distinct(all_cyclists$ride_id)

# analyze ridership data by type and weekday
all_cyclists %>% 
  mutate(weekday = lubridate::wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(weekday,member_casual) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),#calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(weekday,member_casual)

# analyze rideable_type data by weekday and members
all_cyclists %>% 
  mutate(weekday = lubridate::wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(weekday,member_casual,rideable_type) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),#calculates the number of rides and average duration
            average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(weekday,member_casual,rideable_type)


#Descriptive analysis on ride_length
mean(all_cyclists$ride_length) #average 
median(all_cyclists$ride_length) #midpoint number in the ascending array of ride lengths
max(all_cyclists$ride_length) #longest ride
min(all_cyclists$ride_length) #shortest ride

aggregate(all_cyclists$ride_length ~ all_cyclists$member_casual, FUN = mean)
aggregate(all_cyclists$ride_length ~ all_cyclists$member_casual, FUN = median)
aggregate(all_cyclists$ride_length ~ all_cyclists$member_casual, FUN = max)
aggregate(all_cyclists$ride_length ~ all_cyclists$member_casual, FUN = min)

# average ride time by each day for members vs casual users
aggregate(all_cyclists$ride_length ~ all_cyclists$member_casual + all_cyclists$day_of_week, FUN = mean)

# visualizing the number of rides by rider type
all_cyclists %>% 
  mutate(day_of_week = lubridate::wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# visualizing average duration
all_cyclists %>% 
  mutate(weekday = lubridate::wday(start_time, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = average_duration, y = weekday, fill = member_casual)) +
  geom_col(position = "dodge")