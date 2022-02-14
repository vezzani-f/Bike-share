library(tidyverse)
library(lubridate)
library(ggplot2)
getwd()
setwd("/Users/filippo/Desktop")
october<-read.csv("data_1021.csv")
november<-read.csv("data_1121.csv")
december<-read.csv("data_1221.csv")
# Compare column names each of the files
colnames(october)
colnames(november)
colnames(december)
# Inspect the dataframes and look for incongruencies
str(october)
str(november)
str(december)
# Stack individual month's data frames into one big data frame
q4 <- bind_rows(october, november, december)
colnames(q4)
#Statistical summary of data. Mainly for numeric
summary(q4)
# Add columns that list the date
q4$date <- as.Date(q4$started_at)
# Add a "ride_length" calculation to q4 (in seconds)
q4$ride_length <- difftime(q4$ended_at,q4$started_at)
str(q4)
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(q4$ride_length)
q4$ride_length <- as.numeric(as.character(q4$ride_length))
is.numeric(q4$ride_length)
# Statistical summary of ride_lenght
summary(q4$ride_length)
# Compare members and casual users
aggregate(q4$ride_length ~ q4$member_casual, FUN = mean)
aggregate(q4$ride_length ~ q4$member_casual, FUN = median)
aggregate(q4$ride_length ~ q4$member_casual, FUN = max)
# See the average ride time by each day for members vs casual users
q4$day_of_week <- format(as.Date(q4$date), "%a")
head(q4$day_of_week)
# Days name are in italian, fix the language
q4new <- q4 %>%
  mutate(day_of_week = recode(day_of_week, Dom = "Sun"
                              ,Lun = "Mon"
                              ,Mar = "Tue"
                              ,Mer  = "Wed"
                              ,Gio = "Thu"
                              ,Ven = "Fri"
                              ,Sab = "Sat"))
# Ok, lets'do statistic
aggregate(q4new$ride_length ~ q4new$member_casual + q4new$day_of_week, FUN = mean)
# Notice that the days of the week are out of order. Let's fix that.
q4new$day_of_week <- ordered(q4new$day_of_week, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
# Ok, lets'do statistic
aggregate(q4new$ride_length ~ q4new$member_casual + q4new$day_of_week, FUN = mean)
# Analyze ridership data by type and weekday
q4new %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)	   # sorts
# Let's visualize the number of rides by rider type
q4new %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
# Let's create a visualization for average duration
q4new %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
# Type of bike for type of consumer
ggplot(data = q4new) +
  geom_bar(mapping = aes(x = rideable_type,fill=member_casual))
# Save data
counts <- aggregate(q4new$ride_length ~ q4new$member_casual + q4new$day_of_week, FUN = mean)
write.csv(counts, file = "/Users/filippo/Desktop/fffffffff.csv")











