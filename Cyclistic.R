#Load the libraries to be used in cleaning the dataset
library (tidyverse)
library (ggplot2)
library (lubridate)
library (dplyr)
library (readr)
library (janitor)
library (data.table)
library (tidyr)

#Set working directory
setwd("C:/Users/Owner/Desktop/R/Project")

#Check workin directory
getwd()

#Read csv into RStudio from working directory
jan22 <- read_csv ("202201-divvy-tripdata.csv", show_col_types = FALSE)
feb22 <- read_csv ("202202-divvy-tripdata.csv", show_col_types = FALSE)
march22 <- read_csv ("202203-divvy-tripdata.csv", show_col_types = FALSE)
april22 <- read_csv ("202204-divvy-tripdata.csv", show_col_types = FALSE)
may22 <- read_csv ("202205-divvy-tripdata.csv", show_col_types = FALSE)
june22 <- read_csv ("202206-divvy-tripdata.csv", show_col_types = FALSE)
july22 <- read_csv ("202207-divvy-tripdata.csv", show_col_types = FALSE)
aug22 <- read_csv ("202208-divvy-tripdata.csv", show_col_types = FALSE)
sep22 <- read_csv ("202209-divvy-tripdata.csv", show_col_types = FALSE)
oct22 <- read_csv ("202210-divvy-tripdata.csv", show_col_types = FALSE)
nov22 <- read_csv ("202211-divvy-tripdata.csv", show_col_types = FALSE)
dec22 <- read_csv ("202212-divvy-tripdata.csv", show_col_types = FALSE)

#Check the structure of all the dataset
str(jan22)
str(feb22) 
str(march22) 
str(april22) 
str(may22) 
str(june22) 
str(july22) 
str(aug22) 
str(sep22) 
str(oct22) 
str(nov22) 
str(dec22) 
#All the structure of the dataset are the same and we can procede to merging the dataset

#Merge all the dataset into one using the function 'bind rows'
all_trips <- bind_rows (jan22, feb22, march22, april22, may22, june22, july22, aug22, sep22, oct22, nov22, dec22)

#Check new dataset
glimps(all_trips)

#Rename some column names
all_trips <- all_trips %>%
  rename (ride_type = rideable_type,
   customer_type = member_casual)

#Check dataset
glimpse(all_trips)

#Cleaning the dataset
#Check for NA values
colSums(is.na(all_trips))

#Remove NA values and assign it to a new dataset
all_trips_new <- drop_na (all_trips)

#remove duplicate id
all_trips_new <- all_trips_new [!duplicated(all_trips_new$ride_id), ]

#Add new columns that can be used for aggregate function
all_trips_new$hour <- format(as.POSIXct (all_trips_new$started_at), format = "%H")
all_trips_new$hour <- as.numeric(all_trips_new$hour)

#column for day of the week the trip started
all_trips_new$day_of_the_week <- format (as.Date(all_trips_new$started_at), '%a')  

#column for month when the trip started
all_trips_new$month <- format(as.Date(all_trips_new$started_at), '%b_%y')

#Column for day when the trip started and ended. Extract date element from start_at and ended_at
all_trips_new$start_date <- as.Date (all_trips_new$started_at)
all_trips_new$end_date <- as.Date (all_trips_new$ended_at)

#Extract time element
all_trips_new$start_time <- format(as.POSIXct (all_trips_new$started_at), format = "%H : %M:%S")
all_trips_new$end_time <- format(as.POSIXct (all_trips_new$ended_at), format = "%H : %M:%S")

#Check dataset
glimpse(all_trips_new)

#Column for trip duration in minutes
all_trips_new$trip_duration <- (as.double(difftime(all_trips_new$ended_at, all_trips_new$started_at)))/60

#Checking for trip lengths less than 0
nrow(subset(all_trips_new, trip_duration < 0))

#Checking for test rides that was made by company for quality checks
nrow(subset(all_trips_new, start_station_name %like% "TEST")) 
nrow(subset(all_trips_new, start_station_name %like% "Test")) 
nrow(subset(all_trips_new, start_station_name %like% "test")) 

#Remove negative trip durations
all_trips_new <- all_trips_new [!(all_trips_new$trip_duration < 0), ]

#Remove test rides
all_trips_new <- all_trips_new [!((all_trips_new$start_station_name %like% "TEST" |
 all_trips_new$start_station_name %like% "Test"| 
   all_trips_new$start_station_name %like% "test")),]

#Make sure customer type has two distinct values
table(all_trips_new$customer_type)

#Aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips_new, sum), c("customer_type", "total_trip_duration(mins) "))

glimpse(all_trips_new)
#Everything looks good

#Export to directory
write.csv(all_trips_new, "all_trips.csv")
