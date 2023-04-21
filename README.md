# Cyclystic-Project

**Introduction**

For this analysis, I will be working as a junior data analyst with a team of data anlyst at a fictional company named 'Cyclistic' to answer key business questions. 
I will also follow the steps of the data analysis process: ask, prepare, process, analyze, share,  and act


######**About the Company**

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a 
network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system  anytime. 
Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things 
possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes 
are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.


######**Business Task**

The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, my team and I will have to understand 
how casual riders and annual members use Cyclistic bikes differently by analyzing the Cyclistic historical bike trip data to identify trends. From these insights, my team 
and I will design a new marketing strategy to convert casual riders into annual members.


**Data Source**

**This data was collected using sensors at the different docking stations as the bikes can be unlocked from one station and returned to any other station in the system 
anytime. The dataset has the following structure:**

  ..   ride_id = col_character(),
  
  ..   rideable_type = col_character(),
  
  ..   started_at = col_datetime(format = ""),
  
  ..   ended_at = col_datetime(format = ""),
  
  ..   start_station_name = col_character(),
  
  ..   start_station_id = col_character(),
  
  ..   end_station_name = col_character(),
  
  ..   end_station_id = col_character(),
  
  ..   start_lat = col_double(),
  
  ..   start_lng = col_double(),
  
  ..   end_lat = col_double(),
  
  ..   end_lng = col_double(),
  
  ..   member_casual = col_character()
  
** The dataset was uploaded as a zipped file on Bitbucket and can be found here** [link] (https://divvy-tripdata.s3.amazonaws.com/). 


**Data Cleaning Process**

I chose R for the cleaning of this dataset as it has millions of rows. I used RGui but RStudio cloud or RStudio desktop works perfectly fine.
Before starting, install all the packages that will be needed for the analysis. Since I have them alredy installed, I will go straight to loading the libraries.

library (tidyverse)

library (ggplot2)

library (lubridate)

library (dplyr)

library (readr)

library (janitor)

library (data.table)

library (tidyr)


**After calling the libraries, I set my working directory**

setwd("C:/Users/Owner/Desktop/R/Project")

**I checked working directory to be sure it was ready**

getwd()

**My analysis will be using all the data collected in 2022. I read them into R using the >readr package that had been earlier loaded**

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

**After reading all the dataset into R, I checked the structure usin the function >str to make sure they are all the same. This is very important because R will not merge the dataset if they 
have different structures.**

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



**The structure of the dataset are thesame. Now, I can proceed to merge all the dataset using the function >bind_rows**

all_trips <- bind_rows (jan22, feb22, march22, april22, may22, june22, july22, aug22, sep22, oct22, nov22, dec22)



**Check new dataset**

glimps(all_trips)

**Some of the column names were unnecesarily long. So, I renamed these columns using the >rename function**

all_trips <- all_trips %>%
  rename (ride_type = rideable_type,
          customer_type = member_casual)

**I check dataset to make sure these columns were renamed properly**

glimpse(all_trips)

**I went ahead to check for NA values using the is.na function in R. I also used the >ColSums to sum the number of missing value**

colSums(is.na(all_trips))

**I discovered about ten thousand missing values. I proceeded to use the >drop_na function to remove the rows with missing values and saved/assigned the dataset to a new df:**

all_trips_new <- drop_na (all_trips)

**Also, I went ahead to remove duplicate values by ride_id using the function >duplicated and assigned it to the df 'all_trips_new'**

all_trips_new <- all_trips_new [!duplicated(all_trips_new$ride_id), ]

**To help me aggregate, I add new columns. I first extracted 'hour' fro started_at and then converted it to a numeric value**

all_trips_new$hour <- format(as.POSIXct (all_trips_new$started_at), format = "%H")

all_trips_new$hour <- as.numeric(all_trips_new$hour)

**Then I extracted day_of_the_week**

all_trips_new$day_of_the_week <- format (as.Date(all_trips_new$started_at), '%a')  

**I also extracted the month of the year and assigned it to the df 'month'**

all_trips_new$month <- format(as.Date(all_trips_new$started_at), '%b_%y')

**Column for day when the trip started and ended. Extract date element from start_at and ended_at and assing it to the df start_date and end_date respectively**

all_trips_new$start_date <- as.Date (all_trips_new$started_at)

all_trips_new$end_date <- as.Date (all_trips_new$ended_at)

**I extracted time element from both startd_at and ended_at**

all_trips_new$start_time <- format(as.POSIXct (all_trips_new$started_at), format = "%H : %M:%S")

all_trips_new$end_time <- format(as.POSIXct (all_trips_new$ended_at), format = "%H : %M:%S")

**To get the the trip_duration in minutes, I looked for the difference between ended_at ans started_at using the function 'difftime', divide it by 60 and assigned it to the df 'trip_duration' in the format >as.double**

all_trips_new$trip_duration <- (as.double(difftime(all_trips_new$ended_at, all_trips_new$started_at)))/60

**After creating the column 'trip_duration', I checked for trip lengths less than 0**

nrow(subset(all_trips_new, trip_duration < 0))

**I also checked for test rides that were made by company for quality checks using the function 'subset' combined with >nrow to return the number of rows that meets the condition**

nrow(subset(all_trips_new, start_station_name %like% "TEST")) 

nrow(subset(all_trips_new, start_station_name %like% "Test"))

nrow(subset(all_trips_new, start_station_name %like% "test")) 

**I removed trip_durations whose trip length was negative using the operator >!**

all_trips_new <- all_trips_new [!(all_trips_new$trip_duration < 0), ]

**After finding out the test rides that were made by company for quality checks, I went ahead to remove them**

all_trips_new <- all_trips_new [!((all_trips_new$start_station_name %like% "TEST" |
  all_trips_new$start_station_name %like% "Test"| 
    all_trips_new$start_station_name %like% "test")),]

**I made sure to check that customer type has two distinct values using the >table function to classify them**

table(all_trips_new$customer_type)

**I aggregated total trip duration by customer type. (Thisprocess was not important to me since I wont be using R to visualize)**

setNames(aggregate(trip_duration ~ customer_type, all_trips_new, sum), c("customer_type", "total_trip_duration(mins) "))

glimpse(all_trips_new)

**Everything looks good. I proceeded to export my data to my working directory in csv format using the >write function**

write.csv(all_trips_new, "all_trips.csv")
