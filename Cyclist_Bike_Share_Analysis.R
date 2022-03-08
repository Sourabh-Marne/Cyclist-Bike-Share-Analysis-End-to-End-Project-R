#ASK PHASE
#Q.1 What is the problem you are trying to solve?
#Ans: Our main aim is to find marketing strategies to convert casual riders into annual members.

#Q.2  How can your insights drive business decisions?
#Ans: Our insights will help the marketing team increase the annual members.

#PREPARE PHASE
#The data is located in a kaggle dataset which is stored in csv files month by month.
#The dataset contains entire population and there is no bias
#The dataset ROCCCs as it is reliable, original, comprehensive, current and cited.

#Installing required packages
install.packages("tidyverse")
install.packages("lubridate")

# Tidyverse is used for data transformation
# Janitor is used for cleaning the data
# Lubridate is used for working with dates and time efficiently

#Loading the libraries
library(tidyverse)
library(lubridate)

#PROCESS PHASE
#Here we will prepare data for data analysis.

#Reading the bikes data from all 12 months
df1 <- read.csv("./CSV files/202004-divvy-tripdata.csv")
df2 <- read.csv("./CSV files/202005-divvy-tripdata.csv")
df3 <- read.csv("./CSV files/202006-divvy-tripdata.csv")
df4 <- read.csv("./CSV files/202007-divvy-tripdata.csv")
df5 <- read.csv("./CSV files/202008-divvy-tripdata.csv")
df6 <- read.csv("./CSV files/202009-divvy-tripdata.csv")
df7 <- read.csv("./CSV files/202010-divvy-tripdata.csv")
df8 <- read.csv("./CSV files/202011-divvy-tripdata.csv")
df9 <- read.csv("./CSV files/202012-divvy-tripdata.csv")
df10 <- read.csv("./CSV files/202101-divvy-tripdata.csv")
df11 <- read.csv("./CSV files/202102-divvy-tripdata.csv")
df12 <- read.csv("./CSV files/202103-divvy-tripdata.csv")

#Binding all the dataframes together
rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
head(rides)

#Removing Duplicates
rides <- rides[!duplicated(rides$ride_id), ]
print(paste("Removed", nrow(rides) - nrow(rides), "duplicated rows"))

##Converting the "started_at" and "ended_at" columns from characters to Timestamps
rides$started_at <- lubridate::ymd_hms(rides$started_at)
rides$ended_at <- lubridate::ymd_hms(rides$ended_at)

#Converting ride time in minutes
rides <- rides %>% mutate(ride_mins = as.numeric(rides$ended_at - rides$started_at) / 60)
summary(rides$ride_mins)

#Separating year and month
rides <- rides %>% mutate(year_month = paste(strftime(rides$started_at, "%Y"),
              "-",strftime(rides$started_at, "%m"),
                 paste("(",strftime(rides$started_at, "%b"), ")", sep="")))
unique(rides$year_month)

#Extracting the week days
rides <- rides %>% mutate(weekday = paste(strftime(rides$ended_at, "%u"), "-", strftime(rides$ended_at, "%a")))
unique(rides$weekday)

#Extracting the ride hour 
rides$hour <- lubridate::hour(rides$ended_at)
unique(rides$hour)

#Saving our result in a CSV file
rides %>% write.csv("bike_rides_clean.csv")

#ANALYZE PHASE

summary(rides)

#Finding number of casuals vs number of annual members
rides %>% group_by(member_casual) %>% summarise(count = length(ride_id),
            'Percentage' = (length(ride_id) / nrow(rides)) * 100)

#Plotting number of casuals vs annuals
ggplot(rides, aes(member_casual, fill=member_casual)) + geom_bar() +
  labs(x="Casuals vs Annual Members", title="Casuals vs Members Distribution")

#Monthly Distribution of the Data
rides %>%
  group_by(year_month) %>%
  summarise(count = length(ride_id),
            'Percentage' = (length(ride_id) / nrow(rides)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% Difference' = members_p - casual_p)

rides %>%
  ggplot(aes(year_month, fill=member_casual)) +
  geom_bar() +
  labs(x="Month", title="Chart 02 - Distribution by month") +
  coord_flip()

#Insights from above chart
#1.There are more riders in 2020
#2.August has the biggest number of data points(18% of the dataset)
#3.We have more members than riders in every month. 

#Lets observe how the temperature affects number of riders
chicago_mean_temp <- c(-3.2, -1.2, 4.4, 10.5, 16.6, 22.2, 24.8, 23.9, 19.9, 12.9, 5.8, -0.3)
month <- c("001 - Jan","002 - Feb","003 - Mar","004 - Apr","005 - May","006 - Jun","007 - Jul",
           "008 - Aug","009 - Sep","010 - Oct","011 - Nov","012 - Dec")

data.frame(month, chicago_mean_temp) %>%
  ggplot(aes(x=month, y=chicago_mean_temp)) +
  labs(x="Month", y="Mean temperature", title="Mean temperature for Chicago (1991-2020)") + geom_col()

#As we can see the temperature highly affects the number of riders in every month
#The colder the weather, the lesser the bike riders

#Week Day Distribution
rides %>%
  group_by(weekday) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(rides)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% Difference' = members_p - casual_p)

ggplot(rides, aes(weekday, fill=member_casual)) +
  geom_bar() +
  labs(x="Weekday", title="Distribution by weekday") +
  coord_flip()

#Insights
#1.The highest voulme of riders is on weekends.
#2.Saturday being the highest.
#3.More casual riders are seen on weekends.

#Hourly Distribution
rides %>%
  group_by(hour) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(rides)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% Difference' = members_p - casual_p)

rides %>%
  ggplot(aes(hour, fill=member_casual)) +
  labs(x="Hour of the day", title="Hourly Distribution of the day") +geom_bar()

#Insights
#1.There are more bikers in the afternoon
#2.There are more members in the morning from 5 am to 11 am.
#3.There are more casuals in the afternoon from 11 pm to 4 am.

#Weekends vs Mid Week Distribution
rides %>%
  mutate(type_of_weekday = ifelse(weekday == '6 - Sat' | weekday == '7 - Sun', 'weekend', 'midweek')) %>%
  ggplot(aes(hour, fill=member_casual)) +
  labs(x="Hour of the day", title="Distribution by hour of the day in the midweek") +
  geom_bar() +
  facet_wrap(~ type_of_weekday)

#Insights
#1. Weekends have mre casuals from 11 am to 6 pm.
#2. There is huge rise in weekdays from 5 pm to 6 pm, then it drops.
#3. On weekends there is smooth flow which rises in the afternoon.

#Type of Rideable bikes
rides %>%
  group_by(rideable_type) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(rides)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% Difference' = members_p - casual_p)

ggplot(rides, aes(rideable_type, fill=member_casual)) +
  labs(x="Rideable type", title="Distribution of types of bikes") + geom_bar() +   coord_flip()

#Insights:
#1. Docked bikes have highest volume of riders.
#2. Members prefer the classic and electric bikes.

#SHARE PHASE

#Key Insights
#1. Members have the biggest proportion of the dataset than casuals.
#2. There are more riders at the last semester of 2020.
#3. August recorded the biggest count of data points which was 18% of the dataset.
#4. Temperature heavily influences the number of rides in each month.
#5. Weekends have highest number of riders.
#6. There are more riders in the afternoon.

#Members vs Casual Riders
#1.Members have highest volume of data, except for saturdays.
#2.Weekends have more casuals than members.
#3.There are more members in the morning from 5am to 11 am.
#4.There are more casuals from 11pm to 4am.
#5.There is an increase from 6am to 8am on weekdays for members. Next big rise is from 5pm to 6pm.
#6.Members prefer classic bikes.
#7.Casuals have more riding time than members.

#Conclusion
#1.Bikes are used as an recreational activity on weekends.
#2. Temperature affects the number of riders.
#3. Members use bikes for daily activities like going to work.


#ACT PHASE
#Now the marketing teams can use these insights for increasing the number of members and converting
#casuals to annual members.
