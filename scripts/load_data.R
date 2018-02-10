library(tidyverse)
library(lubridate)


df <- read_csv("data/archive-police-blotter.csv") #Read the data into R
problems(data) #There are 6 rows that could not be processed

colnames(df) <- tolower(colnames(df)) #Change all the column names to lower case

#Rename the column names so they are easier to work with
df <- df %>% 
  rename(date_time = incidenttime,
         location = incidentlocation,
         cleared_flag = clearedflag,
         neighborhood = incidentneighborhood,
         zone = incidentzone,
         description = hierarchydesc,
         tract = incidenttract)


df <- df %>%
  mutate(date_time = mdy_hm(date_time), #Create dates and times with Lubridate
         date = ymd(substr(date_time, 1, 10)),
         year = year(date),
         month = month(date, label = TRUE),
         wday = wday(date, label = TRUE),
         mday = mday(date),
         hour = hour(date_time)) %>% 
  select(date, #select the columnns we want
         year,
         month,
         wday,
         mday,
         hour,
         location,
         neighborhood,
         zone,
         description,
         cleared_flag,
         x,
         y)
