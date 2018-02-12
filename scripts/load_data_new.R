library(tidyverse)
library(lubridate)

#df <- read_csv("data/archive-police-blotter.csv") #Read the data into R
df <- read_csv("data/pittsburgh_crime_UCR_coded.csv")
problems(data) #There are 6 rows that could not be processed

colnames(df) <- tolower(colnames(df)) #Change all the column names to lower case
colnames(df)
#Rename the column names so they are easier to work with
df %>% 
  rename(date_time = incidenttime,
         location = incidentlocation,
         cleared_flag = clearedflag,
         neighborhood = incidentneighborhood,
         zone = incidentzone,
         description = incidenthierarchydesc,
         tract = incidenttract) -> df

df_ucr <- read_csv("data/ucr_codes.csv")

df %>% 
  left_join(df_ucr) -> df

#dates not parsing correctly
df %>% 
  mutate(date_time = ymd_hms(date_time),
         date = ymd(str_sub(date_time, 1, 10)),
         year = year(date),
         month = month(date, label = TRUE),
         wday = wday(date),
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
         hierarchy,
         hierarchy_description,
         description,
         cleared_flag,
         x,
         y) -> df
