setwd("C:/Users/conor/githubfolder/pgh-crime")

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")

library(tidyverse)
library(lubridate)
library(viridis)

theme_set(theme_nhh())

data <- read_csv("archive-police-blotter.csv")
problems(data)

df <- data

colnames(df) <- tolower(colnames(df))

df <- df %>% 
  rename(incident_date_time = incidenttime,
         incident_location = incidentlocation,
         cleared_flag = clearedflag,
         incident_neighborhood = incidentneighborhood,
         incident_zone = incidentzone,
         hierarchy_description = hierarchydesc,
         incident_tract = incidenttract)

df <- df %>%
  mutate(incident_date_time = mdy_hm(incident_date_time),
         incident_date = ymd(substr(incident_date_time, 1, 10)),
         incident_year = year(incident_date),
         incident_month = month(incident_date, label = TRUE),
         incident_wday = wday(incident_date, label = TRUE),
         incident_mday = mday(incident_date)) %>% 
  select(incident_date,
         incident_year,
         incident_month,
         incident_wday,
         incident_mday,
         incident_location,
         incident_neighborhood,
         incident_zone,
         cleared_flag,
         x,
         y)
glimpse(df)


df %>% 
  filter(incident_year != 2017) %>% 
  group_by(incident_date, incident_month) %>% 
  count() %>% 
  ggplot(aes(incident_date, n, color = incident_month, fill = incident_month)) +
  geom_smooth() +
  #facet_wrap(~incident_month, ncol = 1) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)
ggsave("pgh_crime_date_month_line_plot.png", width = 16, height = 9)
