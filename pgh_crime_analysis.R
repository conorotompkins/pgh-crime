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
  group_by(incident_zone) %>% 
  count() %>% 
  arrange(-n)


df %>% 
  filter(incident_zone %in% c(1:6)) %>% 
  group_by(incident_date, incident_zone) %>% 
  count() %>% 
  ggplot(aes(incident_date, n, color = incident_zone, fill = incident_zone)) +
  geom_point(alpha = .02) +
  geom_smooth() +
  #facet_wrap(~incident_month, ncol = 1) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  coord_cartesian(ylim = c(0, 75))
ggsave("pgh_crime_date_zone_line_plot.png", width = 16, height = 9)

df %>%
  group_by(incident_neighborhood) %>% 
  count() %>% 
  arrange(-n)

df %>% 
  filter(incident_zone %in% c(1:6)) %>% 
  group_by(incident_zone, incident_neighborhood, incident_date) %>% 
  count() %>% 
  ggplot(aes(incident_date, n, color = incident_neighborhood, fill = incident_neighborhood)) +
  geom_line() +
  facet_wrap(~incident_zone, ncol = 1) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)
