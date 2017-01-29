setwd("C:/Users/conor/githubfolder/pgh-crime")

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")

library(tidyverse)
library(lubridate)
library(viridis)
library(ggmap)

theme_set(theme_nhh())

data <- read_csv("archive-police-blotter.csv") #Read tge data into R
problems(data) #There are 6 rows that could not be processed

df <- data

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
         mday = mday(date)) %>% 
  select(date, #select the columnns we want
         year,
         month,
         wday,
         mday,
         location,
         neighborhood,
         zone,
         description,
         cleared_flag,
         x,
         y)

glimpse(df) #View the data

#Which zone has the most arrests?
df %>%
  group_by(zone) %>% 
  count() %>% 
  arrange(-n)


#How has the number of arrests changed over time?
df %>% 
  filter(zone %in% c(1:6)) %>% 
  group_by(zone, date) %>% 
  count() %>% 
  ggplot(aes(date, n, color = zone, fill = zone)) +
  #geom_point(alpha = .02) +
  geom_smooth() +
  #facet_wrap(~month, ncol = 1) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  coord_cartesian(ylim = c(0, 75))
ggsave("pgh_crime_date_zone_line_plot.png", width = 16, height = 9)

#Looks like there is a data or reporting problem in Zone 6

#Which neighborhood has the most arrests?
df %>%
  group_by(neighborhood) %>% 
  count() %>% 
  arrange(-n)


df %>% 
  filter(zone == c(1:6)) %>% 
  group_by(zone, neighborhood, date) %>% 
  count() %>% 
  arrange(zone, neighborhood, date) %>% 
  mutate(cumsum = cumsum(n)) %>% 
  ggplot(aes(date, cumsum, color = neighborhood, fill = neighborhood)) +
  geom_line() +
  facet_wrap(~zone, ncol = 1) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

#Which arrest descriptions are the most common?
df %>%
  #filter(incident_zone == 2) %>% 
  group_by(description) %>% 
  count() %>% 
  arrange(-n)
#Many NA values

#Have the number of marijuana-related arrests changed over time?
#Create a dataframe
pot_arrests <- df %>%
  filter(grepl("MARIJUANA", description) == TRUE) %>%
  group_by(date) %>% 
  count() 

#Plot the data
ggplot(data = pot_arrests, aes(x = date, y = n)) +
  geom_smooth()

#Let's look at how the data looks on a map
#First, create the map
city_map <-  qmap("North Oakland, Pittsburgh, PA", 
                  zoom = 12,
                  maptype = "toner", 
                  source = "stamen")

#View the map to make sure it looks right
city_map

#Filter out data that is not in one of the six police zones
df_map <- df %>% 
  filter(zone %in% c(1:6))

#Put the data on the map
city_map +
  stat_density_2d(data = df_map, #Using a 2d contour
                  aes(x, #longitude
                      y, #latitude
                      fill = ..level.., #Use the count of arrests as the fill
                      alpha = .5), #Use alpha so you can see the map under the data
                  geom = "polygon") + #We want the contour in a polygon
  #facet_wrap(~wday, nrow = 2) +
  scale_fill_viridis() +
  guides(alpha = FALSE,
         fill = guide_colorbar("Count of Arrests")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

df %>% 
  group_by(mday, wday) %>% 
  count() %>% 
  ggplot(aes(mday, wday, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis()

