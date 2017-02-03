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

df <- df %>%
  mutate(wday = factor(wday, levels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")))

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

df %>% 
  group_by(month, wday) %>% 
  count() %>% 
  ggplot(aes(month, wday, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0),
                   limits = rev(levels(df$wday))) +
  scale_fill_viridis()


#Let's look at how the data looks on a map
#First, create the map
city_map <-  get_map("North Oakland, Pittsburgh, PA", 
                  zoom = 12,
                  maptype = "toner", 
                  source = "stamen")

#View the map to make sure it looks right
ggmap(city_map)

city_map <- ggmap(city_map)

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

df_map_zones <- df %>% 
  filter(zone %in% c(1:6)) %>% 
  select(zone, x, y) %>% 
  mutate(zone = as.factor(paste("Zone:", zone)))

faceted_zone_map <- city_map +
  geom_point(data = df_map_zones, aes(x, y, color = zone), alpha = .3, size = 1) +
  facet_wrap(~zone, nrow = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
        x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         color = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
faceted_zone_map
ggsave("faceted_zone_map.png", faceted_zone_map, width = 16, height = 9)


zone_map <- city_map +
  geom_point(data = df_map_zones, aes(x, y, color = as.factor(zone)), alpha = .3, size = .7) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
       x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         fill = guide_colorbar("Count of Arrests")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
  

neighborhoods_top10 <- df %>% 
  filter(!is.na(neighborhood)) %>% 
  select(neighborhood, x, y) %>% 
  group_by(neighborhood) %>% 
  count() %>%
  arrange(-n) %>% 
  mutate(neighborhood = factor(neighborhood)) %>% 
  top_n(n = 10, wt = n)

df_nbh <- df %>% 
  filter(neighborhood %in% neighborhoods_top10$neighborhood) %>% 
  select(neighborhood, x, y) %>% 
  mutate(neighborhood = factor(neighborhood, levels = neighborhoods_top10$neighborhood))

nbh_map <- city_map +
  geom_point(data = df_nbh, aes(x, y, color = neighborhood), alpha = .3, size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
       x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         fill = guide_colorbar("Count of Arrests")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
nbh_map

nbh_map_faceted <- city_map +
  geom_point(data = df_nbh, aes(x, y, color = neighborhood), alpha = .3, size = 1) +
  facet_wrap(~neighborhood, ncol = 5) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
     x = NULL,
     y = NULL) +
  guides(alpha = FALSE,
         fill = guide_colorbar("Count of Arrests")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
nbh_map_faceted
#there appears to be a pattern in the error in the data. looks like the same arrests could have been reported across multiple neighborhoods. I'm assuming each incident is unique.

write_csv(df_nbh, "nbh_map_df.csv")


