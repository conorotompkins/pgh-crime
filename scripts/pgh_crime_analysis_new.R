source("scripts/load_data_new.R")

library(viridis)
library(ggmap)
library(scales)

theme_set(theme_bw(base_size = 18))

glimpse(df) #View the data

df %>% 
  filter(date >= "2016-01-01") -> df

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
#ggsave("images/pgh_crime_date_zone_line_plot.png", width = 16, height = 9)

#Looks like there is a data or reporting problem in Zone 6

#Which neighborhood has the most arrests?
df %>%
  group_by(neighborhood) %>% 
  count() %>% 
  arrange(-n)

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
  count(date)

#Plot the data
ggplot(data = pot_arrests, aes(x = date, y = n)) +
  geom_smooth()

df %>% 
  filter(!is.na(month), !is.na(wday)) %>% 
  count(month, wday) %>% 
  arrange(month, wday) %>% 
  complete(month, wday) %>% 
  replace_na(list(n = 0)) -> df_tile

df_tile %>% 
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
zone_map

#zoom in for facets
city_map_facets <-  ggmap(get_map("North Oakland, Pittsburgh, PA", 
                                  zoom = 12,
                                  maptype = "toner-lite", 
                                  source = "stamen"))

df_map_zones_year <- df %>% 
  select(year, zone, x, y) %>% 
  filter(zone %in% c(1:6)) %>% 
  mutate(zone = as.factor(paste("Zone", zone)))

#facet by zone and year
faceted_zone_map_year <- city_map_facets +
  geom_point(data = df_map_zones_year, aes(x, y, color = zone), alpha = .3, size = 1) +
  facet_grid(zone~year) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
       x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         color = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank(),
        strip.text = element_text(size = 9))
faceted_zone_map_year
ggsave("faceted_zone_map.png", faceted_zone_map_year, width = 16, height = 9)

#neighborhood maps
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

write_csv(df_nbh, "nbh_map_df.csv")

nbh_map <- city_map +
  geom_point(data = df_nbh, aes(x, y, color = neighborhood), alpha = .3, size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
       x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         color = FALSE) +
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

#maps by neighborhood and year
#zoom in for facets
city_map_facets <-  ggmap(get_map("North Oakland, Pittsburgh, PA", 
                                  zoom = 12,
                                  maptype = "toner-lite", 
                                  source = "stamen"))

df_nbh_year <- df %>% 
  filter(neighborhood %in% neighborhoods_top10$neighborhood) %>% 
  select(year, neighborhood, x, y) %>% 
  mutate(neighborhood = factor(neighborhood, levels = neighborhoods_top10$neighborhood))

faceted_nbh_map_year <- city_map_facets +
  geom_point(data = df_nbh_year, aes(x, y, color = neighborhood), alpha = .3, size = 1) +
  facet_grid(neighborhood~year) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Crime Incident Data",
       x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         color = FALSE) +
  theme(axis.text = element_blank(),
        strip.text = element_text(size = 9))
faceted_nbh_map_year
ggsave("faceted_neighborhood_map.png", faceted_nbh_map_year, width = 16, height = 9)
#identify the correct zones for neighborhoods by finding the zones with the highest # of incidents for a neighborhood
df_correct_zone <- df %>% 
  mutate(key = paste(zone, neighborhood)) %>% 
  select(key, zone, neighborhood) %>% 
  group_by(key, zone, neighborhood) %>%
  count() %>% 
  arrange(neighborhood, -n) %>% 
  group_by(neighborhood) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(zone) %>% 
  mutate(correct_zone = zone) %>% 
  select(correct_zone, neighborhood)

#calculate how many of a neighborhood's incidents were reported in the correct zone
df_zones_nbh <- df %>% 
  mutate(key = paste(zone, neighborhood)) %>% 
  select(key, zone, neighborhood) %>% 
  filter(!is.na(zone),
         !is.na(neighborhood),
         !(zone %in% c("OSC", "OUTSIDE")),
         !(neighborhood %in% c("Outside State", "Outside County", "Outside City"))) %>% 
  group_by(key, zone, neighborhood) %>%
  count() %>% 
  left_join(., df_correct_zone) %>% 
  mutate(flag = ifelse(zone == correct_zone, "Correct", "Incorrect")) %>% 
  group_by(zone, neighborhood, flag) %>% 
  summarize(n = sum(n)) %>% 
  spread(key = flag,
         value = n,
         fill = 0) %>% 
  group_by(neighborhood) %>% 
  summarize(Correct = sum(Correct),
            Incorrect = sum(Incorrect),
            count = Correct + Incorrect,
            percent_correct = round(Correct/count, 2)) %>% 
  left_join(., df_correct_zone)

ggplot(df_zones_nbh, aes(count, percent_correct, label = neighborhood, fill = correct_zone)) +
  geom_label() +
  scale_y_continuous(labels = percent) +
  labs(x = "Count of Arrest Incidents",
       y = "Percent Reported in Correct Zone",
       title = "Nieghborhood-Zone Reporting Analysis") +
  guides(fill = guide_legend(title = "Correct Zone"))


#what are the drivers of the incorrect assignments
#create helper factor for neighborhood
df_bad_zones_helper1 <- df %>% 
  mutate(key = paste(zone, neighborhood)) %>% 
  select(key, zone, neighborhood) %>% 
  filter(!is.na(zone),
         !is.na(neighborhood),
         (zone %in% 1:6),
         !(neighborhood %in% c("Outside State", "Outside County", "Outside City"))) %>% 
  group_by(key, zone, neighborhood) %>%
  count() %>% 
  left_join(., df_correct_zone) %>% 
  mutate(flag = ifelse(zone == correct_zone, "Correct", "Incorrect")) %>% 
  group_by(zone, neighborhood, flag) %>% 
  summarize(n = sum(n)) %>% 
  filter(flag == "Incorrect") %>% 
  group_by(neighborhood) %>% 
  summarize(n = sum(n)) %>% 
  arrange(n)

#create helper factor for zone
df_bad_zones_helper2 <- df %>% 
  mutate(key = paste(zone, neighborhood)) %>% 
  select(key, zone, neighborhood) %>% 
  filter(!is.na(zone),
         !is.na(neighborhood),
         (zone %in% 1:6),
         !(neighborhood %in% c("Outside State", "Outside County", "Outside City"))) %>% 
  group_by(key, zone, neighborhood) %>%
  count() %>% 
  left_join(., df_correct_zone) %>% 
  mutate(flag = ifelse(zone == correct_zone, "Correct", "Incorrect")) %>% 
  group_by(zone, neighborhood, flag) %>% 
  summarize(n = sum(n)) %>% 
  filter(flag == "Incorrect") %>% 
  group_by(zone) %>% 
  summarize(n = sum(n)) %>% 
  arrange(-n)

#create df for incorrect assignments
df_bad_zones <- df %>% 
  mutate(key = paste(zone, neighborhood)) %>% 
  select(key, zone, neighborhood) %>% 
  filter(!is.na(zone),
         !is.na(neighborhood),
         (zone %in% 1:6),
         !(neighborhood %in% c("Outside State", "Outside County", "Outside City"))) %>% 
  group_by(key, zone, neighborhood) %>%
  count() %>% 
  left_join(., df_correct_zone) %>% 
  mutate(flag = ifelse(zone == correct_zone, "Correct", "Incorrect")) %>% 
  group_by(zone, neighborhood, flag) %>% 
  summarize(n = sum(n)) %>% 
  filter(flag == "Incorrect") %>% 
  ungroup() %>% 
  mutate(zone = factor(zone, levels = df_bad_zones_helper2$zone),
         neighborhood = factor(neighborhood, levels = df_bad_zones_helper1$neighborhood))

#create heatmap for incorrect assigments
ggplot(df_bad_zones, aes(zone, neighborhood, fill = n)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis() +
  theme(panel.grid = element_blank()) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(x = "Zone",
       y = "Neighborhood",
       title = "Incorrect Neighborhood-Zone Assignments") +
  guides(fill = guide_colorbar("Count of Incidents")) +
  theme(axis.text = element_text(size = 8))