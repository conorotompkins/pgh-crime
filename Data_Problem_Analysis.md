This is an analysis of potential data problems in the Pittsburgh Police Incident Blotter Archive.

Zone reporting consistency
==========================

Does the Zone the incident is assigned to match the geolocations the incident is reported at?

``` r
city_map_11 <-  get_map(location = "Oakland, Pittsburgh, PA",
               zoom = 11,
               maptype = "toner", 
               source = "stamen",
               messaging = FALSE)

city_map_11 <- ggmap(city_map_11)
```

``` r
df_map_zones <- df %>% 
  filter(zone %in% c(1:6)) %>% 
  select(zone, x, y) %>% 
  mutate(zone = as.factor(paste("Zone:", zone)))
```

In this veiw, the data looks accurate

``` r
city_map_11 +
  geom_point(data = df_map_zones, aes(x, y, color = as.factor(zone)), alpha = .3, size = .7, show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Police Incident Data",
       x = NULL,
       y = NULL) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
```

    ## Warning: Removed 22716 rows containing missing values (geom_point).

![](Data_Problem_Analysis_files/figure-markdown_github/create%20zone%20map-1.png)

``` r
city_map_12 <-  get_map(location = "Oakland, Pittsburgh, PA",
               zoom = 12,
               maptype = "toner", 
               source = "stamen",
               messaging = FALSE)
```

    ## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Oakland,+Pittsburgh,+PA&zoom=12&size=640x640&scale=2&maptype=terrain&sensor=false

    ## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=Oakland,%20Pittsburgh,%20PA&sensor=false

    ## Map from URL : http://tile.stamen.com/toner/12/1137/1542.png

    ## Map from URL : http://tile.stamen.com/toner/12/1138/1542.png

    ## Map from URL : http://tile.stamen.com/toner/12/1139/1542.png

    ## Map from URL : http://tile.stamen.com/toner/12/1137/1543.png

    ## Map from URL : http://tile.stamen.com/toner/12/1138/1543.png

    ## Map from URL : http://tile.stamen.com/toner/12/1139/1543.png

    ## Map from URL : http://tile.stamen.com/toner/12/1137/1544.png

    ## Map from URL : http://tile.stamen.com/toner/12/1138/1544.png

    ## Map from URL : http://tile.stamen.com/toner/12/1139/1544.png

    ## Map from URL : http://tile.stamen.com/toner/12/1137/1545.png

    ## Map from URL : http://tile.stamen.com/toner/12/1138/1545.png

    ## Map from URL : http://tile.stamen.com/toner/12/1139/1545.png

``` r
city_map_12 <- ggmap(city_map_12)
```

One note of concern is that 5% of the data is outside the x,y coordinates in this map

``` r
paste0(round(22716 / nrow(df_map_zones), 2), "%")
```

    ## [1] "0.05%"

However, when you facet the data by Zone to separate it, the data looks less accurate

``` r
city_map_12 +
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
```

    ## Warning: Removed 29005 rows containing missing values (geom_point).

![](Data_Problem_Analysis_files/figure-markdown_github/faceted%20zone%20map-1.png)

Neighborhood reporting consistency
==================================

Does the data for the Neighborhoods look the same?

``` r
neighborhoods_top10 <- df %>% 
  filter(!is.na(neighborhood)) %>% 
  select(neighborhood) %>% 
  group_by(neighborhood) %>% 
  count() %>%
  arrange(-n) %>% 
  mutate(neighborhood = factor(neighborhood)) %>% 
  top_n(n = 10, wt = n)

df_nbh <- df %>% 
  filter(neighborhood %in% neighborhoods_top10$neighborhood) %>% 
  select(neighborhood, x, y) %>% 
  mutate(neighborhood = factor(neighborhood, levels = neighborhoods_top10$neighborhood))
```

Looking at the data from the top 10 Neighborhoods in one map:

``` r
city_map_12 +
  geom_point(data = df_nbh, aes(x, y, color = neighborhood), alpha = .3, size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Police Incident Data",
       x = NULL,
       y = NULL) +
  guides(alpha = FALSE,
         color = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank())
```

    ## Warning: Removed 5032 rows containing missing values (geom_point).

![](Data_Problem_Analysis_files/figure-markdown_github/nbh%20map-1.png)

The borders between Neighborhoods are significantly less clearly delineated than the borders for the Zones were

How does the data look when it is faceted by Neighborhood?

``` r
city_map_12 +
  geom_point(data = df_nbh, aes(x, y, color = neighborhood), alpha = .3, size = 1) +
  facet_wrap(~neighborhood, ncol = 5) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Pittsburgh Police Incident Data",
     x = NULL,
     y = NULL) +
  guides(color = FALSE) +
  theme(axis.text = element_blank(),
        strip.text = element_text(size = 6))
```

    ## Warning: Removed 5032 rows containing missing values (geom_point).

![](Data_Problem_Analysis_files/figure-markdown_github/nbh%20facet%20map-1.png)

There appears to be significant data quality issues with the Neighborhood designations

Many Neighborhoods have incidents reported in multiple zones

``` r
df_zone_nbh <- df %>% 
  filter(zone %in% c(1:6)) %>% 
  group_by(zone, neighborhood) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(neighborhood = factor(neighborhood))
```

``` r
ggplot(df_zone_nbh, aes(zone, reorder(neighborhood, n), fill = n)) +
  geom_tile() +
  facet_wrap(~zone,
             nrow = 1,
             scales = "free_x") +
  coord_equal() +
  labs(x = "Zone",
       y = "Neighborhood",
       title = "Pittsburgh Police Incident Data") +
  guides(fill = guide_colorbar("Count of Incidents")) +
  scale_fill_viridis() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.grid = element_blank(),
        aspect.ratio= 12/1)
```

![](Data_Problem_Analysis_files/figure-markdown_github/zone%20vs%20nbh%20plot-1.png)

what are the drivers of the incorrect assignments?

First, we need to identify the correct zones for neighborhoods

My method is to find the Zone with the highest \# of incidents for a Neighborhood

``` r
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
```

Then, calculate how many of a neighborhood's incidents were reported in the correct zone

``` r
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
```

    ## Joining, by = "neighborhood"
    ## Joining, by = "neighborhood"

``` r
ggplot(df_zones_nbh, aes(count, percent_correct, label = neighborhood, fill = correct_zone)) +
  geom_label(size = 2) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = comma) +
  labs(x = "Count of Arrest Incidents",
       y = "Percent Reported in Correct Zone",
       title = "Nieghborhood-Zone Reporting Analysis") +
  guides(fill = guide_legend(title = "Correct Zone")) +
  theme(axis.title = element_text(size = 10))
```

![](Data_Problem_Analysis_files/figure-markdown_github/correct%20zone%20percentage%20plot-1.png)

Zone 6 appears to have the lowest % of correct Zone assignments. Southside Flats and Golden Triangle/Civic Arena have most of the incorrect assignments

``` r
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
```

    ## Joining, by = "neighborhood"

``` r
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
```

    ## Joining, by = "neighborhood"

``` r
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
```

    ## Joining, by = "neighborhood"

``` r
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
```

![](Data_Problem_Analysis_files/figure-markdown_github/bad%20zones%20plot-1.png)

Brookline has the most incidents reported in the incorrect zone

Zone 3 is a major driver of incorrect Zone assignments

Zone 6
======

Zone 6 was closed in 2003, and reopened in 2008.

<http://www.post-gazette.com/local/neighborhoods/2008/03/31/Police-manpower-tipped-to-West-End-s-Zone-6/stories/200803310145>

Therefore, we should expect that there are 0 incidents between 2005 (when the data begins) and 2008

``` r
df %>%
  filter(zone == 6, year <= 2008) %>% 
  select(zone, date, description) %>% 
  group_by(zone) %>% 
  count()
```

    ## # A tibble: 1 × 2
    ##    zone     n
    ##   <chr> <int>
    ## 1     6  2889

``` r
df %>%
  filter(zone == 6, year <= 2008) %>% 
  select(zone, date, description) %>% 
  head()
```

    ## # A tibble: 6 × 3
    ##    zone       date                  description
    ##   <chr>     <date>                        <chr>
    ## 1     6 2005-07-09                         <NA>
    ## 2     6 2005-10-05                         <NA>
    ## 3     6 2005-10-05               IDENTITY THEFT
    ## 4     6 2005-10-28                         <NA>
    ## 5     6 2005-04-26 INVOLUNTARY DEV SEXUAL INTER
    ## 6     6 2005-03-24                         <NA>

Where did this incidents occur?

``` r
z6_map <-  get_map(location = "Mount Washington, Pittsburgh, PA",
               zoom = 12,
               maptype = "toner", 
               source = "stamen",
               messaging = FALSE)
```

``` r
df_map_z6 <- df %>% 
  filter(zone == 6, year <= 2008)
```

``` r
ggmap(z6_map) +
  stat_density_2d(data = df_map_z6, #Using a 2d contour
                  aes(x, #longitude
                      y, #latitude
                      fill = ..level.., #Use the count of incidents as the fill
                      alpha = .5), #Use alpha so you can see the map under the data
                  geom = "polygon") + #We want the contour in a polygon
  scale_fill_viridis() +
  guides(alpha = FALSE,
         fill = guide_colorbar("Count of Incidents")) +
  labs(x = "",
       y = "",
       title = "Zone 6 Incidents before 2008") +
  theme_nhh() +
  theme(axis.text = element_blank())
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

    ## Warning: `legend.margin` must be specified using `margin()`. For the old
    ## behavior use legend.spacing

    ## Warning: Removed 170 rows containing non-finite values (stat_density2d).

![](Data_Problem_Analysis_files/figure-markdown_github/plot%20map-1.png)

This broadly lines up with the borders of Zone 6

Incidents reported in Zone 6 before Zone 6 was reopened in 2008 appear to be geolocated appropriately
