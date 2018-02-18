
pgh-crime
=========

This is a repo for analyzing crime data in Pittsburgh PA

``` r
knitr::opts_chunk$set(echo = TRUE)
```

``` r
source("scripts/load_data_new.R")
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.2.0
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    ## Parsed with column specification:
    ## cols(
    ##   `_id` = col_integer(),
    ##   PK = col_integer(),
    ##   CCR = col_integer(),
    ##   HIERARCHY = col_integer(),
    ##   INCIDENTTIME = col_datetime(format = ""),
    ##   INCIDENTLOCATION = col_character(),
    ##   CLEAREDFLAG = col_character(),
    ##   INCIDENTNEIGHBORHOOD = col_character(),
    ##   INCIDENTZONE = col_character(),
    ##   INCIDENTHIERARCHYDESC = col_character(),
    ##   OFFENSES = col_character(),
    ##   INCIDENTTRACT = col_integer(),
    ##   COUNCIL_DISTRICT = col_integer(),
    ##   PUBLIC_WORKS_DIVISION = col_integer(),
    ##   X = col_double(),
    ##   Y = col_double()
    ## )

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 2)

    ## Warning: 1 parsing failure.
    ## row # A tibble: 1 x 5 col     row col   expected               actual file                           expected   <int> <chr> <chr>                  <chr>  <chr>                          actual 1 65694 CCR   no trailing characters .19743 'data/pittsburgh_crime_UCR_co… file # A tibble: 1 x 5

    ## Parsed with column specification:
    ## cols(
    ##   hierarchy = col_integer(),
    ##   hierarchy_description = col_character()
    ## )

    ## Joining, by = "hierarchy"

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(ggmap)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:viridis':
    ## 
    ##     viridis_pal

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
theme_set(theme_bw(base_size = 18))
```

``` r
city_map <-  get_map("North Oakland, Pittsburgh, PA", 
                     zoom = 12,
                     maptype = "toner", 
                     source = "stamen")

city_map <- ggmap(city_map)
```

``` r
#Filter out data that is not in one of the six police zones
df_map <- df %>% 
  filter(zone %in% c(1:6), date >= "2005-01-01")

date_first <- first(df_map$date)
date_last <- last(df_map$date) 

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
  labs(title = str_c("Arrests in Pittsburgh, ", date_first, " to ", date_last),
       x = "",
       y = "") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_blank(),
        legend.key.width = unit(.1, "npc"))
```

![](README_files/figure-markdown_github/plot_crime-1.png)
