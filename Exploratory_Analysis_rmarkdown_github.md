Exploratory Analysis
--------------------

The Western Pennsylvania Regional Data Center (<http://www.wprdc.org/>) is a great resource for data about Pittsburgh

They have published an archive of crime incident data from 2005-2015

You can download the data here: <https://data.wprdc.org/dataset/uniform-crime-reporting-data>

We will be using the following R packages for this analysis

-   tidyverse
-   lubridate
-   viridis
-   ggmap

Install these packages using the following code

``` r
install.packages(c("tidyverse", "lubridate", "viridis", "ggmap"))
```

Then load the packages

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(viridis)
library(ggmap)
```

Set your working directory

<http://rprogramming.net/set-working-directory-in-r/>

Then, read the data into R

``` r
df <- read_csv("archive-police-blotter.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   PK = col_integer(),
    ##   CCR = col_integer(),
    ##   HIERARCHY = col_integer(),
    ##   INCIDENTTIME = col_character(),
    ##   INCIDENTLOCATION = col_character(),
    ##   CLEAREDFLAG = col_character(),
    ##   INCIDENTNEIGHBORHOOD = col_character(),
    ##   INCIDENTZONE = col_character(),
    ##   HIERARCHYDESC = col_character(),
    ##   OFFENSES = col_character(),
    ##   INCIDENTTRACT = col_integer(),
    ##   X = col_double(),
    ##   Y = col_double()
    ## )

    ## Warning: 6 parsing failures.
    ##    row col               expected  actual
    ##  26010 CCR no trailing characters .19743 
    ##  47848 CCR no trailing characters -78692 
    ##  96952 CCR no trailing characters -46945 
    ## 233211 CCR no trailing characters  89837 
    ## 307276 CCR no trailing characters  024874
    ## ...... ... ...................... .......
    ## See problems(...) for more details.

readr::read\_csv tells us how it interpreted the column classes, and alerted us that it was unable to load 6 rows from the data

Next, we will change the column names so they are easier to work with

``` r
colnames(df) <- tolower(colnames(df))
```

Then, rename the column names

``` r
df <- df %>% 
  rename(date_time = incidenttime,
         location = incidentlocation,
         cleared_flag = clearedflag,
         neighborhood = incidentneighborhood,
         zone = incidentzone,
         description = hierarchydesc,
         tract = incidenttract)
```

Next, we will use lubridate to change the relevant columns to dates and times

``` r
df <- df %>%
  mutate(date_time = mdy_hm(date_time),
         date = ymd(substr(date_time, 1, 10)),
         year = year(date),
         month = month(date, label = TRUE),
         wday = wday(date, label = TRUE),
         mday = mday(date))
```

Now we will reorder the columns and select the ones we want to work with

``` r
df <- df %>% 
  select(date, 
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
```

Let's see how the data look now

``` r
glimpse(df)
```

    ## Observations: 495,251
    ## Variables: 12
    ## $ date         <date> 2005-01-01, 2005-01-03, 2005-01-03, 2005-01-05, ...
    ## $ year         <dbl> 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2...
    ## $ month        <ord> Jan, Jan, Jan, Jan, Jan, Mar, Mar, Mar, Mar, Mar,...
    ## $ wday         <ord> Sat, Mon, Mon, Wed, Wed, Wed, Wed, Wed, Wed, Wed,...
    ## $ mday         <int> 1, 3, 3, 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 4, 8, 5, 3...
    ## $ location     <chr> "5400 Block NORTHUMBERLAND ST PITTSBURGH, PA", "P...
    ## $ neighborhood <chr> "Squirrel Hill North", "Squirrel Hill North", "Sq...
    ## $ zone         <chr> "4", "4", "3", "1", "4", "2", "5", "1", "2", "1",...
    ## $ description  <chr> "MOTOR THEFT (OTHER)-PLATE ONLY", "MTR VEH THEFT ...
    ## $ cleared_flag <chr> "N", "N", "N", "N", "N", "Y", "N", "N", "Y", "N",...
    ## $ x            <dbl> -79.93403, 0.00000, 0.00000, -80.02433, 0.00000, ...
    ## $ y            <dbl> 40.43887, 0.00000, 0.00000, 40.46254, 0.00000, 40...

Which zone has the most arrests?

``` r
df %>%
  group_by(zone) %>% 
  count() %>% 
  arrange(-n)
```

    ## # A tibble: 16 × 2
    ##         zone      n
    ##        <chr>  <int>
    ## 1          3 121018
    ## 2          5  90976
    ## 3          1  85195
    ## 4          2  80782
    ## 5          4  78313
    ## 6          6  35425
    ## 7    OUTSIDE   2125
    ## 8        OSC   1390
    ## 9          9     11
    ## 10      <NA>      5
    ## 11      SAFC      3
    ## 12       TRU      3
    ## 13       SDD      2
    ## 14      HAPD      1
    ## 15 NARC/VICE      1
    ## 16  PROPROOM      1

Before we start making plots, we will load my custom theme

``` r
source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

    ## Warning: `legend.margin` must be specified using `margin()`. For the old
    ## behavior use legend.spacing

    ## Warning: New theme missing the following elements: axis.title.x.top,
    ## axis.title.y.right, axis.text.x.top, axis.text.y.right, legend.spacing.x,
    ## legend.spacing.y, legend.box.margin, legend.box.background,
    ## legend.box.spacing, panel.spacing.x, panel.spacing.y, plot.subtitle,
    ## plot.caption, strip.placement

How has the number of arrests changed over time?

``` r
df %>% 
  filter(zone %in% c(1:6)) %>% 
  group_by(zone, date) %>% 
  count() %>% 
  ggplot(aes(date, n, color = zone, fill = zone)) +
  geom_smooth() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)
```

    ## `geom_smooth()` using method = 'gam'

![](Exploratory_Analysis_rmarkdown_github_files/figure-markdown_github/arrests%20over%20time-1.png)

Looks like there is a data or reporting problem in Zone 6

Bob Gradeck pointed out that Zone 6 was closed in 2003 and reopened in 2008

<http://www.post-gazette.com/local/neighborhoods/2008/03/31/Police-manpower-tipped-to-West-End-s-Zone-6/stories/200803310145>

What does the trend look like if we just look at incidents since 2008?

``` r
df %>% 
  filter(zone %in% c(1:6), year >= 2008) %>% 
  group_by(zone, date) %>% 
  count() %>% 
  ggplot(aes(date, n, color = zone, fill = zone)) +
  geom_smooth() +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)
```

    ## `geom_smooth()` using method = 'gam'

![](Exploratory_Analysis_rmarkdown_github_files/figure-markdown_github/arrests%20over%20time%202008-1.png)

Which neighborhood has the most arrests?

``` r
df %>%
  group_by(neighborhood) %>% 
  count() %>% 
  arrange(-n)
```

    ## # A tibble: 95 × 2
    ##                   neighborhood     n
    ##                          <chr> <int>
    ## 1  Golden Triangle/Civic Arena 29147
    ## 2             South Side Flats 28251
    ## 3                      Carrick 18059
    ## 4                   Bloomfield 16223
    ## 5                    Shadyside 14584
    ## 6                 East Liberty 11903
    ## 7                    Knoxville 11276
    ## 8               Homewood South 11019
    ## 9             Mount Washington 11018
    ## 10             Central Oakland 10435
    ## # ... with 85 more rows

Which arrest descriptions are the most common?

``` r
df %>%
  group_by(description) %>% 
  count() %>% 
  arrange(-n)
```

    ## # A tibble: 330 × 2
    ##                       description      n
    ##                             <chr>  <int>
    ## 1                            <NA> 109346
    ## 2                 THEFT FROM AUTO  29617
    ## 3                THEFT/ALL OTHERS  28790
    ## 4        CRIMINAL MISCHIEF (AUTO)  28143
    ## 5           SIMPLE ASSAULT/INJURY  22188
    ## 6               CRIMINAL MISCHIEF  19635
    ## 7  HARRASSMENT/THREAT/ATTEMPT/PHY  16988
    ## 8          BURGLARY/FORCE ENT/RES  15171
    ## 9            MTR VEH THEFT (AUTO)  12313
    ## 10              MARIJUANA-POSSESS  11666
    ## # ... with 320 more rows

Looks like there are many NA values

Have the number of marijuana-related arrests changed over time?

``` r
pot_arrests <- df %>%
  filter(grepl("MARIJUANA", description) == TRUE) %>%
  group_by(date) %>% 
  count() 

#Plot the data
ggplot(data = pot_arrests, aes(x = date, y = n)) +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'gam'

![](Exploratory_Analysis_rmarkdown_github_files/figure-markdown_github/marijuana%20arrests-1.png)

Let's look at how the data looks on a map

First, create the map

``` r
city_map <-  get_map(location = "Oakland, Pittsburgh, PA",
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

View the map to make sure it looks right

``` r
ggmap(city_map)
```

![](Exploratory_Analysis_rmarkdown_github_files/figure-markdown_github/view%20map-1.png)

Filter out data that is not in one of the six police zones

``` r
df_map <- df %>% 
  filter(zone %in% c(1:6))
```

``` r
ggmap(city_map) +
  stat_density_2d(data = df_map, #Using a 2d contour
                  aes(x, #longitude
                      y, #latitude
                      fill = ..level.., #Use the level of arrests as the fill
                      alpha = .5), #Use alpha so you can see the map under the data
                  geom = "polygon") + #We want the contour in a polygon
  scale_fill_viridis() +
  guides(alpha = FALSE,
         fill = guide_colorbar("Count of Arrests")) +
  labs(x = "",
       y = "") +
  theme_nhh() +
  theme(axis.text = element_blank())
```

    ## Warning: `panel.margin` is deprecated. Please use `panel.spacing` property
    ## instead

    ## Warning: `legend.margin` must be specified using `margin()`. For the old
    ## behavior use legend.spacing

    ## Warning: Removed 29005 rows containing non-finite values (stat_density2d).

![](Exploratory_Analysis_rmarkdown_github_files/figure-markdown_github/plot%20map-1.png)
