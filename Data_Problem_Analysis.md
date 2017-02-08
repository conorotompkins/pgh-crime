    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

This is an analysis of potential data problems in the Pittsburgh Police Incident Blotter Archive.

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

Zone 6 was closed in 2003, and reopened in 2008. Therefore, we should expect that there are 0 incidents between 2005 (when the data begins) and 2008

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
