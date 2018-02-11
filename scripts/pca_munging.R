source("scripts/load_data_new.R")

df %>% 
  filter(date >= "2016-01-01") %>% 
  count(neighborhood, sort = TRUE) %>% 
  replace_na(list(neighborhood = "Not recorded")) -> df_top_nbh

df %>%
  filter(date >= "2016-01-01") %>% 
  count(description, sort = TRUE) %>% 
  replace_na(list(description = "Not recorded")) -> df_top_crimes


                  
