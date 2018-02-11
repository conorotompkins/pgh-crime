source("scripts/load_data_new.R")

df %>% 
  filter(date >= "2016-01-01", !is.na(neighborhood)) -> df

#consider filtering out "Not recorded"
df %>%
  count(description, sort = TRUE) %>% 
  replace_na(list(description = "Not recorded")) %>%
  filter(n >= 500) -> df_top_crimes
df_top_crimes

description_list <- df_top_crimes$description

df %>% 
  semi_join(df_top_crimes) %>% 
  count(neighborhood,description) %>% 
  complete(neighborhood, description = description_list) %>% 
  replace_na(list(n = 0)) %>% 
  arrange(neighborhood, desc(n)) -> df

df %>% 
  ungroup() %>% 
  group_by(neighborhood) %>% 
  mutate(neighborhood_total = sum(n)) %>% 
  ungroup() %>% 
  mutate(crime_percentage = n / neighborhood_total) %>% 
  select(-n, -neighborhood_total) -> df

df %>% 
  spread(key = description, value = crime_percentage) -> df
df