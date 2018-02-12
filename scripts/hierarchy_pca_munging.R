source("scripts/load_data_new.R")

df %>% 
  filter(date >= "2016-01-01", 
         !is.na(neighborhood), 
         zone %in% c(1:6), 
         !(neighborhood %in% c("Outside State", "Outside County", "Outside City")),
         hierarchy != 99) -> df

#consider filtering out "Not recorded"
df %>%
  count(hierarchy, hierarchy_description, sort = TRUE) %>% 
  arrange(hierarchy) -> df_top_crimes
df_top_crimes

hierarchy_description_list <- df_top_crimes$hierarchy_description

df %>% 
  semi_join(df_top_crimes) %>% 
  count(neighborhood, hierarchy_description) %>% 
  complete(neighborhood, hierarchy_description = hierarchy_description_list) %>% 
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
  spread(key = hierarchy_description, value = crime_percentage) -> df

df
