library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
set.seed(8000)

install.packages(c("maptools", "rgeos", "Cairo"))

setwd("C:/Users/conor/githubfolder/pgh-crime/shapefiles/Pittsburgh_Neighborhoods")
neighborhoods.shp <- readShapeSpatial("Pittsburgh_Neighborhoods.shp")
class(neighborhoods.shp)
names(neighborhoods.shp)
print(neighborhoods.shp$hood)


df_nbh <- df %>% 
  select(neighborhood) %>% 
  mutate(id = neighborhood) %>% 
  group_by(id) %>% 
  count() %>% 
  arrange(-n)

df_nbh$id[df_nbh$id == "Golden Triangle/Civic Arena"] <- "Central Business District"

neighborhoods.shp.f <- fortify(neighborhoods.shp, region = "hood")
class(neighborhoods.shp.f)


merge.neighborhoods <- merge(neighborhoods.shp.f, df_nbh, by="id", all.x=TRUE)
final.plot <- merge.neighborhoods[order(merge.neighborhoods$order), ] 

ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = n), 
               color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_viridis()

#problems
#in the crim incident db, downtown is called "Golden Triangle/Civic Arena". It's called "Central Business District" in the shapefile