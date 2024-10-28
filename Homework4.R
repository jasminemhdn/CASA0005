#Calling library
library(sf)
library(tidyverse)
library(tmap)
library(usethis)

#Read the file
world <- st_read("World_Countries_Generalized.shp")
indices <- read_csv("HDR23-24_Composite_indices_complete_time_series.csv")

#Create new dataframe containing only GII 2010 and 2019
GII <- indices %>% 
  mutate(GII_2010 = gii_2010) %>% 
  mutate(GII_2019 = gii_2019) %>% 
  mutate(Difference = GII_2019 - GII_2010) %>% 
  mutate(Change = case_when(Difference > 0 ~ "Increase",
                          Difference < 0 ~ "Decrease",
                          Difference == 0 ~ "No Change",
                          TRUE ~ "Unknown")) %>% 
  mutate(Difference = sqrt(Difference**2)) %>% 
  select(country, GII_2010, GII_2019, Difference, Change)

#Join data with world shp
world <- world %>% 
  merge(., GII, by.x="COUNTRY", by.y="country")

#Visualize
tmap_mode("plot")
GII2010Map <- tm_shape(world) + 
  tm_polygons("GII_2010", palette = c("#abdda4", "#ffffbf", "#d7191c"), n=6) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap("OSM")
GII2019Map <- tm_shape(world) + 
  tm_polygons("GII_2019", palette = c("#abdda4", "#ffffbf", "#d7191c"), n=6) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap("OSM")
DifferenceMap <- tm_shape(world) + 
  tm_polygons("Difference", palette = "Blues" , n=5) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap("OSM")
ChangeMap <- tm_shape(world) + 
  tm_polygons("Change", palette = c("#abdda4", "#d7191c", "#2b83ba")) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap("OSM")
tmap_arrange(GII2010Map, GII2019Map, DifferenceMap, ChangeMap, ncol = 2, nrow = 2)