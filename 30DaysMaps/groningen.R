# MAP: use OpenStreetMap to create a map of Groningen (NL)
library(tidyverse)
library(osmdata)
library(sf)

# obtain data 
names = c('motorway', 'primary', 'secondary', 'tertiary', 'trunk')
streets <- opq('Groningen') %>% 
  add_osm_feature(key = 'highway', 
                  value = c( names, paste0(names, '_link'), 
                             'construction', 'proposed')) %>% 
  osmdata_sf() %>% 
  pluck(5) 
 
small.streets <- opq('Groningen') %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street","unclassified",
                            "service", "footway", 'cycleway', 'track', 'path')) %>%
  osmdata_sf() %>% 
  pluck(5)

water <- opq('Groningen') %>% 
  add_osm_feature(key = 'waterway') %>% 
  osmdata_sf() %>% 
  pluck(5) 

# make circular 
# based on: https://github.com/AbdoulMa/30DayMapChallenge/blob/main/Day5/day5.R
circle <- tibble(lat = 53.215, long = 6.56) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 6384) %>%
  st_buffer(dist = 4000) %>%
  st_transform(crs = 4326)

streets <- st_intersection(circle, st_transform(streets, crs = 4326))
small.streets <- st_intersection(circle, st_transform(small.streets, crs = 4326))
water <- st_intersection(circle, st_transform(water, crs = 4326))
  
# -- make plot -- 
ggplot(data = NULL, aes(geometry = geometry)) +
  geom_sf(data = water, color = 'lightblue', size = 0.5) +
  geom_sf(data = streets, color = 'grey40', size = 0.8) +
  geom_sf(data = small.streets, color = 'grey60', size = 0.4) +
  geom_sf(data = circle, fill = 'transparent', color = 'grey60', size = 3) +
  geom_text(aes(x = 6.56, y = 53.189, geometry = NULL), 
            label = 'Groningen', hjust = 0.5, size = 45, color = 'grey20', 
            family = 'American Typewriter') +
  geom_text(aes(x = 6.56, y = 53.180, geometry = NULL), 
            label = 'the Netherlands', hjust = 0.5, size = 20, color = 'grey20', 
            family = 'American Typewriter', fontface = 'italic') +
  geom_text(aes(x = 6.52, y = 53.250, geometry = NULL), 
            label = '© OpenStreetMap contributors \n@ThomIvar', size = 5, color = 'grey70', 
            family = 'American Typewriter') +
  theme_void() +
  theme(plot.background = element_rect( fill = '#fffef7', color = '#fffef7'),
        legend.position = 'none')

ggsave('30DaysMaps/groningen.png', width = 10, height = 10)


