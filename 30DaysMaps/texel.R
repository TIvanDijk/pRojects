library(tidyverse)
library(osmdata)
library(osmplotr)
library(sf)
library(ggimage)

# -- find key's and values -- 
#av <- data.frame(name = available_features())
available_tags('natural')

# -- styling --
fontName = 'American Typewriter'
titleColor = '#2994B2'
textColor = '#011638'
textColor2 = 'steelblue'
beachColor = '#FFD160'
landColor = '#D2E69C'
forestColor = '#2d6a4f'
waterColor = '#A6E3E9'
scrubColor = '#74c69d'
villageColor = '#D7B49E'
roadColor = '#AAAAAA'

# -- collect data from OpenStreetMaps -- 
bbox_texel = c(4.7, 52.98, 4.90, 53.180)

streets <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'highway', 
                  value = c('primary', 'secondary', 'tertiary')) %>% 
  osmdata_sf() %>% 
  pluck(5)

coast <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'natural', value = 'coastline') %>% 
  osmdata_sf() %>% 
  pluck(5) %>% 
  osm_line2poly(bbox_texel) %>% 
  pluck(1)

water <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'water') %>% 
  osmdata_sf() %>% 
  pluck(8) # extract polygons

forest <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'landuse', value = 'forest') %>% 
  osmdata_sf() %>% 
  pluck(8) %>% # extract polygons
  dplyr::mutate(area = as.numeric(st_area(geometry))) %>% 
  dplyr::filter(area > 5e4)   # select only forests that cover at least 5km^2

beach <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'natural', 
                  value = c('beach')) %>% 
  osmdata_sf() 

scrub <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'natural', value = 'scrub') %>% 
  osmdata_sf() %>% 
  pluck(8)

places <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'landuse', value = c('residential', 'commerical')) %>% 
  osmdata_sf() %>% 
  pluck(6) %>%  # extract polygons 
  dplyr::filter(is.na(name)) %>%   # only 4 bungelowparks have a name associated --> remove
  dplyr::mutate(area = as.numeric(st_area(geometry))) %>% 
  dplyr::filter(area > 1e4)   # select only residential areas that cover at least 1km^2
  
places.labels <- opq(bbox_texel) %>% 
  add_osm_feature(key = 'population') %>% 
  osmdata_sf() %>% 
  pluck(4) %>% # extract points 
  dplyr::filter(name %in% c('Den Burg', 'Oudeschild', 'Den Hoorn', 'De Cocksdorp', 
                            'De Koog', 'Oosterend'))


main <- ggplot(coast, aes(geometry = geometry)) +
  # add all data-parts of the map
  geom_sf(fill = landColor, color = 'transparent') +
  geom_sf(data = scrub,  fill = scrubColor, color = scrubColor) +
  geom_sf(data = beach$osm_multipolygons, fill = beachColor, color = beachColor, size = 1.5) +
  geom_sf(data = beach$osm_polygons, fill = beachColor, color = beachColor, size = 1.5) +
  geom_sf(data = forest, fill = forestColor, color = forestColor) +
  geom_sf(data = places, color = villageColor, fill = villageColor, size = 1.5) +
  geom_sf(data = water, fill = waterColor, color = waterColor) +
  geom_sf(data = streets, color = roadColor, size = 1.1) +
  # add the icons (credits: FreePik, Smashicons, Flat-icons-com, smashingstocks)
  geom_image(aes(x = 4.85, y = 53.194, image = '30DaysMaps/icons/lighthouse.png'), 
             size = 0.08) +
  geom_image(aes(x = 4.70, y = 53.187, image = '30DaysMaps/icons/seagull.png'), 
             size = 0.075) +
  geom_image(aes(x = 4.83, y = 53.12, image = '30DaysMaps/icons/aircraft.png'), 
             size = 0.05) +
  geom_image(aes(x = 4.88, y = 53.045, image = '30DaysMaps/icons/fishing-boat.png'), 
             size = 0.07) +
  geom_image(aes(x = 4.88, y = 53.13, image = '30DaysMaps/icons/sheep.png'), 
             size = 0.05) +
  geom_image(aes(x = 4.886, y = 53.126, image = '30DaysMaps/icons/sheep.png'), 
             size = 0.035) +
  geom_image(aes(x = 4.753, y = 53.106, image = '30DaysMaps/icons/beach-chair.png'), 
             size = 0.05) +
  geom_image(aes(x = 4.73, y = 52.995, image = '30DaysMaps/icons/seal.png'), 
             size = 0.060) +
  geom_image(aes(x = 4.795, y = 53.0715, image = '30DaysMaps/icons/church.png'), 
             size = 0.10) +
  # add the labels
  geom_text(data = places.labels,  aes(x = st_coordinates(geometry)[,1],  
                                       y = st_coordinates(geometry)[,2], 
                                       label = name, 
                                       vjust = ifelse(name %in% c('Den Hoorn', 'Den Burg'), -0.5, 2.2)), 
            family = fontName, hjust = 0.5, fontface = 'bold', 
            color = textColor, size = 7) +
  geom_text(aes(x = 4.695, 53.05), label =  'Noordzee', angle = 70, 
            color = textColor2, family = fontName, size = 6, alpha = 0.6) +
  geom_text(aes(x = 4.83, 53.01), label =  'Waddenzee', angle = 40, 
            color = textColor2, family = fontName, size = 6, alpha = 0.6) +
  annotate('text', x = 4.75, y = 53.17, label = 'Texel', size = 36, 
           family = fontName, color = titleColor) +
  xlim(4.664, 4.935) + ylim(52.97, 53.214) +
  theme_void() +
  theme(plot.background = element_rect(color = waterColor, fill = waterColor), 
        panel.background = element_rect(color = waterColor, fill = waterColor))

ggsave('30DaysMaps/texel.png', width = 10, height = 15)
