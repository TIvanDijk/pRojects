## MAP: cities with over 100,000 inhabitants
library(rgdal)
library(ggplot2)
library(dplyr)
library(sf)

# download data from Natural Earth
temp <- tempfile()
tempUnzipped <- tempfile()
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_populated_places.zip", temp)
unzip(zipfile = temp, exdir = tempUnzipped)

# format data
df <- readOGR( dsn = tempUnzipped, verbose = FALSE) %>%  #read shapefile
  sf::st_as_sf() %>% 
  dplyr::filter(POP_MAX > 100000) %>% 
  select(lon = LONGITUDE, lat = LATITUDE, pop = POP_MAX)

ggplot(df) +
  geom_point( aes(x = lon, y = lat), 
              alpha = 0.7, color = '#9C918E', size = 0.25) + 
  labs(title = 'Cities with more than 100,000 inhabitants', 
       subtitle = '(source: Natural Earth)',
       caption = '@ThomIvar') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#fffef7', color = '#fffef7'), 
        text = element_text('American Typewriter', color = '#9C918E'),
        plot.title = element_text(hjust = 0.5, size = 26, face = 'bold'), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        plot.caption = element_text(size = 8, color = 'grey80'), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = 'cm'))

ggsave("30DaysMaps/cities_map.png", width = 10, height = 6)

