# day 17: connections
library(tidyverse)
library(rgdal)

# -- get data
temp <- tempfile()
tempUnzipped <- tempfile()
download.file(
  'https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_railroads.zip',
  temp
)
unzip(zipfile = temp, exdir = tempUnzipped)

df <- readOGR( dsn = tempUnzipped, verbose = FALSE) %>%  #read shapefile
  sf::st_as_sf()

# -- make plot
ggplot(df) +
  geom_sf(aes(geometry = geometry), color = '#9C918E', size = 0.30, alpha = 0.75) +
  labs(title = 'Railways', 
       caption = '@ThomIvar • source: Natural Earth') +
  theme_void() +
  theme(
    text = element_text('Noteworthy Light', color = '#9C918E'),
    plot.title = element_text(hjust = 0.5, size = 26), 
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.caption = element_text('American Typewriter', size = 7, color = '#c0b9b7'),
    legend.position = "none",
    plot.margin = margin(1,1,1,1, 'cm')
  )
ggsave('30DayChartChallenge/images/connections.png', width = 8, height = 4.20)

