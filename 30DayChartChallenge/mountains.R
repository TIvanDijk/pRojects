# Day 8: Mountains
library(tidyverse)
library(rnaturalearth)
library(sf)
library(elevatr)
library(raster)
library(sysfonts)
library(emojifont)

# -- get data
elev_data <- get_elev_raster(data.frame(x = seq(-180, 180, length.out = 10), 
                                        y = seq(-66, 80, length.out = 10)), 
                             prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", 
                             z = 4, clip = "locations") %>% 
  as.data.frame(xy = TRUE)
colnames(elev_data)[3] = 'elev'

df <- elev_data %>% 
  drop_na() %>% 
  dplyr::filter(elev > 2000)
  
# -- make plot
font_add_google("Satisfy")
font_add_google("Arvo")

ggplot(df) +
  geom_tile(aes(x = x, y = y, color = elev, fill = elev), size = 0.25) +
  labs(title = "Mountains", 
       subtitle = "exceeding 2,000 meter", 
       caption = "@ThomIvar • source: AWS Terrain Tiles with {elevatr}") +
  scale_fill_gradient(low = '#ddbea9', high = '#7f5539') +
  scale_color_gradient(low = '#ddbea9', high = '#7f5539') +
  theme_void() +
  theme(
    plot.title = element_text("Satisfy", hjust = 0.5, size = 44, colour = '#2f0e07'),
    plot.subtitle = element_text("Arvo", hjust = 0.5, size = 10, 
                                 colour = '#2f0e07', margin=margin(-0.5,0,0,0, 'cm')),
    plot.caption = element_text("Arvo", size = 8, colour = 'grey80'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    legend.position = "none",
    plot.margin = margin(1,1,1,1, 'cm')
  )
ggsave('30DayChartChallenge/images/mountains.png', width = 8, height = 6)
