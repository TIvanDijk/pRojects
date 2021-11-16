# MAP: urban / rural areas of Europe 
library(tidyverse)
library(sf)

# -- load data --
# eurostat population polygons 
temp <- tempfile()
estatFile <- tempfile()
download.file("https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/JRC_GRID_2018.zip", temp)
unzip(zipfile = temp, exdir = estatFile)

# natural earth country borders 
temp <- tempfile()
neFile <- tempfile()
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", temp)
unzip(zipfile = temp, exdir = neFile)

# -- prepare data -- 
df <- read_sf(dsn = estatFile,  lay = "JRC_POPULATION_2018") %>% 
  dplyr::mutate(cat = case_when(TOT_P_2018 < 300 ~ 'A', 
                          TOT_P_2018 < 2500 ~ 'B', 
                          T ~ 'C'))

suburb = subset(df, cat == 'B')
urban = subset(df, cat == 'C')

borders <- read_sf(dsn = neFile, lay = 'ne_10m_admin_0_countries')
included <- borders$ISO_A2_EH %in% c(unique(df$CNTR_ID), 'GB', 'GR')

legend.df <-data.frame(x = -12, y = c(46, 47, 48), 
                       color = c('#EBEBD3', '#F4D35E', '#DA4167'), 
                       name = c('Rural', 'Suburban', 'Urban'))

# -- make plot --
ggplot() +
  geom_sf(data = borders, aes(geometry = geometry), 
          fill = ifelse(included == T, '#EBEBD3', 'transparent'), 
          color = 'grey40') +
  geom_sf(data = suburb, fill = "#F4D35E", color = "#F4D35E") +
  geom_sf(data = urban, fill = '#DA4167', color = '#DA4167') +
  # manual legend
  geom_point(data = legend.df, aes(x = x, y = y), shape = 22, size = 8, 
             color = 'grey30', fill = legend.df$color, stroke = 1.5) +
  geom_text(data  = legend.df, aes(x = x, y = y, label = name),
            hjust = 0, nudge_x = 1, family = 'American Typewriter', 
            color = 'grey30', size = 6) +
  # remaining elements
  annotate('text', x = -14, y = 64, label = 'Urbanisation', fontface = 'bold',
           size = 17, family = 'American Typewriter', color = 'grey30', hjust = 0) +
  geom_rect(aes(xmin = -10, xmax = 0, ymin = 59.5, ymax = 63), color = '#fffef7', fill = '#fffef7') + # lazy removal of Munger Skerries & FAEROER
  annotate('text', x = -14, y = 63, family = 'American Typewriter', color = 'grey60',
           label = '@ThomIvar • sources: GEOSTAT eurostat & Natural Earth', hjust = 0) +
  xlim(-14, 30) + ylim(36,66) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#fffef7', color = '#fffef7'))

ggsave('30DaysMaps/urban.png', height = 10.77, width = 10)



