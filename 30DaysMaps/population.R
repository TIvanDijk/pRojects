## -- map on Dutch population --
library(tidyverse)
library(sf)

## -- read data -- 
# obtained from https://opendata.cbs.nl/#/CBS/nl/dataset/03759ned/table?dl=39E0B
cbs_popdata <- read_delim("30DaysMaps/data/cbs_popdata.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2021_gegeneraliseerd&outputFormat=json")

# combine into 1, clean dataset
df <- municipalBoundaries %>% 
  left_join(cbs_popdata, c("statnaam" = "Regio's")) %>% 
  transmute(name = statnaam, 
            pop = `Bevolking op 1 januari (aantal)`, 
            x = st_coordinates(st_centroid(geometry))[,1], 
            y = st_coordinates(st_centroid(geometry))[,2])

## -- make plot -- 
ggplot(df) + 
  geom_text(aes(x = x, y = y, label = name, size = pop, alpha = pop), 
            color = '#250001', family = 'American Typewriter') +
  geom_rect(aes(xmin = 10000, xmax = 290000, ymin = 300000, ymax = 630000), 
            fill = 'transparent', color = '#9C918E', size = 1.5) +
  annotate('label', x = 150000, y = 630000, label = ' Dutch Population ', 
           hjust = 0.5, size = 14, family = 'American Typewriter', fontface = 'bold',
           fill = '#fffef7', color = alpha('#250001', 0.5), label.size = NA ) +
  annotate('text', x = 290000, y = 300000, label = '@ThomIvar • data: CBS', 
           hjust = 1, vjust = 1.5, family = 'American Typewriter', 
           size = 4, color = 'grey70') +
  scale_size_continuous(range = c(2, 9.0)) +
  scale_alpha(range = c(0.20, 1)) +
  theme_void() +
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = '#fffef7', color = '#fffef7'), 
        plot.margin = margin(1,1,1,1, unit = 'cm')) 


ggsave('30DaysMaps/population.png',  height = 12, width = 10)



            