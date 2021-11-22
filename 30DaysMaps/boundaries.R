# MAP: Dutch boundaries
library(tidyverse)
library(sf)
library(ggrough)

# -- edit {ggrough} -- 
# such that it works with geom_sf
# solution by: https://stackoverflow.com/questions/64031046/how-to-shade-shapes

# trace(ggrough:::parse_rough, edit=TRUE) <-- run this 
#   if (geom %in% c("GeomSf")) {          <-- add this to pop-up venster 
#     rough_els <- append(rough_els, parse_sf(svg))
#   }

# define this new parse_sf function
parse_sf <- function (svg) {
  shape <- "path"
  keys <- NULL
  ggrough:::parse_shape(svg, shape, keys) %>% {
    purrr::map(., 
               ~purrr::list_modify(.x, 
                                   points = .x$d, 
                                   shape = "path"
               ))
  }
}

# -- load data -- 
provBorders <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_provincie_2021_gegeneraliseerd&outputFormat=json")
munBorders <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2021_gegeneraliseerd&outputFormat=json") %>% 
  dplyr::mutate(cntr = st_centroid(geometry), 
                x = st_coordinates(cntr)[,1], y = st_coordinates(cntr)[,2])


capitals = c('Utrecht', 'Groningen', 'Haarlem', 'Arnhem', "'s-Hertogenbosch", "'s-Gravenhage",
             'Leeuwarden', 'Maastricht', 'Zwolle',  'Lelystad', 'Assen', 'Middelburg') 
cities <- munBorders[munBorders$statnaam %in% capitals,]




# -- make plot --
vals = c('grey60', 'grey50', 'grey40', 'grey60', 'grey80', 'grey40', 'grey80', 'grey80',
         'grey80', 'grey40', 'grey40', 'grey60')

main <- ggplot() +
  geom_sf(data = provBorders, aes(geometry = geometry, fill = statnaam), color = 'transparent') +
  geom_sf(data = munBorders, aes(geometry = geometry), 
          color ='#fffef7', fill = 'transparent', size = 0.3) +
  geom_point(data = cities, aes(x = x, y = y), color = '#bc4749', size = 2) +
  scale_fill_manual(values = vals ) +
  labs(title = 'Boundaries of the Netherlands', 
       caption = '@ThomIvar •  source: PDOK') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#fffef7', color = '#fffef7'),
        panel.background = element_rect(fill = '#fffef7', color = '#fffef7'),
        plot.margin = margin(1,1,1,1, 'cm'),
        plot.title = element_text('Savoye LET', size = 50, hjust = 0.5, color = 'grey50'),
        plot.caption = element_text('American Typewriter', size = 11, color = 'grey60'),
        legend.key.size = unit(0, 'cm'), 
        legend.title = element_text(size=0), 
        legend.text = element_text(size=0))


optionsRough <- list(
  GeomSf = list(fill_style="cross-hatch",  fill_weight = 0.5, angle_noise = 0.75, 
                gap_noise = 0.5, gap = 4)
)
get_rough_chart(main, optionsRough, width = 8.5, height = 10) 

## NOTE: {ggrough} appear to ignore geom_text()
## capitals of provinces (as in final plot) are annotated manually 



