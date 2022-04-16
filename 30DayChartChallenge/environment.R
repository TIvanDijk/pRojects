# day 16: environment
library(tidyverse)
library(ggtext)

# -- get data 
cc_data <- read_csv("30DayChartChallenge/other/climate-change.csv") %>% 
  dplyr::filter(Entity == 'World') %>% 
  select(ent = Entity, year = Year, CO2 = `CO2 concentrations`, 
         sea_temp = `Sea surface temp`) %>% 
  drop_na() %>% 
  mutate(shape = ifelse(year > 2000, '2000 - 2021', '1850-1999'))


# -- plot data
ggplot(cc_data, aes(x = CO2, y = sea_temp, color = sea_temp)) +
  geom_point(aes(shape = shape), size = 2.5) +
  scale_color_gradient2(low = '#175676', high = '#ba324f', mid = '#cce6f4', 
                        guide = 'none') +
  scale_shape_manual(values = c(1, 19), name = '') +
  labs(
    x = '<br>Atmospheric concentrations of <b>carbon dioxide</b> (ppm)', 
    y = '<b>Temperature</b> of sea surface (in \u00B0C)<br>', 
    title = "It's getting <b style='font-size:54px;color:#ba324f'>HOT</b> in here", 
    caption = '@ThomIvar • source: Our World in Data'
  ) +
  theme_void() +
  theme(
    text = element_text('American Typewriter', colour = 'grey50'),
    plot.title = element_textbox_simple(colour = 'grey30', size = 26, 
                                        margin = margin(b=0.5, unit = 'cm')), 
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1,1,1,1, 'cm'), 
    panel.grid.major = element_line(linetype = 'dotted', color = 'grey80'),
    axis.line = element_line(colour = 'grey50'), 
    axis.text = element_text(size = 9, colour = 'grey70'),
    axis.title = element_textbox(colour = 'grey50', size = 13, hjust = 0.5), 
    axis.title.y = element_textbox(orientation = 'left-rotated'), 
    axis.ticks = element_line(colour = 'grey50'), 
    axis.ticks.length = unit(0.15, 'cm'), 
    legend.position = c(0.15, 0.9), 
  )
ggsave('30DayChartChallenge/images/enviroment.png', width = 8, height = 6)
