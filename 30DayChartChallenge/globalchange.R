# day 19: global change
library(tidyverse)
library(ggstream)
library(ggtext)

# -- get data
pop <- read.csv('30DayChartChallenge/other/population.csv') %>% 
  dplyr::filter(
    Entity %in% c('Asia', 'Africa', 'Europe', 'North America', 'South America', 'Oceania'), 
    Year >= 0
  ) %>% 
  select(group = Entity, year = Year, value = `Population..historical.estimates.`) %>% 
  group_by(group) %>% 
  complete(year = full_seq(0:2020, 1)) %>% 
  mutate(value_approx = zoo::na.approx(value)) %>% 
  ungroup()

pop_labels <- data.frame(
  x = c(1930, 1950, 1950, 1990, 1970),
  y = c(0.25e9, 2.5e9, -8e8, -1.9e9, -2.6e9), 
  group = c('Asia', 'Africa', 'Europe', 'North America', 'South America'), 
  color = c('grey90', '#023e8a', 'grey90', 'grey90', '#90e0ef'), 
  angle = c(0, 75, -60, -75, -80), 
  size = c(6, 4.5, 3, 2.5, 2.5)
)

time_labels <- data.frame(
  x = seq(0, 2000, 500), 
  y = c(-2e8, -2.5e8, -3.5e8, -4e8, -3.5e9), 
  yend = c(-2.25e8, -2.5e8, -3.5e8, -4e8, -3.5e9) + 1e8
)

title_label = "<b style='font-size:70px'>2020 years</b><br> of population figures"

# -- make plot
ggplot(pop, aes(year, value_approx)) +
  geom_stream(aes(fill = group), extra_span = 0, n_grid = 2200) +
  geom_text(data = pop_labels, 
            aes(x = x, y = y, label = group, color = color, angle = angle, size = size), 
            family = 'American Typewriter') +
  coord_cartesian(clip = 'off') +
  # 'y-axis'
  annotate('segment', x = 2070, xend = 2070, y = -1.6e9, yend = 2.4e9, 
           colour = 'grey50', linetype = 'dotted') +
  annotate('richtext', x = 2070, y = 0.4e9, label = '4.6B', angle = 90, color = 'grey50',
           family = 'American Typewriter', size = 3, fill = '#fffef7', label.colour = '#fffef7') +
  # 'x-axis'
  geom_text(data = time_labels, aes(x = x, y = y, label = x), 
            vjust = 1.1, color = 'grey70', family = 'American Typewriter', size = 2.5) +
  geom_segment(data = time_labels, aes(x = x, xend = x, y = y, yend = yend) ,
               color = 'grey70') +
  # 'title' 
  annotate('richtext', x = 0, y = 2.5e9, label = title_label, hjust = 0, 
           family = 'American Typewriter', size = 9, color = 'grey40', 
           label.color = '#fffef7', fill = '#fffef7') +
  # remainder
  labs(caption = '@ThomIvar • source: Our World in Data') +
  scale_fill_manual(values = c('#023e8a', '#0077b6', '#0096c7', '#00b4d8', '#48cae4', '#90e0ef')) +
  scale_color_identity() +
  scale_size_identity() +
  theme_void() +
  theme(
    legend.position = 'none', 
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'),
    plot.caption = element_text('American Typewriter', color = 'grey70', size = 8),
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )
ggsave('30DayChartChallenge/images/globalchange.png', width = 8, height = 5)
