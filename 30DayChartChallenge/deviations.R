# day 28: deviations
library(tidyverse)
library(ggtext)

# -- read data
selected_countries <- c('GBR', 'NLD', 'USA', 'BEL', 'DEU', 'SWE', 'CAN', 'AUS','DNK')  

df <- read_csv('30DayChartChallenge/other/excess-mortality-raw-death-count.csv') %>% 
  dplyr::filter(Code %in% selected_countries, 
                Day >= as.Date('2020-01-05'), 
                Day <= as.Date('2020-12-25')) %>% 
  pivot_longer(4:12, names_to = 'type', values_to = 'value') %>% 
  mutate(
    color = case_when(
      type == 'deaths_2020_all_ages'	~ '#2b2d42', 
      type == 'deaths_2021_all_ages'	~ 'transparent',
      type == 'deaths_2022_all_ages' ~ 'transparent',
      type == 'projected_deaths_2020_2022_all_ages'	~ '#8d99ae',
      T ~ '#edf2f4'
    )
  ) 

df_labs <- group_by(df, Entity) %>% 
  summarise(Day = median(Day), value = max(value, na.rm = T), Code = unique(Code),
            type = NA, color = 'grey50')

# -- make plot
ggplot(df, aes(x = Day, y = value, group = type, color = color)) +
  geom_line(size = 0.7) +
  scale_color_identity() +
  geom_text(data = df_labs, aes(label = Entity, y = value * 1.05), family = 'American Typewriter',
            fontface = 'bold', size = 4) +
  scale_y_continuous(n.breaks = 4, labels = scales::unit_format(unit = 'K', scale = 1e-3)) +
  scale_x_date(date_breaks = '4 months', date_labels = "%b") +
  facet_wrap(~Code, scales = 'free_y') +
  coord_cartesian(clip = 'off') +
  labs(
    title = 'When models deviate from reality', 
    subtitle = '<b style="color:#2b2d42">Deaths in 2020</b> versus 
    <b style="color:#8d99ae">projected deaths in 2020</b> (based on 2015-2019)',
    x = 'Month of the year', y = 'Raw number of deaths',
    caption = '@ThomIvar • source: Our World in Data'
  ) +
  theme_void() +
  theme(
    text = element_text('American Typewriter'),
    plot.title = element_text(color = 'grey30', size = 26, margin = margin(0,0,0.1,0,'cm')),
    plot.subtitle = element_markdown(color = 'grey50', size = 13,
                                     margin = margin(0,0,0.4,0, 'cm')),
    plot.caption = element_text(color = 'grey70', size = 8),
    strip.text = element_blank(), 
    panel.grid.major = element_line(color = 'grey70', linetype = 'dotted'),
    panel.spacing = unit(1.5, 'lines'),
    axis.line = element_line(colour = 'grey50'),
    axis.text = element_text(color = 'grey60', size = 7, margin = margin(r = 2.5)),
    axis.title = element_text(color = 'grey40', margin = margin(t = 4), size = 9), 
    axis.title.y = element_text(angle = 90, margin = margin(0, 0.2, 0, 0, 'cm')),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )
ggsave('30DayChartChallenge/images/deviations.png', width = 10, height = 7)
