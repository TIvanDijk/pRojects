# Day 27: future
library(tidyverse)
library(ggtext)

# -- read data
hist <- read_csv2('30DayChartChallenge/other/UN_hist.csv') %>% 
  pivot_longer(4:5, names_to = 'type', values_to = 'values') %>% 
  mutate(type = tolower(type)) %>% 
  dplyr::filter(Year >= 1980)

# 5 year periods
pred <- read_csv2('30DayChartChallenge/other/UN_pred.csv') %>% 
  transmute(
    Year = Year, type = Type,
    Median = Medan / 5, Lower_95 = Lower_95 / 5, Upper_95 = Upper_95 / 5
         ) %>% 
  bind_rows(list(Year = 2019, type = 'births', Median = 140108052, 
                 Lower_95 = 140108052, Upper_95 = 140108052)) %>% 
  bind_rows(list(Year = 2019, type = 'deaths', Median = 58394378, 
                 Lower_95 = 58394378, Upper_95 = 58394378))

# -- make plot

ggplot(pred, aes(x = Year)) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95, fill = type), alpha = 0.5) +
  geom_line(aes(y = Median, color = type), linetype = 2, size = 1.25) +
  geom_line(data = hist, aes(y = values, color = type), size = 1.25) +
  scale_x_continuous(expand = c(0,0), limits = c(1978, NA)) +
  scale_y_continuous(breaks = c(5e7, 1e8, 1.5e8), labels = c('50M', '100M', '150M')) +
  coord_cartesian(clip = 'off') +
  scale_color_manual(values = c('#08415c', '#cc2936')) +
  scale_fill_manual(values = c('#08415c', '#cc2936')) +
  labs(
    title = '<span style="color:#009EDB; font-size:30px">United Nations:</span> World Population Prospects, 2019',
    subtitle = 'Uncertainty in future population growth is mainly caused 
    by uncertainty in future <b style="color:#08415c">births</b>',
    caption = '@ThomIvar',
    x = 'Year', 
    y = 'Number of <b style="color:#08415c">Births</b> and 
    <b style="color:#cc2936">Deaths</b>'
  ) +
  theme_classic() +
  theme(
    legend.position = 'none',
    text = element_text('American Typewriter', color = 'grey50'), 
    plot.title = element_markdown(face = 'bold', size = 16),
    plot.subtitle = element_markdown(size = 12, color = 'grey40'), 
    plot.caption = element_text(size = 10, color = 'grey70'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'),
    panel.background = element_rect( fill = '#fffef7', color = '#fffef7'),
    panel.grid.major = element_line(color = 'grey70', linetype = 'dotted'),
    axis.line = element_line(color = 'grey50'),
    axis.title.x = element_text(margin = margin(0.25, 0, 0, 0, 'cm')), 
    axis.title.y = element_markdown(margin = margin(0, 0.25, 0, 0, 'cm')),
    plot.margin = margin(1, 1, 1, 1, 'cm'),
  )
ggsave('30DayChartChallenge/images/future.png', width = 9, height = 5.5)  

