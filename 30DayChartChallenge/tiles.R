# day 23: tiles
library(tidyverse)
library(geofacet)
library(ggtext)

# -- prepare data
rents <- read_csv('30DayChartChallenge/other/State_MedianRentalPrice_1Bedroom.csv')[, -c(1,3)] %>% 
  pivot_longer(cols = starts_with('2'), names_to = 'year-month', values_to = 'median') %>% 
  mutate(year = as.numeric(substr(`year-month`, 1, 4)), 
         state_abb = state.abb[match(RegionName, state.name)], 
         state_abb = ifelse(is.na(state_abb), 'DC', state_abb)) %>% 
  group_by(state_abb, year) %>% 
  summarise(price = median(median, na.rm = T)) %>% 
  drop_na()

state_label = rents %>% 
  group_by(state_abb) %>% 
  summarise(x_pos = mean(year), 
            y_pos = ifelse(max(price) > 1500, 500, 2000), 
            color = ifelse(max(price) > 1500, '#f8edeb', '#023047'))

ann_text <- data.frame(
  year = c(2010, 2010),
  price = c(0, 2500),
  state_abb = c('CA', 'CA'),
  lab = c('$0 <b style="font-size:16px">-</b>', 
          '$2500 <b style="font-size:16px">-</b>')
)

# -- make plot
ggplot(rents, aes(year, price)) + 
  geom_area(fill = '#219ebc', color = '#023047') + 
  geom_text(data = state_label, 
            aes(x = x_pos, y = y_pos, label = state_abb, color = color), 
            family = 'American Typewriter', size = 3.5) +
  geom_richtext(data = ann_text, aes(label = lab), hjust = 1,
            family = 'American Typewriter', color = '#023047', size = 3, 
            label.color = 'transparent', fill = 'transparent', 
            label.padding = unit(rep(0, 4), 'lines')) +
  facet_geo(~ state_abb, grid = "us_state_grid2", scale = 'free_x') + 
  coord_cartesian(clip = 'off') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_identity() +
  labs(
    title = 'Median US <b style="color:#219ebc">Rental Price</b> by State', 
    subtitle = 'Based on a 1-bedroom appartment  • 2010-2019',
    caption = '@ThomIvar • source: Zillow (via Kaggle)',
    x = '', y = ''
  ) +
  theme(
    text = element_text('American Typewriter', color = '#023047'),
    plot.title = element_markdown(color = '#023047', size = 26),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(color = 'grey70', size = 9),
    strip.background = element_blank(), strip.text = element_blank(),
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    panel.background = element_rect(fill = '#e2eafc', color = '#e2eafc'),
    plot.margin = margin(1, 1, 1, 1.5, 'cm'), 
  )
ggsave('30DayChartChallenge/images/tiles.png', width = 9, height = 6)

