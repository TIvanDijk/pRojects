# Day 1: part-to-whole
library(tidyverse)
library(ggtext)

# -- params
n = 250
input = c('Cream-filled' = 22, 'Plain-glazed' = 18, 'Chocolate-glazed' = 12, 'Other' = 48)
cols = c('Cream-filled' = '#e36397', 'Plain-glazed' = '#FFC857', 
         'Chocolate-glazed' = '#2f0e07','Other' = '#faeadb')

# -- create dataset
group_sizes = round(input / sum(input) * n, 0)
l = length(group_sizes)
n.act = sum(group_sizes)

data <- data.frame(
  category = rep(names(input), times = group_sizes), 
  ymax = rep(0.9, n.act), 
  ymin = rep(0.1, n.act), 
  x = seq(0, 1, length.out = n.act)
) %>% 
  mutate(ymax = ymax + sin(37.5*x)/10, 
         ymin = ymin + cos(37.5*x)/20)

# ensure no space between layers 
data[cumsum(group_sizes[-l]), 'x'] = data[cumsum(group_sizes[-l]) + 1, 'x']

# -- create ggplot
ggplot(data, aes(ymin = ymin, ymax = ymax)) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = '#e08e45') +
  geom_ribbon(aes(x = x, fill = category)) +
  coord_polar(direction=1) +
  labs(title = "<b style='color:#e36397;font-size:54px'>22%</b> of Americans prefer cream-filled donuts", 
       caption = "@ThomIvar • source: Ipsos") +
  scale_fill_manual(values = cols, name = "") +
  scale_colour_identity() +
  ylim(-.4,1) +
  theme_void() +
  theme(
    text = element_text('American Typewriter'),
    plot.title = element_textbox_simple(hjust = 0.5, size = 26, colour = '#2f0e07'),
    plot.caption = element_text(size = 9, colour = 'grey70'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    legend.position = "top",
    plot.margin = margin(1,1,1,1, 'cm')
  )

ggsave('30DayChartChallenge/images/part-to-whole.png', width = 6, height = 7.4)
