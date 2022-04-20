# day 22: animation
library(tidyverse)
library(gganimate)
library(maps)

# -- get data
df <- read_csv("30DayChartChallenge/other/earthquakes.csv") %>% 
  mutate(date = format(time, '%d %b %Y')) %>% 
  dplyr::filter(mag >= 5, latitude > -62.5, 
                as.Date(time) >= as.Date('2021-10-01'))

set.seed(42)
df$xpos = 0 + rnorm(419, sd = 0.75)
df$ypos = sapply(87.5 + rnorm(419, sd = 0.75), function(x) min(x, 90))

# -- make plot
p1 <- ggplot(df) +
  borders("world", colour = "#edc4b3", fill = "#edc4b3") +
  geom_point(aes(x = longitude, y = latitude, size = mag), alpha = 0.60,
             color = '#c44536') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-62.5, 90)) +
  scale_size_continuous(range = c(3, 8), name = 'Magnitude') +
  coord_cartesian(clip ='off') +
  labs(caption = '@ThomIvar • source: USGS') +
  geom_text(aes(x = -120, y = 85, label = date), color = '#9d6b53', hjust = 1,
            family = 'American Typewriter') +
  annotate('text', x = -42.5, y = 87.5, label = 'A', vjust = -1.8, fontface = 'bold',
           family = 'American Typewriter', size = 8, color = '#b07d62') +
  geom_text(aes(x = xpos, y = ypos), label = 'Shaky', vjust = -1.4, fontface = 'bold',
            family = 'American Typewriter', size = 10, color = '#b07d62') +
  annotate('text', x = 57.5, y = 87.5, label = 'World', vjust = -1.8, fontface = 'bold',
           family = 'American Typewriter', size = 8, color = '#b07d62') +
  transition_time(time) +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = '#9d6b53'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(2, 1, 1, 1, 'cm')
  )
animate(p1, width = 680, nframes = 180, fps = 10)
anim_save(filename = '30DayChartChallenge/images/animation.gif')
