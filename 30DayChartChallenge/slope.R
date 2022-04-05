# Day 5: Slope
library(tidyverse)
library(ggflags)
library(countrycode)

# -- prepare data
births <- read_csv("30DayChartChallenge/other/worldbank_data_births.csv", skip = 3) %>% 
  select(country = `Country Name`, iso3c = `Country Code`,  
         x_start = `2010`, x_end = `2019`)

deaths <- read_csv("30DayChartChallenge/other/worldbank_data_deaths.csv", skip = 3) %>% 
  select(country = `Country Name`, iso3c = `Country Code`,  
         y_start = `2010`, y_end = `2019`)

countries <- c('Netherlands', 'Belgium', 'Germany', 'France', 'Spain', 'Portugal',
               'United States', 'China', 'India', 'Russian Federation', 'Brazil', 
               'Japan', 'Argentina', 'United Kingdom', 'Ireland', 'Denmark', 
               'Norway','Sweden', 'South Africa', 'Israel', 'Turkey')

df <- left_join(births, deaths) %>% 
  dplyr::filter(country %in% countries) %>% 
  mutate(iso2c = tolower(countrycode(iso3c, origin = 'iso3c', destination = 'iso2c')))
  


ggplot(df) +
  geom_abline(intercept = 0, slope = 1, color = 'grey30', linetype = 2) +
  geom_point(aes(x = x_start, y = y_start), color = 'grey50') + 
  geom_segment(aes(x = x_start, xend = x_end, y = y_start, yend = y_end), 
               color = 'grey50') +
  geom_flag(aes(country = iso2c, x = x_end, y = y_end), size = 5) +
  geom_text(aes(x = 14.2, y = 13.8), angle = 35, family = 'American Typewriter',
            label = 'Above this line the death rate exceeds the birth rate', 
            color = 'grey70', size = 3.5) +
  # add self-made legend
  geom_rect(data = NULL, aes(xmin = 19.5, xmax = 22.5, ymin = 15.6, ymax = 16.8), 
            fill = 'transparent', color = 'grey70') +
  geom_point(aes(x = 20, y = 16), color = 'grey50') +
  geom_text(aes(x = 20, y = 16.5), label = '2010', 
            family = 'American Typewriter', color = 'grey70') +
  geom_segment(aes(x = 20, xend = 22, y = 16, yend = 16), color = 'grey50') +
  geom_flag(aes(country = 'nl', x = 22, y = 16), size = 6) +
  geom_text(aes(x = 22, y = 16.5), label = '2019', 
            family = 'American Typewriter', color = 'grey70') +
  xlab('Birth rate, crude (per 1,000 people)') +
  ylab('Death rate, crude (per 1,000 people') +
  labs(caption = '@ThomIvar • source: The World Bank', 
       title = 'Birth & Death rates in the last decade') +
  theme_void() +
  theme(
    text = element_text('American Typewriter'), 
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.title = element_text(color = 'grey40', size = 24, face = 'bold'),
    plot.caption = element_text(color = 'grey80', size = 9),
    plot.margin = margin(1,1,1,1, 'cm'),
    axis.title = element_text(color = 'grey40', margin = margin(0.1,0,0,0, 'cm')), 
    axis.title.y = element_text(angle = 90, margin = margin(0,0.2,0,0, 'cm')), 
    axis.text = element_text(color = 'grey60'),
    panel.grid = element_line(color = 'grey80', linetype = 'dotted'),
  )
ggsave('30DayChartChallenge/images/slope.png', width = 10, height = 6)
