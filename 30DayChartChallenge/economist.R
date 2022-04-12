# Day 12: the Economist
library(ggthemes)
library(sysfonts)
library(tidyverse)

# -- prepare data
df <- read_csv("30DayChartChallenge/other/eurostat_inflation.csv") %>% 
  select('area' = 'geo', 'date' = 'TIME_PERIOD', 'val' = 'OBS_VALUE') %>% 
  mutate(date = as.Date(paste0(date, '-1'), '%Y-%m-%d'))

format(Sys.Date(), '%Y-%m')
font_add_google("PT Sans")
ggplot(df, aes(x = date, y = val)) +
  geom_area(fill = '#01a2d9', color = '#014d64', size = 1.8, 
            alpha = 0.4) +
  scale_x_date(limits = c(min(df$date), max(df$date)), expand = c(0,0), 
               date_minor_breaks = '3 months') +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,4.1), clip = 'off') +
  annotate('rect', xmin = as.Date('2018-03-01'), xmax = as.Date('2018-05-15'), 
           ymin = 4.65, ymax = 5, fill = 'red') +
  annotate('text', x = as.Date('2021-06-01'), y = -0.6, size = 3, color = 'grey70',
           label = '@ThomIvar with {theme_economist}', family = 'PT Sans') +
  labs(title = 'Inflation in the Euro area (19 countries)',
       subtitle = 'as annual rate of change (%, Mar. 2018 - Feb. 2022)', 
       caption = 'source: Eurostat', 
       y = '', x = '') +
  theme_economist(base_family = "PT Sans") +
  theme(
    text = element_text("PT Sans"),
    plot.margin = margin(0.6,0,0.6,0.6, 'cm'), 
    plot.subtitle = element_text(margin = margin(0.10, 0, 0, 0, 'cm'), 
                                 hjust = 0, size = 11),
    plot.caption = element_text(hjust = 0, size = 10), 
    axis.text.x = element_text(hjust = 1, vjust = 1)
  )

ggsave('30DayChartChallenge/images/economist.jpg', width = 7, height = 5)
