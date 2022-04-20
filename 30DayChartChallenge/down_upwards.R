# day 21: down/upwards
library(readxl)
library(tidyverse)
library(zoo)
library(ggimage)
library(ggtext)

# -- get data
# assume linear relation between two datapoints -- remove years before first data point
df <- read_excel('30DayChartChallenge/other/GEC_trend_data.xlsx', sheet = 'Data') %>% 
  group_by(Country, Ecosystem, Stratum) %>% 
  complete(Year = full_seq(1995:2015, 1)) %>% 
  mutate(approx = zoo::na.approx(Estimate, na.rm = F), 
         approx = ifelse(
           is.na(approx),
           approx[!is.na(approx)][1], 
           approx)) %>% 
  group_by(Year) %>% 
  summarise(total = sum(approx)) %>% 
  mutate(total_avg = zoo::rollmean(total, 3, NA)) %>% 
  dplyr::filter(Year>= 2005) %>% 
  drop_na()


# -- make plot
expl_text <- c(
  "A 2016 study by Chase et al. estimated that the African
  elephant populations <b>shrunk by approximately 150,000</b> from 2005 to 2014. <br><br>
  This study is sometimes referred to as the <b>Great Elephant Census</b> and is considered
  to be the first continent-wide approach to keep track of the African savannah elephants
  population. <br><br>
  <span style='font-size:14px'>@ThomIvar • Study: </span><i style='font-family:Arial;font-size:12px'>Continent-wide survey reveals
  massive decline in African savannah elephants</i>"
)

ggplot(df, aes(x = Year, y = total_avg, image = '30DayChartChallenge/other/elephant.png')) +
  # main plot
  geom_area(size = 2, fill = '#E5E1E6', color = 'grey40') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 6.5e5)) +
  # elephants 
  geom_image(aes(x = 2005.25, y = 5.025e5), hjust = 0, size = 0.10) +
  geom_image(aes(x = 2005, y = 4.9e5, image = '30DayChartChallenge/other/elephant_kid.png'), 
             hjust = 0, size = 0.06) +
  geom_image(aes(x = 2011, y = 4.35e5, image = '30DayChartChallenge/other/elephant_kid.png'),
             hjust = 1, size = 0.06) +
  geom_image(aes(x = 2010.88, y = 4.27e5, image = '30DayChartChallenge/other/tear.png'), 
             size = 0.015) +
  # annotations
  annotate('richtext', x = 2014, y = 6e5, 
           label = "The decline of <b style='color:grey40'>African Elephants</b>", 
           hjust = 1.05, color = '#E5E1E6', fill = '#fffef7', label.colour = '#fffef7', 
           size = 12, family = 'American Typewriter') +
  annotate('richtext', x = 2005, y = 4.4e5, label = '<b>2005:</b> 470K', hjust = -0.05, 
           color = 'grey40', fill = '#E5E1E6', label.colour = '#E5E1E6', size = 5, 
           family = 'American Typewriter') +
  annotate('richtext', x = 2014, y = 3.0e5, label = '<b>2014:</b> 320K', hjust = 1.05, 
           color = 'grey40', fill = '#E5E1E6', label.colour = '#E5E1E6', size = 5, 
           family = 'American Typewriter') +
  geom_textbox(aes(x = 2005, y = 2e5), label = expl_text, hjust = -0.05, 
           width = unit(6, 'inch'), family = 'American Typewriter', size = 6, 
           fill = '#E5E1E6', box.colour = '#E5E1E6', color = 'grey40') +
  theme_void() +
  theme(
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7')
  )
ggsave('30DayChartChallenge/images/down_upwards.png', width = 9, height = 6)
