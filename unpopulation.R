# day 30: UN population
library(tidyverse)
library(readxl)
library(ggbeeswarm)
library(ggdist)
library(ggtext)

# -- get data 
cols <- colorRampPalette(c("#8ecae6", "#219ebc"))(10)

historical <- read_excel('30DayChartChallenge/other/rate_of_natural_increase.xlsx', 
           sheet = 'ESTIMATES', skip = 16) %>% 
  dplyr::filter(Type == 'Country/Area') %>% 
  select(c(1:3,6, 8:21)) %>% 
  pivot_longer(5:18, names_to = 'period', values_to = 'value') %>% 
  mutate(value = as.numeric(value), 
         isAfrica = ifelse(Index < 88, T, F), 
         color = ifelse(isAfrica, 'africe_hl', period), 
         alpha = ifelse(isAfrica, 0.9, 0.5), 
         size = ifelse(isAfrica, 1.2, 0.75)) %>% 
  dplyr::filter(
    period != '1950-1955', period != '1955-1960', period != '1960-1965', 
    period != '1965-1970'
  )

names(historical)[2] = 'name'

# -- make plot

ggplot(historical, aes(x = period, y = value)) +
  geom_boxplot(fill = 'transparent', width =  0.4, color = 'grey60', 
               outlier.shape = NA, alpha = 0.8, coef = 0) +
  geom_quasirandom(aes(color = color, alpha = alpha, size = size), width = 0.20) +
  stat_halfeye(aes(fill = period), color = 'grey50', justification = -0.6, 
               width = 0.4, .width = 0, alpha = 0.9) +
  geom_text(data = unique(historical[, c('period')]), 
            aes(y = 45, label = period), color = cols, size = 3,
            family = 'American Typewriter') +
  annotate('text', y = -16, x = 1.5, label = 'Cambodian genocide', 
           family = 'American Typewriter', color = 'grey50', size = 3)+
  geom_curve(data = NULL, aes(x = 1.5, y = -17.5, xend = 1.90, yend = -21.5),
    arrow = arrow(length = unit(0.02, "npc")), color = 'grey60', size = 0.35) +
  scale_color_manual(values = c(cols, '#023047')) +
  scale_alpha_identity() +
  scale_size_identity() +
  scale_fill_manual(values = cols) +
  labs(title = "<span style='color:#023047;font-size:34px'>Africa</span>'s Natural Population Rate 
       exceeds the <span style='color:#5DB6D3'>rest of the world </span>", 
       subtitle = 'Crude birth rate minus crude death rate, per 1000',
       caption = '@ThomIvar • source: UN Population', 
       y = 'Natural population rate, per 1000') +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = 'grey50'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'),
    plot.margin = margin(1, 1, 1, 1, 'cm'),
    plot.title = element_markdown(face = 'bold', size = 17, margin = margin(b = 4)),
    plot.subtitle = element_text(colour = 'grey60', margin = margin(b = 7.5)),
    plot.caption = element_text(colour = 'grey80', size = 8),
    legend.position = 'none',
    axis.line.y = element_line(colour = 'grey50'),
    axis.title.y = element_text(angle = 90, margin = margin(r = 10), size = 10), 
    axis.text.y = element_text(color = 'grey70', size = 9, margin = margin(r = 5)), 
    panel.grid.major.y = element_line(colour = 'grey80', linetype = 'dotted')
  )
ggsave('30DayChartChallenge/images/unpopulation.png', width = 9, height = 6)  
  
  
  
