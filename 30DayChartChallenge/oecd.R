# day 18: OECD
library(tidyverse)
library(ggtext)

# -- get data
inflation <- read.csv('30DayChartChallenge/other/oecd_inflation.csv') %>% 
  dplyr::filter(FREQUENCY == 'M' & MEASURE == 'AGRWTH', SUBJECT == 'TOT') %>% 
  select(place = LOCATION, date = TIME, cpi = Value)

unemployment <- read.csv('30DayChartChallenge/other/oecd_unemployment.csv') %>% 
  dplyr::filter(FREQUENCY == 'M', SUBJECT == 'TOT') %>% 
  select(place = LOCATION, date = TIME, rate = Value)

df <- left_join(inflation, unemployment) %>% 
  drop_na() %>% 
  dplyr::filter(place == 'G-7') %>% 
  mutate(group = case_when(
    str_starts(date, '200') ~ '2000-2009', 
    str_starts(date, '201') ~ '2010-2019', 
    TRUE ~ 'other'
  )) %>% 
  dplyr::filter(group != 'other')

# -- make plot
ggplot(df, aes(x = rate, y = cpi, group = group, color = group)) +
  geom_point(alpha = 0.5, size = 2.25) +
  geom_smooth(method = 'lm', se = FALSE, linetype = 2) +
  scale_color_manual(values = c('#d1495b', '#00798c')) +
  annotate('text', x = 5.5, y = 3.75, label = '2000-2009', color = '#d1495b', 
           family = 'American Typewriter', fontface = 'bold') +
  annotate('text', x = 4.5, y = 0.75, label = '2010-2019', color = '#00798c', 
           family = 'American Typewriter', fontface = 'bold') +
  labs(title = 'What happened to the <b>Phillips Curve</b>?', 
       subtitle = "The Phillips curve is an economic concept that states that inflation
       and unemployment have a stable and inverse relationship. According to this concept, 
       lower unemployment rates cause firms to raise wages to attract new labor, 
       which increases the inflation rate. However, in the <b style='color:#00798c'>
       last decade</b> this relationship appears to have vanished.", 
       caption = '@ThomIvar • source: OECD (monthly, G7 countries)', 
       x = 'Unemployment rate, %', 
       y = 'Inflation rate, %') +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = 'grey30'),
    legend.position = 'none', 
    plot.title = element_textbox_simple(size = 20, margin = margin(b = 0.15, unit = 'cm')),
    plot.subtitle = element_textbox_simple(size = 11, color = 'grey60', 
                                           margin = margin(b = 0.25, unit = 'cm')),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.caption = element_text(size = 10, color = 'grey80'),
    plot.margin = margin(1, 1, 1, 1, 'cm'), 
    panel.grid = element_line(color = 'grey70', linetype = 'dotted'), 
    axis.title = element_text(margin = margin(t = 0.2, r = 0.2, unit = 'cm'), color = 'grey50'), 
    axis.title.y = element_text(angle = 90),
    axis.text = element_text(color = 'grey70', size = 9,
                             margin = margin(t = 0.1, r = 0.1, unit = 'cm')),
    axis.line = element_line(color = 'grey50'), 
    axis.ticks = element_line(color = 'grey50', size = 0.6), 
    axis.ticks.length = unit(0.10, 'cm')
  )
ggsave('30DayChartChallenge/images/oecd.png', width = 9.5, height = 6)  
  
