# day 25: trend
library(tidyverse)

# -- prepare data 
pred <- read_delim("30DayChartChallenge/other/prognose_cbs.csv", ";", 
                   escape_double = FALSE, trim_ws = TRUE, 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  transmute(
    value_type = `Prognose(-interval)`, 
    year = Jaar,
    `immigration` = Immigratie, 
    `grey` = Grijze_druk,
    `life_exp` = Levensverwachting_bij_geboorte, 
    `birth` = Levend_geboren,
  ) %>% 
  pivot_longer(cols = -c(1,2), names_to = 'type', values_to = 'value')

pred_line <- subset(pred, value_type == 'Prognose')
pred_ci <- subset(pred, value_type %in% 
                    c('Ondergrens 95%-prognose-interval', 'Bovengrens 95%-prognose-interval')) %>% 
  pivot_wider(names_from = value_type)
names(pred_ci)[3:4] = c('low', 'high')

hist <- read_delim("30DayChartChallenge/other/data_cbs.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE, 
                   locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  transmute(
     year = Jaar,
    `immigration` = Immigratie * 1000, 
    `grey` = Grijze_druk, 
    `life_exp` = Levensverwachting_bij_geboorte, 
    `birth` = Levend_geboren * 1000) %>% 
  pivot_longer(cols = -1, names_to = 'type', values_to = 'value')

wrap_labels = data.frame(
  x = rep(2021.5, 4),
  y = c(150000, 22.5, 1.2e5, 78.5), 
  label = c('Births', 'Grey population pressure', 'Immigration', 'Life expectancy (at birth)'), 
  type = c('birth', 'grey', 'immigration', 'life_exp')
)

own_y_labels = data.frame(
  x = rep(1998, 11), 
  y = c(160000, 240000, 319000, 20, 40, 60, 1.5e5, 3e5, 80, 85, 90), 
  label = c('160K', '240K', '320K', '20%', '40%', '60%', '150K', '300K', '80', '85', '90'),
  type = c( rep('birth', 3), rep('grey', 3), rep('immigration', 2), 
            rep('life_exp', 3))
)

# -- make plot

ggplot(hist, aes(x = year)) +
  # CI-intervals
  geom_vline(xintercept = 2021, color = 'grey50', size = 0.8, alpha = 0.75) +
  geom_ribbon(data = pred_ci, aes(ymin = low, ymax = high, fill = type), 
              alpha = 0.25) +
  geom_line(data = pred_line, aes(y = value, color = type), size = 1, linetype = 2) +
  # data points 
  geom_point(aes(y = value, color = type), size = 1.75, alpha = 0.8) +
  # annotations
  geom_text(data = wrap_labels, aes(x = x, y = y, label = label, color = type), 
            hjust = 0, family = 'American Typewriter', fontface = 'bold', size = 4.5) +
  geom_text(data = own_y_labels, aes(x = x, y = y, label = label), 
            hjust = 1, family = 'American Typewriter', color = 'grey60', size = 3) +
  # styling
  labs(title = 'Dutch population trends', 
       subtitle = 'historical values (left), predicted values + 95% confidence interval (right)',
       caption = '@ThomIvar • source: CBS',
       x = '', y = '') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous() +
  scale_color_manual(values = c('#2c7da0', '#468faf', '#61a5c2', '#89c2d9')) +
  scale_fill_manual(values = c('#2c7da0', '#468faf', '#61a5c2', '#89c2d9')) +
  facet_wrap(~type, scales = 'free_y') +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(
    text = element_text('American Typewriter', color = 'grey40'),
    strip.text = element_blank(),
    axis.text.y = element_blank(), axis.text.x = element_text(color = 'grey60'),
    panel.spacing = unit(1.5, 'lines'), panel.spacing.x = unit(2.0, 'lines'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1, 1, 1, 1, 'cm'),
    plot.title = element_text(color = 'grey30', size = 26, face = 'bold'), 
    plot.subtitle = element_text(color = 'grey50'), 
    plot.caption = element_text(color = 'grey70'),
    panel.grid = element_line(color = 'grey80', linetype = 'dotted'), 
    legend.position = 'none')
ggsave('30DayChartChallenge/images/trend.png', width = 9, height = 6)  
  
