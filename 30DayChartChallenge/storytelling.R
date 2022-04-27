# day 29: storytelling
library(tidyverse)
library(ggtext)
library(patchwork)

# -- creation of fake data 
main_labels <- data.frame(
  x = rep(0.6, 3), y = 3:1,
  text = c(
    "<b style='font-size:30px;color:#00b4d8'>1.</b> 
    <b style='font-size:18px; color:#00b4d8'>Y-axis does not start at 0</b><br>
    In this graph it appears that Mr. Lucas sells 3-4 times as less than e.g. Miss
    Brahms, even though this difference is much smaller (200 vs 360)", 
    "<b style='font-size:30px;color:#00b4d8'>2.</b> 
    <b style='font-size:18px; color:#00b4d8'>Y-axis with a log-scale</b><br>
    Often we interperet graphs as if they have a linear scale. In this graph it can
    therefore appear as if there are hardly any differences between our salespersons.",
    "<b style='font-size:30px;color:#00b4d8'>3.</b> 
    <b style='font-size:18px; color:#00b4d8'>Y-axis with a fair linear scale</b><br>
    In the current setting this is probably the best way to visualise the performance
    of our salespersons. Mrs. Slocombe is the best performing salesperson, selling 
    roughly 2.5 times as much as Mr. Lucas."
  )
)

fake_data <- data.frame(
  name = c('Mrs. Slocombe', 'Mr. Humphries', 'Miss Brahms', 'Mr. Lucas'), 
  sales = c(520, 440, 360, 200)
)

# -- make plot
main <- ggplot(main_labels) +
  geom_segment(aes(x = x + 0.85, xend = x + 0.85, y = y - 0.35, yend = y + 0.35),
               linetype = 'dotted', color = 'grey30') +
  geom_textbox(aes(x = x, y = y, label = text), hjust = 0.5, color = 'grey50',
               width = unit(3, 'inch'), family = 'American Typewriter', 
               size = 4, fill = 'transparent', box.colour = 'transparent', 
               box.padding = unit(c(0,0,0,0), 'pt')) +
  labs(title = "Don't forget to check the <span style='color:#00b4d8'>Y-axis</span>", 
       subtitle = "Three stories, one fake dataset", 
       caption = "@ThomIvar") +
  ylim(0.5, 3.35) + xlim(0, 4) +
  coord_fixed(clip = 'off') +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = 'grey50'), 
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'),
    plot.margin = margin(1, 1, 1, 1, 'cm'),
    plot.title = element_markdown(face = 'bold', size = 24, color = 'grey30'),
    plot.subtitle = element_text(size = 14), 
    plot.caption = element_text(colour = 'grey70')
  )

p1 <- ggplot(fake_data, aes(x = fct_reorder(name, sales), y = sales-175)) +
  geom_col(fill = ifelse(fake_data$name == 'Mr. Lucas', '#00b4d8' ,'grey70'), color = 'grey50') +
  geom_text(aes(x = name, y = 0, label = name), vjust = -0.25, 
            family = 'American Typewriter', size = 3, color = 'grey95',
            fontface = 'bold') +
  labs(y = 'Sales') +
  scale_y_continuous(expand = c(0, 0), breaks = c(25, 125, 225, 325),
                     labels = c(200, 300, 400, 500)) +
  coord_cartesian(clip = 'off') +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = 'grey50'),
    plot.background = element_rect( fill = 'transparent', color = 'transparent'),
    axis.line.y = element_line(),
    axis.text.y = element_text(), 
    axis.title.y = element_text(angle = 90, margin = margin(r = 5)), 
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )

p2 <- ggplot(fake_data, aes(x = fct_reorder(name, sales), y = log(sales))) +
  geom_col(fill = 'grey70', color = 'grey50') +
  geom_text(aes(x = name, y = 0, label = name), vjust = -0.25, 
            family = 'American Typewriter', size = 3, color = 'grey95',
            fontface = 'bold') +
  labs(y = 'Sales (log)') +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = 'grey50'),
    plot.background = element_rect( fill = 'transparent', color = 'transparent'),
    axis.line.y = element_line(),
    axis.text.y = element_text(), 
    axis.title.y = element_text(angle = 90, margin = margin(r = 5, l = 15)), 
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )

p3 <- ggplot(fake_data, aes(x = fct_reorder(name, sales), y = sales)) +
  geom_col(fill = ifelse(fake_data$name == 'Mrs. Slocombe', '#00b4d8' ,'grey70'), color = 'grey50') +
  geom_text(aes(x = name, y = 0, label = name), vjust = -0.25, 
            family = 'American Typewriter', size = 3, color = 'grey95',
            fontface = 'bold') +
  labs(y = 'Sales') +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = 'off') +
  theme_void() +
  theme(
    text = element_text('American Typewriter', color = 'grey50'),
    plot.background = element_rect( fill = 'transparent', color = 'transparent'),
    axis.line.y = element_line(),
    axis.text.y = element_text(), 
    axis.title.y = element_text(angle = 90, margin = margin(r = 5)), 
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )

main + 
  inset_element(p1, 0.35, 0.667, 1, 1) +
  inset_element(p2, 0.35, 0.333, 1, 0.667) +
  inset_element(p3, 0.35, 0, 1, 0.333) 
ggsave('30DayChartChallenge/images/storytelling.png', width = 9.70, height = 8)




