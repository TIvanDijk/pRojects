# day 15: multivariate 
library(tidyverse)
library(ggtext)
library(sysfonts)
library(emojifont)
library(extrafont)

# -- get data
disney_data <- read_delim("30DayChartChallenge/other/disney.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE,
                     locale = locale(decimal_mark = ",", grouping_mark = ".")) %>% 
  mutate(scale = round(Visitors / max(Visitors) * 20, 0))


# -- make plot
label_big = "The Magic Kingdom Park at Walt Disney World has nearly 
<b style='color:#e63946; font-size:20px'>seven million</b> yearly visitors..."

label_small = "...whereas Blizzard Beach has only 
<b style='color:#e63946; font-size:20px'>300 thousand</b> yearly visitors."

font_add_google('Mouse Memoirs')

ggplot(disney_data, aes(x = Built, y = TicketPrice, size = scale)) +
  # create 'Mickey'
  geom_point(color = 'grey10') +
  geom_point(aes(x = Built - 0.1 * scale, y = TicketPrice + 0.25 * scale, size = 0.75*scale), color = 'grey10') +
  geom_point(aes(x = Built + 0.1 * scale, y = TicketPrice + 0.25 * scale, size = 0.75*scale), color = 'grey10') +
  # annotate
  geom_textbox(data = NULL, aes(x = 1970, y = 107.5, size = 4, label = label_big), 
               family = 'Mouse Memoirs', color = 'grey40', box.color = '#fffef7',
               fill = '#f8fef7', width = unit(2.5, 'inch')) +
  geom_textbox(data = NULL, aes(x = 1995, y = 72.5, size = 4, label = label_small), 
               family = 'Mouse Memoirs', color = 'grey40', box.color = '#fffef7',
               fill = '#f8fef7', width = unit(2.0, 'inch')) +
  # theme & stuff
  scale_size_identity() +
  coord_cartesian(clip = 'off') +
  labs(title = "Most <b style='font-size:48px; color:#e63946'>Popular</b> Disney Parks", 
       y = "<b style='color:#e63946'>Price</b> of a Ticket (US$)",
       x = "<b style='color:#e63946'>Year</b> of Construction", 
       caption = '@ThomIvar • Source: The Park Database') +
  theme_void() +
  theme(
    text = element_text('Mouse Memoirs'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1,1,1,1, 'cm'),
    plot.title = element_textbox_simple(colour = 'grey30', size = 26), 
    plot.caption = element_text(colour = 'grey70', size = 11),
    panel.grid.major = element_line(linetype = 'dotted', color = 'grey80'),
    axis.line = element_line(colour = 'grey30'), 
    axis.text = element_text(colour = 'grey70'),
    axis.title = element_textbox_simple(colour = 'grey50', size = 16), 
    axis.title.y = element_textbox_simple(orientation = 'left-rotated', hjust = 0), 
    axis.ticks = element_line(colour = 'grey30'), 
    axis.ticks.length = unit(0.2, 'cm')
  )
ggsave('30DayChartChallenge/images/multivariate.png', width = 9, height = 6)
