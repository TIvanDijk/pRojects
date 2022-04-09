# Day 9: Statistics 
library(tidyverse)
library(ggtext)
library(sysfonts)
library(emojifont)

# -- make dataset
n = 200
mod_sgment = 16   # out of 1:n
chart_pos = 60    # out of 1:n

# selected values for shape parameters (alpha, beta)
params <- data.frame(
  alpha = c(1, 2, 2, 5, 1, 0.25), 
  beta = c(1, 2, 5, 2, 0.5, 0.25)
)

df <- do.call(rbind, lapply(seq(nrow(params)), function(i) {
  x = seq(0.02, 0.98, length.out = n)
  sgment = ((1:n %% mod_sgment) == 0)
  sgment[n] = F   # no segment on x = 1
  data.frame(
    x = x,
    y = dbeta(x, params$alpha[i], params$beta[i]), 
    type = paste0("<span style='font-size:10px'>(", i, ")</span>",  
                  ' α = ', params$alpha[i], ', β = ', params$beta[i]),
    sgment = sgment)
}))

df_chart <- group_by(df, type) %>% 
  mutate(index = 1:n(), 
         angle = (lead(y) - lag(y)) / (lead(x) - lag(x)), 
         angle = atan(angle) * (180 / pi))  %>% 
  dplyr::filter(index %in% c(chart_pos - n/25, chart_pos, chart_pos + n/25))

# -- make plot
font_add_google("Arvo")

ggplot(df, aes(x = x, y = y)) +
  geom_area(fill = 'grey90', colour = 'grey30', size = 1.5) +
  geom_segment(data = subset(df, sgment == T) ,
               aes(x = x, xend = x, y = 0, yend = y), 
               size = 1.25, color = 'grey30') +
  geom_text( data = df_chart , aes(x = x, y = y, angle = angle * 0.30),
             label = fontawesome('fa-train'), family = 'fontawesome-webfont', 
             vjust = -0.1, color = '#457b9d') +
  geom_textbox(data = data.frame(type = unique(df$type)), 
               aes(x = 0.5, y = 3, label = type), fill = 'transparent',
               color = 'grey50', size = 3.5, box.color = 'transparent',
               width = unit(1.5, 'inch')) +
  scale_x_continuous(expand = c(0,0)) +
  #coord_fixed() +
  facet_wrap(~type, nrow = 3) +
  labs(title = "What if the <b style='color:grey85'>Beta distribution</b> 
       was a <b style='color:grey35'>Rollercoaster</b>?", 
       caption = '@ThomIvar') +
  theme_void() +
  theme(
    text = element_text('Arvo'),
    plot.title = element_textbox_simple(size = 22, colour = 'grey50'),
    plot.caption = element_text(size = 9, colour = 'grey70'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1,1,1,1, 'cm'), 
    strip.text = element_blank(),
    panel.spacing = unit(1, 'cm')
  )
ggsave('30DayChartChallenge/images/statistics.png', width = 9, height = 8)
