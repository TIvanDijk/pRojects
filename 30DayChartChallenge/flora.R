# day 4: flora
library(tidyverse)
library(ggtext)

# -- make shapes
npoints = 250
t = seq(0, pi, length.out = npoints)

# main part of the carrot
df <- data.frame(
  x_right = 6*sin(t)^2,
  x_left = -0.35*sin(t)^2 + 0.02,
  y = cos(t)
)
df_min <- df[df$y < 0, ]
df_max <- df[df$y > 0, ]

# stripes on the carrot 
df_stripes <- df[seq(round(npoints/15), npoints, length.out = 14)[-14],]

# carrot greens
df_greens <- data.frame(
  x = -0.20, y = 0, xend = c(-1.5, -2.25, -1.5), yend = c(-0.5, 0, 0.5),  
  size = c(4, 6, 4)
)

# annotation
df_annot <- data.frame(
  x.mid = c(0.675, 1.85, 3.35, 5), 
  yend = c(-1.3, 1.2, -1.1, 1), 
  num = as.character(1:4), 
  col = c('#d00000', '#dc2f02', '#e85d04', '#f48c06'), 
  boxtext = c(
    "The world produces <b style='color:#d00000'>40 million tons</b> of carrots every year", 
    "Water makes up <b style='color:#dc2f02'>88%</b> of a carrot's mass", 
    "The longest carrot ever grown in history so far measured around <b style='color:#e85d04'>6 meters</b> long",
    "Archaeological evidence points to carrots grown for food in Switzerland and Germany as far back as <b style='color:#f48c06'>3000 BC</b>"
  )
)


# -- create plot
set.seed(42)
ggplot(df) +
  # right side of carrot
  geom_area(data = df_min, aes(x = x_right, y = y), fill = '#ff9500') +
  geom_area(data = df_max, aes(x = x_right, y = y), fill = '#ff9500') +
  geom_path(aes(x = x_right, y = y), color = '#ff7b00', size = 2) +
  # greens of carrot
  geom_segment(data = df_greens, aes(x = x, xend = xend, y = y,
                                     yend = yend, size = size), 
               colour = '#a1c349', lineend = 'round') +
  # left side of carrot
  geom_area(data = df_min, aes(x = x_left, y = y), fill = '#ff9500') +
  geom_area(data = df_max, aes(x = x_left, y = y), fill = '#ff9500') +
  geom_path(aes(x = x_left, y = y), color = '#ff7b00', size = 2) + 
  # add stripes 
  geom_segment(data = df_stripes, aes(x = x_right, xend = x_right, 
                                      y = y + ifelse(y < 0, 0.02, -0.02), 
                                      yend = y / runif(13, 3, 5)), 
               size = runif(13, 2, 3.5), colour = '#ff7b00', lineend = 'round') +
  # add group lines
  geom_segment(data = data.frame(x = seq(1.1, 4.1, 1.5), y = 1), 
               aes(x = x, xend = x, y = -1, yend = 1), 
               size = 3, color = '#fffef7') +
  # add annotations
  geom_point(data = df_annot, aes(x = x.mid, y = 0),  
             color = 'grey60', size = 2.5) +
  geom_segment(data = df_annot,
               aes(x = x.mid, xend = x.mid, y = 0, yend = yend), 
               color = 'grey60', size = 1) +
  geom_point(data = df_annot, aes(x = x.mid, colour = col,
                                  y = yend + ifelse(yend < 0, -0.225, 0.225)), 
             shape = 1, size = 14, stroke = 2) +
  geom_text(data = df_annot, aes(x = x.mid,  label = num, colour = col,
                                 y = yend + ifelse(yend < 0, -0.225, 0.225)), 
            size = 8.5, family = 'American Typewriter') +
  geom_textbox(data = df_annot, aes(x = x.mid, label = boxtext,
                                    y = yend + ifelse(yend < 0, -0.225, 0.225)),
               hjust = -.15, fill = 'transparent', box.colour = 'transparent', 
               family = 'American Typewriter', color = 'grey40') +
  coord_fixed() +
  labs(title = "4 Facts about <span style='color:#ff7b00'>Carrots</span>", 
       caption = '@ThomIvar • source: facts.net') +
  ylim(-2, 2) + xlim(-2.25, 7) +
  scale_size_identity() +
  scale_color_identity() +
  theme_void() +
  theme(legend.position = 'none', 
        text = element_text('American Typewriter'),
        plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
        plot.margin = margin(1,1,1,1, 'cm'), 
        plot.title = element_markdown(color = 'grey30', size = 28), 
        plot.caption = element_text(color = 'grey70', size = 10))
ggsave('30DayChartChallenge/images/flora.png', width = 10, height = 5.45)
