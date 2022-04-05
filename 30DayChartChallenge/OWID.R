# Day 6: OWID
library(tidyverse)
library(gganimate)
library(magick)
library(ggtext)


# -- get data from OWID
OWID_forestgain <- read_csv("30DayChartChallenge/other/OWID_forestgain.csv") %>% 
  dplyr::filter(Year == 2015 & Code == 'OWID_WRL') %>% 
  pluck(4)

OWID_netchange <- read_csv("30DayChartChallenge/other/OWID_netchange.csv") %>% 
  dplyr::filter(Year == 2015 & Code == 'OWID_WRL') %>% 
  pluck(4)

# net change = gain - loss --> loss = gain - netchange
OWID_forestloss <- OWID_forestgain - OWID_netchange

# seconds to gain / loose 1 ha of forest
t_gain = (365 * 24 * 60 * 60) / OWID_forestgain
t_loss = (365 * 24 * 60 * 60) / OWID_forestloss

# -- make dataset for {gganimate}
n = 100
ratio = round(n * t_loss / t_gain)

df <- data.frame(
  t = 1:n, 
  xmin = 0, ymin = 0, ymax = 1,
  xmax_gain = seq(0, 1, length.out = n), 
  xmax_loss = rep(1, n)
)
df$xmax_loss[1:ratio] = seq(0, 1, length.out = ratio)

# -- make plots 
theme_both <- function(){
  theme(
    text = element_text('American Typewriter'),
    plot.background = element_rect(fill = '#fffef7', color = '#fffef7'), 
    plot.title = element_markdown(size = 31.5, color = 'grey40', face = 'bold'), 
    plot.subtitle = element_markdown(size = 16, color = 'grey60'), 
    plot.caption = element_text(size = 12, color = 'grey70'),
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )
}

p_gain <- ggplot(df) +
  geom_rect(aes(xmin = xmin, xmax = xmax_gain, ymin = ymin, ymax = ymax), 
            fill = '#326e2f') +
  coord_polar() +
  ylim(-1.5, 1) +
  labs(title = "Time it takes to <span style='color:#326e2f'>gain 1 ha of forest </span>",
       subtitle = "Measured as yearly average for 2015 to 2020", 
       caption = "purely for alignment") + 
  transition_reveal(t) +
  theme_void() +
  theme_both() +
  theme(plot.caption = element_text(size = 12, color = 'transparent'))
p_gain <- animate(p_gain, duration = t_gain, width = 580, height = 647)
p_gain

p_loss <- ggplot(df) +
  geom_rect(aes(xmin = xmin, xmax = xmax_loss, ymin = ymin, ymax = ymax), 
            fill = '#d32e05') +
  coord_polar() +
  ylim(-1.5, 1) +
  labs(title = "and to <span style='color:#d32e05'>lose 1 ha of forest </span>",
       subtitle = "purely for alignment",
       caption = "@ThomIvar • source: OWID Forests") +
  transition_reveal(t) +
  theme_void() +
  theme_both() +
  theme(plot.subtitle = element_markdown(size = 16, color = 'transparent'), 
        plot.margin = margin(1, 1, 1, 0, 'cm'))
p_loss <- animate(p_loss, duration = t_gain, width = 550, height = 647)
p_loss

# -- combine two gifs 
# solution from https://github.com/thomasp85/gganimate/wiki/Animation-Composition
a_mgif <- image_read(p_gain)
b_mgif <- image_read(p_loss)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif
anim_save('30DayChartChallenge/images/OWID.gif', new_gif)
