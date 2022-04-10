# Day 10: experimental
library(tidyverse)
library(gganimate)
library(ggimage)
library(ggtext)
library(showtext)


# -- make dataset
n = 100
max_mean = 1.25
small_step_mean = 0.6
min_sd = 0.70
small_step_sd = 0.90

small_move = seq(max_mean, small_step_mean, length.out = 8)
small_move_sd = seq(min_sd, small_step_sd, length.out = 8)
move = seq(0, max_mean, length.out = 20)
move_sd = seq(1, min_sd, length.out = 20)
full_move = seq(-1 * max_mean, max_mean, length.out = 40)

params <- data.frame(
  mean = c(move, small_move, rev(small_move), -1 * full_move, -1 * small_move, 
           -1 * rev(small_move), -1 * rev(move)), 
  sd = c(move_sd, small_move_sd, rev(small_move_sd), rev(move_sd), move_sd, 
         small_move_sd, rev(small_move_sd), rev(move_sd))
)

df <- do.call(rbind, lapply(seq(nrow(params)), function(i){
  x = seq(-3, 3, length.out = n)
  y = dnorm(x, mean = params$mean[i], sd = params$sd[i])
  
  data.frame(
    x = x, y = y, type = i,
    pos.x = params$mean[i], 
    pos.y = median(y),
    mean = paste(round(params$mean[i], 2)),
    sd = paste(round(params$sd[i], 2))
  )
}))

df_eyes <- group_by(df, type) %>% slice(1)

# -- make plot
img_link = '30DayChartChallenge/other/eyes.png'
font_add_google("Slackey")
showtext_auto()

p1 <- ggplot(df) +
  geom_area(aes(x = x, y = y), fill = '#ff808b', color = 'grey30') +
  geom_text(data = df_eyes, 
            aes(x = -3, y = 0.5, 
                label = paste0('mean: ', mean, '\nsd: ', sd)),
            hjust = 0, color = 'grey50') +
  geom_image(data = df_eyes,
             aes(image = img_link, x = pos.x, y = pos.y), size = 0.25) +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  labs(title = "A dancing <span style='color:grey30'>normal</span> or 
       <span style='color:#ff808b'>Patrick Star?</span>", 
       caption = '@ThomIvar') +
  theme_void() +
  theme(
    text = element_text('Slackey'),
    plot.title = element_textbox_simple(size = 28, colour = 'grey50'),
    plot.caption = element_text(size = 9, colour = 'grey70'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1,1,1,1, 'cm'), 
    axis.line.x = element_line(color = 'grey30', size = 1.5),
    axis.text.x = element_text(color = 'grey70', margin = margin(0.35, 0,0,0,'cm')), 
    axis.text.y = element_text(color = 'grey70', hjust = -1)
  ) +
  transition_time(type)

anim_save('30DayChartChallenge/images/experimental.gif',
          p1, nframes = 112, duration = 4)


