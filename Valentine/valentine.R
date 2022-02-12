# -- File to create valentine.gif -- 
library(tidyverse)
library(gganimate)

# -- 1: create dataset --
n.draw = 100  
n.fill = 100
t = seq(-pi, pi, length.out = n.draw)

# drawing of the heart
df <- data.frame(
  x = 18 * sin(t)^3,
  y = 13 * cos(t) - 5 * cos(2*t) - 2 * cos(3*t) - cos(4 * t)
)

# add effects after drawing is completed 
make_label <- function(index, label = "Happy Valentine's Day!"){
  if (index < n.draw) return('')
  
  opts = floor(seq(0, n.fill/2, length.out = str_length(label))) + n.draw
  return(substr(label, 1, max(which(index >= opts))))
}

df <- do.call(rbind, lapply(seq(nrow(df) + n.fill), function(i) {
  df$frame <- i
  df$alpha <- c(rep(0, n.draw), seq(0.1, 1, length.out = n.fill/2), rep(1, n.fill/2))[i]
  df$txt <- make_label(i)
  df[1:min(i, n.draw),]
}
))
df <- df[df$frame > 1,]


# 2: make graph
p <- ggplot(df, aes(x = x, y = y)) +
  geom_path(aes(alpha = 1 - alpha), color = '#f8f4ef', size = 1) +
  geom_polygon(aes(x = x - 0.50, y = y - 0.70, alpha = alpha), fill = '#702632') +
  geom_polygon(aes(alpha = alpha), fill = '#912f40') +
  geom_text(aes(y = 17.5 - 0.10, x = 0 - 0.20, label = txt), hjust = 0.5, 
            size = 12, color = '#702632', family = 'American Typewriter') +
  geom_text(aes(y = 17.5, x = 0, label = txt), hjust = 0.5, 
            size = 12, color = '#912f40', family = 'American Typewriter') +
  labs(caption = '@ThomIvar') +
  scale_alpha_identity() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = '#352828', color = '#352828'),
        plot.margin = margin(1,1,1,1, 'cm'), 
        plot.caption = element_text('American Typewriter', color = '#f8f4ef')) + 
  transition_states(frame)

animate(p, nframes = 2*(n.draw+n.fill), duration = 8)
anim_save('valentine.gif')


