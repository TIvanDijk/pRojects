## Use {gganimate} to progressively draw a 'mathematical' pumpkin 
library(ggplot2)
library(gganimate)
library(tidyverse)
library(showtext)
library(ggtext)

npoints = 250
t = seq(0, 2*pi, length.out = npoints)

font_add_google("Creepster", 'creepster')
showtext_auto()

# define the functions
## could not find a way to use expressions and {frame_along} combined
## work around using {ggtext}
shapeFun <- function(npoints, t){
  x = 8 * sin(t)^3
  y = 8 * cos(t) - 3 * cos(t)^3 - cos(t)^5 
  return( tibble(x = x, y = y, type = '1:shape', 
                 eq = 'for <i>&theta;</i> &epsilon; [0, 2<i>&pi;</i>]: <br> 
                 <span style="color:grey20">tab</span><b><i>x</i></b> = 8 · sin<sup>3</sup>(<i>&theta;</i>) <br> 
                 <span style="color:grey20">tab</span><b><i>y</i></b> = 8 · cos(<i>&theta;</i>) - 3 · cos<sup>3</sup>(<i>&theta;</i>) - cos<sup>5</sup>(<i>&theta;</i>) '))
} 

stemFun <- function(npoints, t){
  x = 0.5*cos(t) - 0.5*sin(t) + 0.15*pi
  y = -1.5*sin(t)^3 + 1.70*pi
  return( tibble(x = x, y = y, type = '2:stem', 
                 eq = 'for <i>&theta;</i> &epsilon; [0, 2<i>&pi;</i>]: <br>
                 <span style="color:grey20">tab</span><b><i>x</i></b> = 0.5 · cos(<i>&theta;</i>) - 0.5 · sin(<i>&theta;</i>) + 0.15<i>&pi;</i> <br>
                 <span style="color:grey20">tab</span><b><i>y</i></b> = -1.5 · sin<sup>3</sup>(<i>&theta;</i>) + 1.7<i>&pi;</i>'))
}

eyesFun <- function(npoints, t,  type = 'l'){
  x = ifelse( type == 'l', 1, -1) * (2*cos(t) - cos(t)^2 + 0.5 * cos(t)^3 -0.75*pi)
  y = 2 * sin(t)^3 - sin(t)^5  + 0.5*pi
  
  return( tibble(x = x, y = y, type = paste0('3:eyes', type), 
                 eq = 'for <i>&theta;</i> &epsilon; [0, 2<i>&pi;</i>]: <br>
                 <span style="color:grey20">tab</span><b><i>x</i></b> = &#177; (2 · cos(<i>&theta;</i>) - cos<sup>2</sup>(<i>&theta;</i>) + 0.5 · cos<sup>3</sup>(<i>&theta;</i>) - 0.75&pi;) <br>
                 <span style="color:grey20">tab</span><b><i>y</i></b> = 2 · sin<sup>3</sup>(<i>&theta;</i>) - sin<sup>5</sup>(<i>&theta;</i>) + 0.5&pi;'))
}

mouthFun <- function(npoints, t){
  x = 5*sin(t) - 3 * sin(t)^3 + sin(t)^5
  y = cos(t) - 0.75*pi
  return( tibble(x = x, y = y, type = '4:mouth', 
                 eq = 'for <i>&theta;</i> &epsilon; [0, 2<i>&pi;</i>]: <br>
                 <span style="color:grey20">tab</span><b><i>x</i></b> = 5 · sin(<i>&theta;</i>) - 3 · sin<sup>3</sup>(<i>&theta;</i>) + sin<sup>5</sup>(<i>&theta;</i>) <br>
                 <span style="color:grey20">tab</span><b><i>y</i></b> = cos(<i>&theta;</i>) - 0.75&pi;'))
}

# calculate the coordinates 
df <-rbind(
        shapeFun(npoints, t),
        stemFun(npoints, t),
        eyesFun(npoints, t, type = 'l'), 
        eyesFun(npoints, t, type = 'r'), 
        mouthFun(npoints, t)) %>% 
  mutate(step = 1:n())

pumpkin <- ggplot(df, aes(x = x, y = y, color = type, fill = type)) +
  geom_polygon() +
  geom_line() +
  geom_point(color = 'white', size = 6, shape = 10, stroke = 2) +
  scale_color_manual( values = c('orange', 'brown', rep('grey20', 3))) +
  scale_fill_manual( values = c('orange', 'brown', rep('grey20', 3))) +
  scale_y_continuous(breaks = c(-4, 0, 4)) +
  transition_reveal(step, keep_last = F) +
  #shadow_wake(wake_length = 0.10, alpha = 0.5) 
  labs(title = 'Spooky mathematics', 
       subtitle = '{ifelse(as.integer(frame_along) == nrow(df), "", df$eq[as.integer(frame_along)])}',
       caption = 'by @ThomIvar') +
  theme_void() +
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = 'grey20'),
        plot.title = element_text(hjust = 0.5, size = 52, color = 'white', 
                                  family = 'creepster'), 
        plot.subtitle = element_markdown(color = 'white', size = 18, lineheight = 1.3,
                                         margin = margin(t = 0.5, unit = 'cm')),
        plot.caption = element_text(color = 'white', size = 14, family = 'creepster'),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), 
        axis.text = element_text(color = 'white', family = 'creepster', size = 12, face = 'bold'))

animate(pumpkin, fps = 20, width = 550, height = 550, duration = 20, end_pause = 80)
anim_save("pumpkin.gif") 

