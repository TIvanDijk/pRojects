# Day 2: pictograms
library(tidyverse)
library(emojifont)
library(sysfonts)
library(ggtext)
#load.fontawesome()

# -- add some fonts
font_add_google("Arvo")       # main text
font_add_google("Poppins")    # for Hindi
font_add_google("Indie Flower") # title

# -- make datasets
langs = c('english' = 6, 'chinese' = 7, 'hindi' = 12, 
          'spanish' = 14, 'french' = 28)

df <- data.frame(
  x = unlist(sapply(langs, function(x) seq(0, 2, length.out = x))),
  y = rep(c(2, 1.45, 0.95, 0.55, 0.2), times = langs), 
  label = rep(fontawesome("fa-flag"), sum(langs)), 
  size = rep(c(24, 18, 9, 8, 4), times = langs), 
  color = 'grey70'
)
# change colour for first icon in the row
df[c(1, cumsum(langs)[-length(langs)] + 1), 'color'] = 
  c('#012169', '#cd071e', '#138808', '#F1BF00', '#002395')

df.text <- data.frame(
  x = 0,
  y = unique(df$y) + c(0.25, 0.20, 0.15, 0.15, 0.125), 
  label = c(
    "<b style='color:#012169'>1 out of 6</b> persons can understand this", 
    "<b style='color:#cd071e'>七个人中有一</b>个能理解这一点", 
    "<b style='color:#138808'>12 में से 1</b> व्यक्ति उसे समझसकता है",
    "<b style='color:#F1BF00'>1 de cada 14</b> personas puede entender esto",
    "<b style='color:#002395'>1 personne sur 24</b> peut comprendre cela"
  ), 
  family = c('Arvo', 'wqy-microhei', 'Poppins', 'Arvo', 'Arvo')
)

# -- make plot
ggplot(df, aes(x = x, y = y)) +
  geom_text(aes(label = label, size = size, color = color), 
            alpha = ifelse(df$color == 'grey70', 0.35, 1),
            family = 'fontawesome-webfont') +
  geom_textbox(data = df.text, aes(label = label, family = family), 
               hjust = 0, fill = 'transparent', box.color = 'transparent',
               width = unit(4.5, 'inch'), box.padding = unit(rep(0, 4), 'pt'), 
               color = 'grey50', size = 5) +
  scale_size_identity() +
  scale_color_identity() +
  coord_fixed() +
  labs(title = 'Most-spoken Languages in the World', 
       subtitle = 'Either natively or as a second language',
       caption = '@ThomIvar • sources: CIA (The World Factbook, 2020), Google Translate') +
  xlim(-0.1, 2.1) +
  theme_void() +
  theme(
    text = element_text('Arvo'),
    plot.background = element_rect(fill = '#fffef7', color = '#fffef7'), 
    plot.title = element_text('Indie Flower', size = 30, color = 'grey30', hjust = 0.5), 
    plot.subtitle = element_text('Indie Flower', hjust = 0.5, 
                                 size = 18, color = 'grey50'),
    plot.caption = element_text(size = 8, color = 'grey80'),
    plot.margin = margin(1, 1, 1, 1, 'cm')
  )
ggsave('30DayChartChallenge/images/pictograms.png', width = 7.5, height = 8.05)
