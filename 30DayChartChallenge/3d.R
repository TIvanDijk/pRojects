# Day 14: 3D
library(tidyverse)
library(rayshader)
library(sf)
library(ggtext)

# -- get data
df <- readRDS('30DayChartChallenge/other/borders.rds')

# -- make base plot
p1 <- ggplot(df, aes(geometry = gem.geometry)) +
  geom_sf(aes(fill = inh), color = 'grey50') +
  scale_fill_gradient2(low = '#e8e8e4', mid = '#ddbea9', high = '#cb997e', 
                       midpoint = 3e5, breaks = c(2e5, 5e5, 8e5), 
                       labels = c('200K', '500K', '800K'), 
                       name = '') +
  labs(title = 'Where do the Dutch live?', 
       caption = '@ThomIvar • source: CBS') +
  theme_void() +
  theme(
    text = element_text('American Typewriter'),
    plot.background = element_rect(fill = '#fffef7', color = '#fffef7'),
    plot.title = element_text(size = 18, margin = margin(b = 1, unit = 'cm')),
    plot.caption = element_text(size = 10), 
    plot.margin = margin(1,1,1,1, 'cm'),
    legend.position = 'top'
  )


# -- make 3D with rayshader
phivechalf = 30 + 45 * 1/(1 + exp(seq(-7, 20, length.out = 90)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 45 * sin(seq(0,359,length.out = 180) * pi/180)
zoomvec = 0.45 + 0.3 * 1/(1 + exp(seq(-5, 20, length.out = 90)))
zoomvecfull = c(zoomvec, rev(zoomvec))

plot_gg(p1, width = 4.1, height = 6, shadow_intensity = 0.25, scale = 100)
render_movie(filename = '30DayChartChallenge/images/3d.gif', type = "custom", 
             frames = 180,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)
