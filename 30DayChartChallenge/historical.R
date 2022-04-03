# Day 3: historic
# data from:
# - Gardner (1990), Famous First Bubbles
# - Thompson (2006), The tulipmania: fact or artifact?
library(tidyverse)
library(ggtext)
library(ggimage)
library(patchwork)
library(sysfonts)
library(emojifont)

font_add_google('Kaushan Script')
font_add_google("Arvo")

# -- get data (from Thompson (2006) - Appendices I, III)
df <- data.frame(
  # day-month-year
  date = as.Date(
    c("15-12-1634", "01-02-1636", "15-05-1636", "15-06-1636", "21-07-1636", 
      "29-08-1636", "01-11-1636", "10-11-1636", '12-11-1636', '01-12-1636', 
      '12-12-1636', "01-02-1637", "03-02-1637", "05-02-1637", "09-02-1637", 
      "11-02-1637", "01-05-1637", "31-05-1637"), 
    '%d-%m-%Y'),
  # price indeces as calculared by Thompson
  index = c(22, 34, 61, 38, 51, 61, 10.2, 7.7, 97, 108, 176, 199, 202, 178, 
            148, 145, 11, 11)
)


# -- make plot
makeAnnot <- function(myDate, y, type, lab = NULL){
  myDate = as.Date(myDate, '%d-%m-%Y')
  
  if (type == 1){
    annotate('segment', x = myDate, xend = myDate, y = 5, yend = y, 
             linetype = 'dotted', colour = 'grey20')
  } else if (type == 2){
    annotate('segment', x = myDate - 160, xend = myDate, y = y, yend = y, 
             colour = 'grey20')
  } else if (type == 3){
    lab = paste0("<b style='font-size:16px;colour:#000000'>", 
                 format(myDate, '%B %Y'),"</b><br>", lab)
    geom_textbox(data = NULL, aes(x = myDate - 160, y = y), hjust = 0.025, 
                 vjust = -0.05, width = unit(2, 'in'), fill = 'transparent', 
                 box.colour = 'transparent', label = lab, alpha = 0.1, 
                 size = 2.5, color = 'grey50', family = 'Arvo')
  }
}

main <- ggplot(df, aes(x = date, y = index)) +
  geom_area(fill = 'grey70', alpha = 0.25) +
  geom_line(color = 'grey20') +
  geom_point(color = 'grey20') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 250)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b. %Y", expand = c(0.01,0)) +
  # annotations 
  makeAnnot('15-06-1635', 60, type = 1) +
  makeAnnot('15-06-1635', 60, type = 2) +
  makeAnnot('15-06-1635', 60, type = 3, 
            lab = 'period in which tulip prices started to increase.') +
  makeAnnot('10-10-1636', 75, type = 1) +
  makeAnnot('10-10-1636', 75, type = 2) +
  makeAnnot('10-10-1636', 75, type = 3, 
            lab = 'Unexpected German defeat at Wittstock, 
            eliminating the enormous German demand for tulips and putting
            the livelihoods of specutalive buyers at risk.') +
  makeAnnot('15-11-1636', 140, type = 1) +
  makeAnnot('15-11-1636', 140, type = 2) +
  makeAnnot('15-11-1636', 140, type = 3, 
            lab = 'Rumours that new relugation will be introduced that 
            allow option holders to only pay a fraction of their contract swell.') +
  makeAnnot('05-02-1637', 210, type = 1) +
  makeAnnot('05-02-1637', 210, type = 2) +
  makeAnnot('05-02-1637', 210, type = 3, 
            lab = 'Tulip trade is suspended on primary markets.') +
  labs(title = 'The Dutch Tulip Mania (1636-1637)', 
       caption = '@ThomIvar • sources: 
       <b>Thompson</b> (2006): <i>The tulipmania: Fact or artifact?</i> 
       <b>Gardner</b> (1990): <i> Famous First Bubbles </i>') +
  theme_void() +
  theme(
    text = element_text('Arvo'),
    axis.line.x = element_line(size = 1, colour = 'grey20'),
    axis.ticks.x = element_line(size = 1),
    axis.text.x = element_text(vjust = -1, colour = 'grey20'),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1,1,1,1, 'cm'),
    plot.title = element_text('Kaushan Script', colour = 'grey20', size = 22),
    plot.caption = element_textbox(size = 9, margin = unit(c(0.5,0,0,0), 'cm'), 
                                   colour = 'grey70')
  )

label = "After the tulip's introduction into Europe from Turkey in the 1500s,
the Netherlands became a hotspot for the development and production of new variaties.
Professional growers and flower investors created a market for rare varieties where 
bulbs were sold for large amounts of money. In the winter of 1636-1637 prices increased
rapidly, untill the collapse in February 1637. This <b>Dutch Tulip Mania</b>
is one of the earliest examples of an economic bubble. Although sources are limited,
<b>Thompson</b> (2006) has tried to provide an explanation for this mania."

transparent <- function(img) {
  magick::image_fx(img, expression = "0.15*a", channel = "alpha")
}

subtitle <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 0.20), fill = 'transparent') +
  geom_image(aes(x = 0.10, y = 0.17), 
             image = '30DayChartChallenge/other/tulip.png', 
             image_fun = transparent, size = 0.3, by = 'height') +
  geom_textbox(aes(x = 0, y = 0.10, label = label), hjust = 0, 
               family = 'Arvo', width = unit(6, 'in'), size = 3, 
               fill = 'transparent', box.colour = 'transparent', 
               colour = 'grey40') +
  coord_fixed() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() 

main + inset_element(subtitle, 0, 0.70, 0.55, 1) &
  plot_annotation(theme = theme(plot.background = element_rect( fill = '#fffef7', color = '#fffef7')))

ggsave('30DayChartChallenge/images/historical.png', width = 12, height = 6)

