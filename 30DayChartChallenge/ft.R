# Day 24: Financial Times
# inspiration: https://twitter.com/jburnmurdoch/status/1250538655707430913/photo/1
library(ftplottools)
library(rvest)
library(zoo)

# -- scrape data 
readRanking <- function(round){
  link = paste0("https://www.voetbal.com/wedstrijdgegevens/ned-eredivisie-2021-2022-spieltag/", 
                round, "/")
  tables <- read_html(link) %>% 
    html_nodes('body') %>% 
    xml2::xml_find_all("//table[contains(@class, 'standard_tabelle')]") %>% 
    html_table()
  
  # some cleaning up
  df = tables[[2]]
  df$round = round
  df$`#` = zoo::na.approx(df$`#`)
  df = df[, -2]
  return(df)
}

df = readRanking(1)
for (r in 2:29){
  df = bind_rows(df, readRanking(r))
}

df_base = df[, c('round', '#', 'Team')]
names(df_base)[3] = 'group'

# -- make plot
ggplot(df, aes(x = round, y = -`#`, color = Team)) +
  geom_path(data = df_base, aes(x = round, y = -`#`, group = group), 
            color = ft_colors('black-20'), size = 0.4) +
  geom_path(color = ft_colors('oxford-60'), size = 0.8) +
  geom_point(data = readRanking(29), color = ft_colors('oxford-60'), size = 1.5) +
  scale_y_continuous(breaks = c(-1, -10, -18), labels = c('1', '10', '18')) +
  scale_x_continuous(breaks = c(5, 15, 25)) +
  facet_wrap(~ Team) +
  labs( title = 'Ranking of Eredivisie clubs per matchweek', 
        subtitle = 'Season 2021/22, until matchweek 29', 
        caption = 'graphic: @ThomIvar\nsource: voetbal.com\ntheme: Financial Times', 
        x = 'Matchweek', y = 'Rank') +
  ft_theme() +
  theme(plot.background = element_rect(fill = ft_colors('paper'), color = ft_colors('paper')), 
        strip.text = element_text(hjust = 0, color = ft_colors('oxford-60'), face = 'bold'), 
        plot.margin = margin(1, 1, 0.25, 1, 'cm'), 
        plot.caption = element_text(size = 8, color = ft_colors('black-30')))
ggsave('30DayChartChallenge/images/ft.png', width = 8.5, height = 6)




