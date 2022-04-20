# prep for day 20
library(tidyverse)

# -- get data
names <- read_delim('30DayChartChallenge/other/q05a.txt', ';')
colnames(names) <- c('sex', 'year', 'month', 'name', 'count')

# names with only a different special character are considered 'equal'
names$name = str_replace_all(names$name, '\xeb', 'e')

names_prep <- group_by(names, name, year) %>% 
  summarise(total = sum(count)) %>% 
  mutate(year = as.numeric(year)) %>% 
  dplyr::filter(year <= 2012) %>% 
  ungroup() %>% 
  pivot_wider(names_from = name, values_from = total) %>% 
  arrange(year)

# -- save data
write_csv(names_prep, '30DayChartChallenge/other/newtool.csv')
