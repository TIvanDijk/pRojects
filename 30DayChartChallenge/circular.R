# Day 11: circular
library(tidyverse)
library(tabulizer)

# -- transform pdf to df 
df_from_pdf <- extract_tables(
  file   = "30DayChartChallenge/other/darts501.pdf", 
  method = "decide", 
  output = "data.frame")[[1]]

# manually adapt format such that it works..
mySplitFunc <- Vectorize(function(vec, dart){
  if (vec[2] == '---' | vec[dart + 1] %in% c(25, 'Bull')) return(NA)
  if (length(vec) == 3 & dart == 3) return(NA)
  
  str_remove(vec[dart + 1], "[DT]")
})

df_base <- data.frame(
  from_pdf = c(df_from_pdf[2:41, 1], df_from_pdf[2:32, 2], df_from_pdf[2:41, 3])
) %>% 
  mutate(
    goal = str_split(from_pdf, ' '),
    dart1 = mySplitFunc(goal, 1), dart2 = mySplitFunc(goal, 2), dart3 = mySplitFunc(goal, 3)
  )

cnt = as.data.frame(table(c(df_base$dart1, df_base$dart2, df_base$dart3)))
cnt$Var1 = as.numeric(paste0(cnt$Var1))

# -- prepare data
df <- data.frame(
  section = rep(LETTERS[1:4], 20), 
  id = rep(1:20, each = 4),
  scale = rep(c(5,0.05,4,0.15), 20), 
  color = rep(c('#000000', '#E3292E','#000000', '#E3292E','#F9DFBC', '#309F6A', 
                '#F9DFBC', '#309F6A'), 10),
  label = rep(c(20, 1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5), each = 4)
)
df <- left_join(df, cnt, by = c('label' = 'Var1')) %>% 
  mutate(value = 1.5 + ifelse(is.na(Freq), scale, Freq*scale), 
         value = log(value))

df_labs <- group_by(df, id) %>% 
  summarise(total = sum(value), label = unique(label))

# -- make plot
ggplot(df) +
  geom_col(aes(x = id, y = value, 
               group = id, fill = color), width = 1) +
  geom_text(data = df_labs, aes(x = id, y = 1.1 * total, label = label, size = total), 
            family = 'American Typewriter', color = 'grey60', 
            angle = c(360 + seq(0, -93, length = 6), 
                      seq(72, -72, by = -18), seq(90, 18, by = -18))) +
  labs(title = 'Most Popular Digits in Dart Checkouts', 
       subtitle = 'that are part of two- or three dart finishes (log-scale)',
       caption = '@ThomIvar • Source: checkout sheet of Darts501.com
       Note that singles, doubles and triples are counted equally') +
  scale_fill_identity() +
  scale_size(range = c(4,11)) +
  coord_polar(start = -0.15) +
  theme_void() +
  theme(
    text = element_text('American Typewriter'), 
    legend.position = 'none',
    plot.title = element_text(color = 'grey40', size = 24, face = 'bold'),
    plot.subtitle = element_text(color = 'grey60', size = 14),
    plot.caption = element_text(color = 'grey80', size = 10),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
    plot.margin = margin(1,1,1,1, 'cm')
  )

ggsave('30DayChartChallenge/images/circular.png', width = 7.5, height = 8.45)







