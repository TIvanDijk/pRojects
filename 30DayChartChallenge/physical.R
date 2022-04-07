# Day 7: Physical
library(tidyverse)

# -- get data
df <- read_delim("30DayChartChallenge/other/CBS_age.csv", 
           ";", escape_double = FALSE, trim_ws = TRUE, skip = 5) %>% 
  transmute(
    age = sprintf("%02d", as.numeric(str_remove(Leeftijd, ' jaar'))), 
    age_stem = as.numeric(substring(age, 1, 1)), 
    age_leaf = as.numeric(substring(age, 2, 2)), 
    total_men = `Totaal Man`,
    total_women = `Totaal Vrouw`
  ) %>% 
  dplyr::filter(as.numeric(age) < 100)

df_stem <- df %>% 
  group_by(age_stem) %>% 
  summarise(total_men = sum(total_men), total_women = sum(total_women)) %>% 
  mutate(ratio_men = total_men / max(total_men), 
         ratio_women = total_women / max(total_women))

df <- left_join(df, df_stem[, c('age_stem', 'ratio_men', 'ratio_women')])


# -- make plot
ggplot(df) +
  # stem
  geom_text(data = df_stem, aes(x = 0, y = age_stem, label = age_stem), 
            family = 'American Typewriter', color = '#415a77', 
            fontface = 'bold', size = 11) +
  geom_vline(xintercept = -0.75, color = '#415a77') + 
  geom_vline(xintercept = 0.75, color = '#415a77') +
  # leafs
  geom_text(aes(x = (age_leaf) * ratio_women + 1.5, y = age_stem, 
                label = age_leaf, size = total_women, color = total_women),
            family = 'American Typewriter') +
  geom_text(aes(x = (-age_leaf) * ratio_men - 1.5, y = age_stem, 
                label = age_leaf, size = total_men, color = total_men),
            family = 'American Typewriter') +
  # annotations
  coord_cartesian(ylim = c(0, 9), clip = 'off') +
  annotate('text', x = -0.75, y = 9.75, label = 'men', hjust = 1,
           family = 'American Typewriter', color = '#1b263b') +
  annotate('text', x = 0.75, y = 9.75, label = 'women', hjust = 0,
           family = 'American Typewriter', color = '#1b263b') +
  annotate('text', x = 0, y = -0.60, label = 'decades', size = 3,
           family = 'American Typewriter', color = 'grey70') +
  annotate('text', x = 4, y = -0.60, label = 'years', size = 3,
           family = 'American Typewriter', color = 'grey70') +
  annotate('text', x = -4, y = -0.60, label = 'years', size = 3,
           family = 'American Typewriter', color = 'grey70') +
  # other styling
  labs(title = 'Age Distribution of the Netherlands',
       subtitle = 'Stem-and-leaf diagram scaled by group size, as of 2021', 
       caption = '@ThomIvar • source: CBS') +
  scale_size_area(max_size = 9) +
  scale_color_gradient(low = 'grey90', high = '#778da9') +
  theme_void() +
  theme(legend.position = 'none', 
        text = element_text('American Typewriter'),
        plot.title = element_text(hjust = 0.5, size = 26, colour = '#1b263b'),
        plot.subtitle = element_text(hjust = 0.5, size = 14, colour = '#415a77', 
                                     margin = margin(0,0,0.75,0, 'cm')),
        plot.caption = element_text(size = 9, colour = 'grey70'),
        plot.background = element_rect( fill = '#fffef7', color = '#fffef7'), 
        plot.margin = margin(1,1,1,1, 'cm'))

ggsave('30DayChartChallenge/images/physical.png', width = 8, height = 6)

