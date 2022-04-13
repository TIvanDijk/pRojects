# Day 13: correlation
library(tidyverse)
library(ggflags)
library(countrycode)
library(ggtext)

# -- get data
alcohol <- read_csv("30DayChartChallenge/other/worldbank_alcohol.csv", skip = 3) %>% 
  select('name' = `Country Name`, 'iso3c' = `Country Code`,  'alc_cons' = `2018`)

health <- read_csv('30DayChartChallenge/other/worldbank_life_expectancy.csv', skip = 3) %>% 
  select('name' = `Country Name`, 'iso3c' = `Country Code`, 'life_exp' = `2018`)

# cherry-picking of countries 
df <- left_join(alcohol, health) %>% 
  dplyr::filter(
    name %in% c('Spain', 'France', 'Netherlands', 'Germany', 'Sweden', 'Norway', 
                'Italy', 'Japan', 'Poland', 'Costa Rica', 'Brazil', 'Paraguay', 
                'Venezuela, RB', 'Honduras', 'Czech Republic', 'Lithuania', 'Rwanda', 
                'Gabon', 'Fiji', 'Somalia', 'Chad', 'Cuba', 'Romania', 'Belarus', 
                'North Macedonia', 'Cameroon', "Cote d'Ivoire", 'United States', 
                'Portugal', 'United Kingdom', 'Turkey', 'Pakistan', 'Kenya', 
                'Zimbabwe', 'Iraq', 'Mali', 'Bolivia', 'South Africa', 'Ukraine',
                'Albania', 'Saudi Arabia', 'India', 'Suriname', 'Mexico', 
                'Bangladesh', 'Azerbaijan', 'Colombia', 'Argentina', 'Congo')
  ) %>% 
  mutate(iso2c = tolower(countrycode(iso3c, origin = 'iso3c', destination = 'iso2c'))) %>% 
  drop_na()

# -- make plot
ggplot(df, aes(y = alc_cons, x = life_exp)) +
  geom_smooth(method = "lm", se = FALSE, color = 'grey50', linetype = 2) +
  geom_point() +
  geom_flag(aes(country = iso2c), size = 6) +
  geom_text(aes(label = toupper(iso2c)), vjust = -1, hjust = 0.5, 
            family = 'American Typewriter', color = 'grey40', size = 3) +
  labs(x = 'Life expectancy at birth (years)', 
       y = 'Alcohol consumption per capita (liters)', 
       title = "Drink a <b style='color:#f7cd25'>beer</b> and gain a year", 
       subtitle = "Or take some <b style='color:#d90429'>wine</b>, you will be fine", 
       caption = "Note: the purpose of this graph is to show that <b>correlation does
       not imply causation</b>. Please be aware of this the next time a stranger
       (often on the internet) makes a bold claim about some correlation between
       two variables.<br>
       <span style='font-size:12px;color:grey80'>@ThomIvar • Source: The World Bank</span>") +
  coord_cartesian(clip = 'off') +
  theme_void() +
  theme(
    text = element_text('American Typewriter'),
    plot.title = element_textbox_simple(size = 28, colour = 'grey50'),
    plot.subtitle = element_textbox_simple(size = 18, colour = 'grey70'),
    plot.caption = element_textbox_simple(size = 10, colour = 'grey70', 
                                          margin = margin(t = 3, unit = "mm")),
    plot.background = element_rect( fill = '#fffef7', color = '#fffef7'),
    plot.margin = margin(1,1,1,1, 'cm'),
    panel.grid.major = element_line(colour = 'grey70', linetype = 'dotted'),
    panel.grid.minor = element_line(colour = 'grey90', linetype = 'dotted'),
    axis.line = element_line(colour = 'grey50'), 
    axis.title = element_text(colour = 'grey40'),
    axis.title.x = element_text(margin = margin(t = 2, unit = 'mm')), 
    axis.title.y = element_text(angle = 90, margin = margin(r = 2, unit = 'mm')), 
    axis.text = element_text(colour = 'grey70'), 
    axis.text.x = element_text(margin = margin(t = 1, unit = 'mm')), 
    axis.text.y = element_text(margin = margin(r = 1, unit = 'mm')),
    axis.ticks = element_line(size = 1.25, colour = 'grey50'), 
    axis.ticks.length = unit(1.5, 'mm')
  )
ggsave('30DayChartChallenge/images/correlation.png', width = 10, height = 6)
