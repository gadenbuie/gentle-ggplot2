# ┌─────────────────────────────────────────────────────────────┐
# │                                                             │
# │   A Gentle Guide to the Grammar of Graphics with ggplot2    |
# │                                                             │
# │                                                             │
# |                    Garrick Aden-Buie                        |
# |                        @grrrck                              |
# │                                                             │
# │               http://bit.ly/gentle-ggplot2                  │
# │                                                             │
# └─────────────────────────────────────────────────────────────┘

# ---- setup ----
# Uncomment below to install any of the packages you need
# install.packages("tidyverse")
# install.packages("gapminder")

library(tidyverse)
library(gapminder)

## ---- guess-data-from-plot ----
df_mpg <- mpg %>% 
  filter(class %in% c("compact", "suv")) %>% 
  filter(manufacturer %in% c("toyota", "ford", "audi")) %>% 
  filter(year == 2008) %>% 
  group_by(manufacturer, model) %>% 
  sample_n(1)

g <- ggplot(df_mpg) +
  aes(x = cty, y = hwy, color = class, shape = manufacturer) +
  geom_point(size = 4) +
  labs(x = NULL, y = NULL, shape = NULL, color = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(fill = rgb(250, 250, 250, max = 255),
                                   color = "#777777")
  )

# Minimal plot
g + 
  guides(color = FALSE, shape = FALSE) +
  theme(axis.text = element_blank())

# Final plot
g + 
  ggtitle("MPG Ratings") +
  labs(x = "City", y = "Highway", shape = "Manufacturer", color = "Class") +
  theme(
    panel.border = element_rect(fill = NA, color = "grey85"),
    panel.grid.major = element_line(color = "grey90")
  )

## ---- tidy-messy1 ----
pop_simple <- gapminder %>% 
  filter(
    country %in% c("Canada", "China", "United States"), 
    year >= 1997
  ) %>% 
  select(country, year, pop) %>% 
  mutate(pop = pop / 10^6)

messy_pop <- pop_simple %>% spread(year, pop)

knitr::kable(messy_pop, format = 'html')

## ---- tidy-tidy ----
tidy_pop <- gather(messy_pop, 'year', 'pop', -country)

## ---- geom-demo ----
minimal_theme <- theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

set.seed(4242)
df_geom <- data_frame(y = rnorm(10), x = 1:10)

g_geom <- list()
g_geom$point <- ggplot(df_geom, aes(x, y)) + geom_point() + ggtitle("geom_point")
g_geom$line <- ggplot(df_geom, aes(x, y)) + geom_line() + ggtitle("geom_line")
g_geom$bar <- ggplot(df_geom, aes(x, y)) + geom_col() + ggtitle("geom_bar")
g_geom$boxplot <- ggplot(df_geom, aes(y = y)) + geom_boxplot() + ggtitle("geom_boxplot") + labs(x = "x")

g_geom <- map(g_geom, ~ . + minimal_theme)

g_geom

# ---- grammar-of-graphics ----
tidy_pop <- left_join(tidy_pop, select(gapminder, country, continent))

ggplot(tidy_pop) +
  aes(x = year,
      y = pop,
      color = country) +
  geom_point() +
  geom_line(aes(group = country))

g <- ggplot(tidy_pop) +
  aes(x = year,
      y = pop,
      color = country) +
  geom_point() +
  geom_line(aes(group = country))

g + facet_wrap(~ country)

g + facet_grid(continent ~ country)

g + labs(x = "Year", y = "Population")

g + coord_flip()

g + coord_polar()

g + scale_color_manual(values = c("peru", "pink", "plum"))

g + scale_y_log10()

g + scale_x_discrete(labels = c("MCMXCVII", "MMII", "MMVII"))

g + theme_bw()

g + theme_minimal() + theme(text = element_text(family = "Palatino"))

my_theme <- theme_bw() +
  theme(
    text = element_text(family = "Palatino", size = 12),
    panel.border = element_rect(colour = 'grey80'), 
    panel.grid.minor = element_blank()
  )

#theme_set(my_theme)

g + my_theme

g + my_theme + theme(legend.position = 'bottom')

## ----reset_theme, include=FALSE------------------------------------------
theme_set(theme_gray())

## ----head-gapminder, echo=FALSE------------------------------------------
head(gapminder)

## ----summary-gapminder, echo=FALSE, comment=""---------------------------
summary(gapminder)

## ----gapminder-le-gdp-1, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp)

## ----gapminder-le-gdp-2, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp) +
  geom_point() #<<

## ----gapminder-le-gdp-3, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) + #<<
  geom_point()

## ----gapminder-le-gdp-4, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_point() +
  scale_x_log10() #<<

## ----gapminder-le-gdp-5, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~ continent) + #<<
  guides(color = FALSE)     #<<

## ----gapminder-le-gdp-6, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_point(size = 0.25) + #<<
  scale_x_log10() +
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-gdp-7, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_line() + #<<
  geom_point(size = 0.25) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-gdp-8, fig.show="hide"---------------------------------
ggplot(gapminder) +
  aes(x = gdpPercap,
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country) #<<
  ) +
  geom_point(size = 0.25) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-gdp-year-1, fig.show="hide"-------------------------------
ggplot(gapminder) +
  aes(x = year, #<<
      y = gdpPercap, #<<
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  scale_y_log10() + #<<
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-gdp-year-2, fig.show="hide"-------------------------------
ggplot(gapminder) +
  aes(x = year,
      y = gdpPercap,
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  scale_y_log10() +
  scale_x_continuous(            #<<
    breaks = #<<
      seq(1950, 2000, 25) #<<
  ) +                            #<<
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-1, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp, #<<
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  #scale_y_log10() + #<<
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-2, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country)
  ) +
  geom_point(size = 0.25) +
  geom_smooth() + #<<
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-3, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75" #<<
  ) +
  geom_point(size = 0.25) +
  geom_smooth() + 
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-4, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  #geom_point(size = 0.25) + #<<
  geom_smooth() + 
  scale_x_continuous(
    breaks = 
      seq(1950, 2000, 25)
  ) +  
  facet_wrap(~ continent) +
  guides(color = FALSE)

## ----gapminder-le-year-5, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  # scale_x_continuous(
  #   breaks = 
  #     seq(1950, 2000, 25)
  # ) +  
  # facet_wrap(~ continent) + #<<
  guides(color = FALSE)

## ----gapminder-le-year-6, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  theme( #<<
  legend.position = "bottom" #<<
  ) #<<

## ----gapminder-le-year-7, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  theme_minimal() + #<<
  theme(
  legend.position = "bottom"
  )

## ----gapminder-le-year-8, fig.show="hide"--------------------------------
ggplot(gapminder) +
  aes(x = year, 
      y = lifeExp,
      color = continent) +
  geom_line(
    aes(group = country),
    color = "grey75"
  ) +
  geom_smooth() + 
  theme_minimal( 
    base_size = 8) + #<<
  theme(
  legend.position = "bottom"
  )

## ----gapminder-americas-data, fig.show="hide"----------------------------
americas <- 
  gapminder %>% 
  filter(
    country %in% c(
      "United States",
      "Canada",
      "Mexico",
      "Ecuador"
    )
  )

## ----echo=FALSE----------------------------------------------------------
americas

## ----gapminder-americas-1, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = pop
  ) +
  geom_col()

## ----gapminder-americas-2, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = pop,
    fill = country #<<
  ) +
  geom_col()

## ----gapminder-americas-3, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = pop,
    fill = country
  ) +
  geom_col(
    position = "dodge" #<<
  )

## ----gapminder-americas-4, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = pop / 10^6, #<<
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  )

## ----gapminder-americas-5, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = pop / 10^6,
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  ) +
  facet_wrap(~ country) + #<<
  guides(fill = FALSE) #<<

## ----gapminder-americas-6, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = pop / 10^6,
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  ) +
  facet_wrap(~ country,
    scales = "free_y") + #<<
  guides(fill = FALSE)

## ----gapminder-americas-7, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp, #<<
    fill = country
  ) +
  geom_col(
    position = "dodge" 
  ) +
  facet_wrap(~ country,
    scales = "free_y") +
  guides(fill = FALSE)

## ----gapminder-americas-8, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp,
    fill = country
  ) +
  geom_line() + #<<
  facet_wrap(~ country,
    scales = "free_y") +
  guides(fill = FALSE)

## ----gapminder-americas-9, fig.show="hide"-------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp,
    color = country #<<
  ) +
  geom_line() +
  facet_wrap(~ country,
    scales = "free_y") +
  guides(color = FALSE) #<<

## ----gapminder-americas-10, fig.show="hide"------------------------------
ggplot(americas) +
  aes(
    x = year,
    y = lifeExp,
    color = country
  ) +
  geom_line()

## ----gapminder-all-americas-1, fig.show="hide"---------------------------
gapminder %>% 
  filter(
    continent == "Americas"
  ) %>% #<<
  ggplot() + #<<
  aes(
    x = year,
    y = lifeExp
  )

## ----gapminder-all-americas-2, fig.show="hide"---------------------------
gapminder %>% 
  filter(
    continent == "Americas"
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = lifeExp
  ) +
  geom_boxplot() #<<

## ----gapminder-all-americas-3, fig.show="hide"---------------------------
gapminder %>% 
  filter(
    continent == "Americas"
  ) %>%
  mutate( #<<
    year = factor(year) #<<
  ) %>%  #<<
  ggplot() +
  aes(
    x = year,
    y = lifeExp
  ) +
  geom_boxplot()

## ----gapminder-all-americas-4, fig.show="hide"---------------------------
gapminder %>% 
  # filter(
  #   continent == "Americas"
  # ) %>%
  mutate(
    year = factor(year)
  ) %>% 
  ggplot() +
  aes(
    x = year,
    y = lifeExp
  ) +
  geom_boxplot()

## ----gapminder-all-americas-5, fig.show="hide"---------------------------
gapminder %>% 
  mutate(
    year = factor(year)
  ) %>% 
  ggplot() +
  aes(
    x = year,
    y = lifeExp,
    fill = continent #<<
  ) +
  geom_boxplot()

## ----gapminder-all-americas-6, fig.show="hide"---------------------------
gapminder %>% 
  mutate(
    year = factor(year)
  ) %>% 
  ggplot() +
  aes(
    x = year,
    y = lifeExp,
    fill = continent
  ) +
  geom_boxplot() +
  coord_flip() #<<

## ----gapminder-all-americas-7, fig.show="hide"---------------------------
gapminder %>% 
  mutate(
    decade = floor(year / 10), #<<
    decade = decade * 10,      #<<
    decade = factor(decade)      #<<
  ) %>% 
  ggplot() +
  aes(
    x = decade, #<<
    y = lifeExp,
    fill = continent
  ) +
  geom_boxplot() +
  coord_flip()

## ----gapminder-all-americas-8, fig.show="hide", echo=1-------------------
g <- gapminder %>% 
  filter( #<<
    continent != "Oceania" #<<
  ) %>% #<<
  mutate(
    decade = floor(year / 10) * 10, decade = factor(decade)      
  ) %>% 
  ggplot() +
  aes(
    x = decade,
    y = lifeExp,
    fill = continent
  ) +
  geom_boxplot() +
  coord_flip()

g

## ----gapminder-all-americas-9, fig.show="hide", echo=1-------------------
g +
  theme_minimal(8) +
  labs(
    x = "Life Expectancy",
    y = "Decade",
    fill = NULL,
    title = "Life Expectancy by Continent and Decade",
    caption = "gapminder.org"
  )

## ----hans-rosling-1, echo=1, out.height="99%", fig.width=16, fig.height=8, fig.show="hide"----
g_hr <- 
  ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  facet_wrap(~year)
g_hr

## ----hans-rosling-1a, echo=1, out.height="99%", fig.width=16, fig.height=8, fig.show="hide"----
g_hr <- 
  ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  facet_wrap(~year) +
  guides(color = FALSE, size = FALSE)
g_hr

## ----hans-rosling-2, echo=1, out.height="99%", fig.width=16, fig.height=8, fig.show="hide"----
g_hr <- 
  g_hr +
  scale_x_log10(breaks = c(10^3, 10^4, 10^5), labels = c("1k", "10k", "100k")) +
  scale_color_manual(values = gapminder::country_colors) +
  scale_size(range = c(0.5, 12))
  
g_hr

## ----hans-rosling-3, echo=1, out.height="99%", fig.width=16, fig.height=8, fig.show="hide"----
g_hr <- 
  g_hr +
  labs(
    x = "GDP per capita",
    y = "Life Expectancy"
  ) +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(fill = NA, color = "grey40"),
    panel.grid.minor = element_blank()
  )
g_hr

## ----hans-rosling-final, fig.width=16, fig.height=9, fig.show="hide"-----
ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, size = pop, color = country) +
  geom_point() +
  facet_wrap(~year) +
  guides(color = FALSE, size = FALSE) +
  scale_x_log10(
    breaks = c(10^3, 10^4, 10^5), 
    labels = c("1k", "10k", "100k")) +
  scale_color_manual(values = gapminder::country_colors) +
  scale_size(range = c(0.5, 12)) +
  labs(
    x = "GDP per capita",
    y = "Life Expectancy") +
  theme_minimal(14, base_family = "Fira Sans") +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(fill = NA, color = "grey40"),
    panel.grid.minor = element_blank())


# ---- May the force be with you ----
# ggplot2 Extensions
#
# ** Extending ggplot2
#
# - Check out theggplot2 extensions gallery: 
#   http://www.ggplot2.tidyverse.org/gallery/
#
# ** Learn more
# 
# - ggplot2 docs: 
#   http://ggplot2.tidyverse.org/
# 
# - R4DS - Data visualization: 
#   http://r4ds.had.co.nz/data-visualisation.html
# 
# - Hadley Wickham's ggplot2 book: 
#   https://www.amazon.com/dp/0387981403/
#
# Practice and Review
#
# ** Fun Datasets
#
# library(fivethirtyeight)
# library(nycflights)
# library(ggplot2movies)
# library(tidyr) # for `population` and `who` datasets

# ---- Author ----
# Garrick Aden-Buie
# Website: garrickadenbuie.com
# Twitter: @grrrck
# Slides and code on GitHub: 
#   http://github.com/gadenbuie/gentle-ggplot2
