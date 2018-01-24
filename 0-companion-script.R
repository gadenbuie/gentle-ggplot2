# ┌─────────────────────────────────────────────────────────────┐
# │                                                             │
# │   A Gentle Guide to the Grammar of Graphics with ggplot2    |
# │                                                             │
# |                   Tampa R Users Group                       |
# |                     January 23, 2018                        |
# │                                                             │
# |                    Garrick Aden-Buie                        |
# |                        @grrrck                              |
# │                                                             │
# │                http://bit.ly/trug-ggplot2                   │
# │                                                             │
# └─────────────────────────────────────────────────────────────┘

# ---- setup ----
# Uncomment below to install any of the packages you need
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("babynames")

library(tidyverse)
library(lubridate)       # in tidyverse, but not loaded by default
library(reshape2)        # for the `tips` data (part 2)
library(babynames)       # for the `babynames` data (part 3)

# ---- tidy-messy-example ----
pop_simple <- population %>% 
  filter(
    country %in% c("United States of America", "Canada", "China"),
    year %in% seq(1995, 2010, 5)
  ) %>% 
  mutate(
    country = ifelse(country == "United States of America",
                     "USA", country)
  )

messy1 <- pop_simple %>% mutate(population = population/10^6) %>% spread(year, population)
messy1

messy2 <- pop_simple %>% mutate(population = population/10^6) %>% spread(country, population)
messy2

# Using the gather function from tidy package
tidy1 <- gather(messy1, 'year', 'population', -country)
tidy1

tidy2 <- gather(messy2, 'country', 'population', -year)
tidy2 

# ---- Our first plot ----
ggplot(tidy1)          #< Base: data

ggplot(tidy1) +
  aes(x = year,        #< Add aesthetics 
      y = population)  #< population by year

ggplot(tidy1) +
  aes(x = year,
      y = population) +
  geom_point()         #< Add points

ggplot(tidy1) +
  aes(x = year,
      y = population,
      color = country) + #< Map country to color
  geom_point()

ggplot(tidy1) +
  aes(x = year,
      y = population,
      color = country) +
  geom_point() +
  geom_line()            #< Add lines (gives warning)

ggplot(tidy1) +
  aes(x = year,
      y = population,
      color = country) +
  geom_point() +
  geom_line(
    aes(group = country)) #< Lines need group (or facet)

# ---- Data for demoing stat/bin ----
# Individual observations as rows
sw_chars <- starwars %>% 
  mutate(
    n_movies = map_int(films, length), 
    gender = ifelse(
      !gender %in% c('female', 'male'), 
      'other', gender)
  ) %>% 
  select(name, gender, n_movies)
sw_chars

# Group by number of movies + gender
sw_chars_id <- sw_chars %>% 
  group_by(n_movies, gender) %>% 
  tally
sw_chars

# ---- Plot Examples of Stat & Bin Parameters ----
ggplot(sw_chars) +
  aes(x = n_movies) +         #< y-axis implied
  geom_bar(stat = "count")    #< Count & aggregate into bars

ggplot(sw_chars) +
  aes(x = n_movies,
      fill = gender) +        #< Color by gender
  geom_bar(stat = "count")

ggplot(sw_chars_id) +         #< Pre-binned
  aes(x = n_movies,
      y = n,                  #< Map count to y-axis
      fill = gender) +
  geom_bar(stat = 'identity') #< and use identity

ggplot(sw_chars_id) +
  aes(x = n_movies,
      y = n,
      fill = gender) +
  geom_col(position = "fill") #< Stack & stretch bars

ggplot(sw_chars_id) +
  aes(x = n_movies,
      y = n,
      fill = gender) +
  geom_col(position = "dodge") #< Bars side-by-side

# ---- Data for demo of facets ----
# Same as above, but add `hair_color`
sw_chars <- starwars %>% 
  mutate(
    n_movies = map_int(films, length),
    gender = ifelse(
      !gender %in% c("female", "male"), 
      "other", gender),
    hair_color = case_when(
      str_detect(hair_color, "blond") ~ "blond",
      str_detect(hair_color, "brown") ~ "brown",
      str_detect(hair_color, "auburn") ~ "auburn",
      str_detect(hair_color, "(grey|white)") ~ "grey/white",
      TRUE ~ "other"
    )
  )

# ---- Facet Wrap Example ----
# Base plot (from above)
g <- ggplot(sw_chars) +
  aes(x = n_movies,
      fill = gender) +
  geom_bar()

g + facet_wrap(~ gender)            #< Individual plots by `gender`

g + facet_grid(gender ~ hair_color) #< facet in grid: row ~ column

g + facet_grid(gender ~ hair_color, 
               scales = 'free_y')   #< Set y-axis by row

# ---- Labels Example ----
g <- g + 
  labs(
    x = "Film Appearances",
    y = "Count of Characters",
    title = "Recurring Star Wars Characters",
    subtitle = "How often do characters appear?",
    fill = "Gender"
  )
g

# ---- Scales Examples ----
g <- g + scale_fill_brewer(palette = 'Set1')
g

# ---- Theme Examples ----
# theme_*() for built-in "complete" themes
g + theme_bw()

# theme() for theme tweaks
g + theme_minimal() + 
  theme(text = element_text(family = "Palatino"))

# Create your own theme by building up from base theme
my_theme <- theme_bw() +
  theme(
    text = element_text(family = "Palatino", size = 12),
    panel.border = element_rect(colour = 'grey80'), 
    panel.grid.minor = element_blank()
  )

# Set plot themes globally with `theme_set()`
# All plots from now on will use `my_theme`
theme_set(my_theme)
g

# But you can still tweak the theme of any plot 
# (relative to current theme)
g + theme(legend.position = 'bottom')

# ---- "Live" Coding Examples ----
# This section explores a dataset and iterates on many different
# Plot types and views of the data. The plots aren't great---they're
# not even good. But this section demonstrates how quickly data can
# be explored with ggplot2, how you audition many ideas by iterating,
# and, ultimately, how "good" plots are made.

# Uses `tips` data from `reshape2`
data(tips, package = "reshape2")
head(tips)

# 1-D view of most important variable: tips
ggplot(tips) +
  aes(x = tip) +
  geom_histogram(   
    binwidth = 0.25 
  )                 

# Replace histogram with density
ggplot(tips) +
  aes(x = tip) +
  geom_density(     
    aes(fill = day) 
  )                 

# See through density overlays
ggplot(tips) +
  aes(x = tip) +
  geom_density(
    aes(fill = day),
    alpha = 0.4 
  )

# See around density overlays with facet
ggplot(tips) +
  aes(x = tip/total_bill) +
  geom_density(
    aes(fill = day)
  ) +
  facet_wrap(~ day) 

# Okay, plot tip vs total bill
ggplot(tips) +
  aes(x = total_bill,
      y = tip) + 
  geom_point()   

# Add a linear regression (is it 20%?)
ggplot(tips) +
  aes(x = total_bill,
      y = tip) +
  geom_point() +
  geom_smooth(method = "lm") 

# Add some lines for 20% and 15% tip
ggplot(tips) +
  aes(x = total_bill,
      y = tip) +
  geom_point() +
  geom_smooth(method = "lm")+ 
  geom_abline(            
    slope = c(0.2, 0.15), 
    intercept = 0,        
    color = c('#69b578', 
              "#dd1144"),
    linetype = 3)

# What about tip as percent of total vs total
ggplot(tips) +
  aes(x = total_bill,
      y = tip/total_bill) + 
  geom_point() +
  geom_hline( 
    yintercept = c(0.2, 0.15),
    color = c('#69b578', 
              "#dd1144"),
    linetype = 1)

# Add a variable to `tips` to make this easier
tips$percent <-  
  tips$tip/tips$total_bill 

# Now: Tip percent vs size of party
# Plus smoker as point color
ggplot(tips) +
  aes(x = size,
      y = percent, 
      color = smoker) + 
  geom_point()

# Hard to see points, switching to `geom_jitter`
ggplot(tips) +
  aes(x = size,
      y = percent,
      color = smoker) +
  geom_jitter(width = 0.25) 

# Trying to see everything:
# Tips percentage vs day of week
# Faceted by time of day (rows) and smoker status (cols)
# Colored by sex because why not
ggplot(tips) +
  aes(x = day,
      y = percent,
      color = sex) +
  geom_jitter(width = 0.25) +
  facet_grid(time ~ smoker) 

# Note that day of week and time of day
# are a bit out of order, need to refactor
tips <- mutate(tips,
  time = factor(time, 
    c("Lunch", "Dinner")), 
  day = factor(day, 
    c("Thur", "Fri", 
      "Sat", "Sun") 
  ))

# This looks better
ggplot(tips) +
  aes(x = day,
      y = percent,
      color = sex) +
  geom_jitter(width = 0.25) +
  facet_grid(time ~ smoker)

# Replace jitter with a boxplot
ggplot(tips) +
  aes(x = day,
      y = percent,
      fill = time) +
  geom_boxplot() + 
  facet_grid(. ~ smoker)

# Replace boxplot with a violin plot
ggplot(tips) +
  aes(x = day,
      y = percent,
      color = smoker, 
      fill  = smoker) + 
  geom_violin(alpha = 0.3) + 
  facet_wrap(~ smoker)

# Add back jitter
g <- ggplot(tips) +
  aes(x = day,
      y = percent,
      color = smoker,
      fill = smoker) +
  geom_violin(alpha = 0.3) + 
  geom_jitter(alpha = 0.2,   
              width = 0.1,  
              size  = 0.8)+  
  facet_wrap(~ smoker)
g

# Spiff up the plot a bit
g + guides(color = FALSE,
           fill  = FALSE) +
  labs(x = '', 
       y = 'Tip Rate') +
  scale_y_continuous(        
    labels = scales::percent 
  )                          

# ---- More In-Depth Examples ----
# The next two examples demonstrate more complicated plotting, as both
# require some initial data processing to isolate the region of interest
# in the data. They also require some thought about how to collect that
# data in the appropriate format for the plot. And finally, I've added
# some extra bits (possibly chart junk) just for fun.

# Uses the `babynames` table from the `babynames` package
data(babynames, 'babynames')

# Peek at the data
babynames %>% 
  sample_n(25)

# ---- Babynames Ex 1: Babynames from 2015 ----
# Top 10 babynames, by sex, from 2015
babynames_pop2015 <- babynames %>% 
  filter(year == 2015) %>% 
  mutate(
    n = n/1000,
    sex = case_when(
      sex == "F" ~ "Girl Names", #< Make life easier later
      TRUE ~ "Boy Names"         #< By giving factors good labels
  )) %>% 
  group_by(sex) %>% 
  top_n(10, n)

babynames_pop2015

# ---- Babynames Ex 1: Plot ----
# Start with base bar plot
ggplot(babynames_pop2015) +
  aes(y = n, x = name) +
  geom_col()

ggplot(babynames_pop2015) +
  aes(y = n, x = name) +
  geom_col() +
  coord_flip()              #< Flip coordinates for horiz. bars

ggplot(babynames_pop2015) +
  aes(y = n, 
      x = fct_reorder(name, n)) + #< Reorder factor by name count
  geom_col() +                    #  Thanks: forecats::fct_reorder
  coord_flip()

ggplot(babynames_pop2015) +
  aes(y = n, 
      x = fct_reorder(name, n), 
      fill = sex) +         #< Color bars by sex
  geom_col() +
  coord_flip()

ggplot(babynames_pop2015) +
  aes(y = n, 
      x = fct_reorder(name, n), 
      fill = sex) +
  geom_col() +
  coord_flip() +
  facet_wrap( ~ sex, scales = 'free_y') #< Break into separate plots by sex


g_babynames <- ggplot(babynames_pop2015) +
  aes(y = n, 
      x = fct_reorder(name, n),
      fill = sex) +
  geom_col() +
  # Add text annotations in each bar
  geom_text(
    # geom_text needs `label` aesthetic
    aes(label = format(n*1000, big.mark = ',')),
    # set appropriate size, and move horiz. so inside bars
    size = 9, hjust = 1.1, 
    # set color and font
    color = 'white', family = 'Fira Sans'
  ) +
  coord_flip() +
  facet_wrap( ~ sex, scales = 'free_y')
g_babynames 

g_babynames +
  labs(x = '', 
       y = 'Number of Babies Born in 2015 (thousands)') + 
  # Hide legend
  guides(fill = FALSE) +
  # Manually set colors for fill
  scale_fill_manual(
    values = c("Boy Names"  = "#77cbb9",    #< Note factor level as name
               "Girl Names" = "#a077cb")) +
  # Tweak some theme elements
  theme(
    strip.text = element_text(face = 'bold', size = 20),
    strip.background = element_blank(),
    text = element_text(size = 24)
  )

# ---- Babynames Ex 2: Gender-Bending Baby Names ----
# This plot show names that historically were given to babies of one sex and are
# now trending towards the opposite sex. The setup is a little more involved,
# but is replicated below. Run everything up to line 542 to skip haed. I pulled
# out the ten names for both starting sexes that had the greatest change from
# start to finish... ...AND were at one point actually used for the original
# gender.

library(tidyverse)
library(babynames)

if_na <- function(x, val) ifelse(is.na(x), val, x)

top_change_babynames <- babynames %>% 
  mutate(
    sex = c('M' = 'Male', "F" = "Female")[sex]
  ) %>% 
  select(-n) %>% 
  spread(sex, prop) %>% 
  mutate(
    Female = if_na(Female, 0),
    Male = if_na(Male, 0),
    year_group = case_when(
      year < 1900 ~ '1800s',
      year >= 2000 ~ "2000s",
      TRUE ~ "1900s"
    ),
    sex_diff = Female - Male
  ) %>% 
  select(-year, -Female:-Male) %>% 
  group_by(name, year_group) %>% 
  summarize(sex_diff = mean(sex_diff)) %>% 
  spread(year_group, sex_diff, fill = 0) %>% 
  ungroup %>% 
  mutate(
    change = abs(`1800s` - `2000s`),
    sign_change = sign(`1800s`) != sign(`2000s`)
  ) %>% 
  filter(sign_change, 
         abs(`1800s`) > 0,
         abs(`2000s`) > 0) %>% 
  arrange(-change)


names_m2f <- top_change_babynames %>% 
  filter(`1800s` < 0) %>% 
  slice(1:10) %>% 
  pull(name)

names_f2m <- top_change_babynames %>% 
  filter(`1800s` > 0) %>% 
  slice(1:10) %>% 
  pull(name)


sel_change_babynames <- babynames %>%
  filter(name %in% c(names_m2f, names_f2m)) %>% 
  mutate(
    sex = c('M' = 'Male', "F" = "Female")[sex]
  ) %>% 
  select(-n) %>% 
  spread(sex, prop) %>% 
  group_by(name) %>% 
  mutate(
    Female = if_na(Female, 0),
    Male = if_na(Male, 0),
    prop = Female - Male
  ) %>%
  ungroup %>%
  split(.$name) %>% 
  map_df(function(x) {
    # thank you: https://stackoverflow.com/a/7883556
    prop_groups <- rle(x$prop > 0)
    x$prop_group <- rep.int(
      1:length(prop_groups$lengths),
      times = prop_groups$lengths
    )
    x
  }) %>% 
  mutate(
    name = factor(name, levels = c(names_m2f, rev(names_f2m)))
  )

# `sel_change_babynames` is the end result of the data setup
sel_change_babynames

# ---- Plotting Babynames Ex 2 ----
# Base: proportion by year
ggplot(sel_change_babynames) + 
  aes(x = year, y = prop)

ggplot(sel_change_babynames) + 
  aes(x = year, y = prop) +
  geom_line(color = "grey50",   #< Add grey lines
            aes(group=name))    #< for each name

ggplot(sel_change_babynames) + 
  aes(x = year, y = prop, 
      fill = prop > 0) +                #< Add fill for area (could be inside `geom_area`)
  geom_area(aes(group = prop_group)) +  #< Add geom_area to shade between line and x-axis
  geom_line(color = "grey50", 
            aes(group=name))+
  facet_wrap(~ name,             #< Each name gets its own plot
             scales = 'free_y',  #< with individual y-axis scale
             ncol = 5)           #< 5 per row (first two rows are male-to-female, 
                                 #  second two are female-to-male)

g_bnc <- 
  ggplot(sel_change_babynames) + 
  aes(x = year, y = prop,
      fill = prop > 0) +
  geom_area(aes(group = prop_group)) +
  geom_line(color = "grey50", 
            aes(group=name))+
  facet_wrap(~ name, 
             scales = 'free_y', 
             ncol = 5) + 
  scale_fill_manual(                    #< Change fill colors
    values = c("#6ec4db", "#fa7c92")) + #< Only two values (FALSE, TRUE)
  guides(fill = FALSE) +                #< Hide legends
  labs(x = '', y = '')                  #< Hide axis labels too

g_bnc


# Start from `theme_minimal` with Palatino
# Tweak theme some more
g_bnc <- g_bnc +
  theme_minimal(base_family = 'Palatino') +
  theme(
    axis.text.y = element_blank(),
    strip.text = element_text(size = 18, face = 'bold'),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linetype = 3))

g_bnc

# ---- May the force be with you ----
# ggplot2 Extensions
#
# ** Extending ggplot2
#
# - Check out theggplot2 extensions gallery: 
#   http://www.ggplot2-exts.org/gallery/
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
#   http://github.com/gadenbuie/trug-ggplot2
