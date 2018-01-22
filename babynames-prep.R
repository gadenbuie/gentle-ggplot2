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


babynames_plot <- ggplot(sel_change_babynames) + 
  aes(x = year, y = prop, fill = prop > 0) +
  geom_area(aes(group = prop_group)) +
  geom_line(color = "grey50", aes(group=name))+
  facet_wrap(~ name, scales = 'free_y', ncol = 5) + 
  scale_fill_manual(values = c("#6ec4db", "#fa7c92")) +
  guides(fill = FALSE) +
  theme(
    axis.text.y = element_blank(),
    strip.text = element_text(size = 18, face = 'bold'),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linetype = 3)
  ) +
  labs(x = '', y = '')
