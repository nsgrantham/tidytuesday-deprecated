library(tidyverse)
library(readxl)
theme_set(theme_minimal())

## Read and clean data

data_path <- file.path("data", "2018-07-10", "week15_beers.xlsx") 

convert_encoding <- function(x) iconv(x, from = "utf-8", to = "iso-8859-1")

beers <- read_excel(data_path, sheet = "beers") %>%
  select(id, name, style, brewery_id, everything(), -count) %>%
  rename(beer_id = id) %>%
  mutate_at(vars(name, style), convert_encoding)

breweries <- read_excel(data_path, sheet = "breweries") %>%
  select(id, everything(), -count) %>%
  rename(brewery_id = id, brewery_name = name) %>%
  mutate(brewery_name = convert_encoding(brewery_name))

## Some data summaries

breweries_per_state <- breweries %>%
  group_by(state) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

beers_per_brewery <- beers %>%
  left_join(breweries, by = "brewery_id") %>%
  group_by(brewery_id, brewery_name, state) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

beers_per_style <- beers %>%
  group_by(style) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

## Plot ABV by beer style

top_beer_styles <- beers_per_style %>%
  filter(n >= 10) %>%
  pull(style)

top_beers <- beers %>%
  filter(style %in% top_beer_styles) %>%
  group_by(style) %>%
  mutate(mean_abv = mean(abv, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(mean_abv) %>%
  mutate(style = fct_inorder(style))

ggplot(top_beers, aes(style, abv)) + 
  geom_jitter(width = 0.2, alpha = 0.5) + 
  coord_flip()
