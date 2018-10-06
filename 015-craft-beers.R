library(tidyverse)
library(readxl)

## Read and clean data

data_path <- file.path("data", "2018-07-10", "week15_beers.xlsx") 

style_recodes <- list(
  "KÃ¶lsch" = "Kölsch",
  "MÃ¤rzen / Oktoberfest" = "Märzen / Oktoberfest",
  "BiÃ¨re de Garde" = "Bière de Garde"
)

beers <- read_excel(data_path, sheet = "beers") %>%
  select(id, name, style, brewery_id, everything(), -count) %>%
  rename(beer_id = id) %>%
  mutate(style = recode(style, !!! style_recodes))

brewery_name_recodes <- list(
  "TrÃ¶egs Brewing Company" = "Tröegs Breweing Company"
)

breweries <- read_excel(data_path, sheet = "breweries") %>%
  select(id, everything(), -count) %>%
  rename(brewery_id = id, brewery_name = name) %>%
  mutate(brewery_name = recode(brewery_name, !!! brewery_name_recodes))

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
