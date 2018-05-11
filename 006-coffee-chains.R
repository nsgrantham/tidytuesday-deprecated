# Tidy Tuesday | Week 6: Coffee chain locations
#  2018-05-08  | Neal Grantham (@nsgrantham)
library(tidyverse)
library(readxl)
library(geofacet)
theme_set(theme_minimal())

chains <- "data/week6_coffee_chains.xlsx"

starbucks <- read_xlsx(chains, sheet = "starbucks") %>%
  rename(brand = Brand, state = `State/Province`, country = Country) %>%
  filter(country %in% c("CA", "US")) %>%
  select(brand, state, country)

dunkin <- read_xlsx(chains, sheet = "dunkin") %>%
  rename(brand = biz_name, state = e_state, country = e_country) %>%
  mutate(country = replace(country, country == "USA", "US")) %>%
  mutate(brand = replace(brand, str_detect(brand, "Dunkin"), "Dunkin' Donuts")) %>%
  select(brand, state, country)

coffee <- rbind(starbucks, dunkin) %>% 
  filter(country == "US", brand %in% c("Starbucks", "Dunkin' Donuts")) %>%
  rename(state_abbr = state)

state_w_abbr <- as.tibble(cbind(State = state.name, state_abbr = state.abb))
states <- read_csv("data/acs2015_county_data.csv") %>%  # week 5 dataset
  group_by(State) %>%
  summarize(population = sum(TotalPop)) %>%
  left_join(state_w_abbr, by = "State") %>%
  rename(state_name = State) %>%
  mutate(state_abbr = replace(state_abbr, state_name == "District of Columbia", "DC"))

coffee_by_state <- coffee %>%
  left_join(states, by = "state_abbr") %>%
  group_by(brand, country, state_name) %>%
  summarize(n = n(), population = first(as.numeric(population))) %>%
  transform(locations_per_100k_people = 100000 * (n / population)) %>%
  mutate(locations_per_100k_people = ifelse(brand == "Starbucks", 
                                            -locations_per_100k_people, 
                                            locations_per_100k_people))

starbucks_vs_dunkin <- coffee_by_state %>%
  group_by(state_name) %>%
  summarize(difference = sum(locations_per_100k_people))

coffee_by_state <- coffee_by_state %>% 
  left_join(starbucks_vs_dunkin, by = "state_name") %>%
  arrange(difference) %>%
  mutate(state_name = factor(state_name, levels = unique(state_name)))

coffee_by_state_diff <- coffee_by_state %>% 
  group_by(state_name) %>%
  filter(abs(locations_per_100k_people) == max(abs(locations_per_100k_people)))

# dunkin' donuts pink, starbucks green
palette <- c("#E11383", "#00653B")

ggplot(coffee_by_state, aes(state_name, locations_per_100k_people, fill = brand)) + 
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_bar(data = coffee_by_state_diff, aes(state_name, difference, fill = brand), 
           stat = "identity", alpha = 0.7, inherit.aes = FALSE) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(breaks = c(-10, 0, 10, 20), labels = c(10, 0, 10, 20)) +
  labs(title = "Starbucks vs. Dunkin' Donuts",
       x = "U.S. state or capital", y = "Store locations per 100,000 people",
       caption = "Source: kaggle.com, odditysoftware.com | Graphic: @nsgrantham") +
  guides(fill = FALSE) +
  coord_flip()

ggsave("006-coffee-chains.png", width = 6, height = 8)  