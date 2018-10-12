library(tidyverse)

download.file("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-10-09/voter_turnout.csv",
              "voter_turnout.csv")
turnout_messy <- read_csv("voter_turnout.csv")

turnout <- turnout_messy %>%
  filter(!is.na(votes)) %>%
  select(year, state, votes, eligible_voters) %>%
  mutate(percent_voted = 100 * votes / eligible_voters,
         election_type = case_when(
           year %in% seq(1982, 2014, by = 4) ~ "midterm",
           year %in% seq(1980, 2012, by = 4) ~ "presidential"
         )) %>%
  group_by(year) %>%
  mutate(national_percent_voted = 100 * sum(votes) / sum(eligible_voters)) %>%
  ungroup() %>%
  mutate(rel = percent_voted - national_percent_voted,
         rel2 = case_when(
           rel > 0 ~ "high",
           rel <= 0 ~ "low"
         ))

range(turnout$rel)

ggplot(turnout, aes(year, percent_voted, fill = rel)) + 
  geom_col(width = 2, size = 0.1) +
  facet_geo(~ state) +
  scale_fill_distiller(type = "div", palette = "PuOr", direction = 1) +
  expand_limits(y = c(0, 80), fill = c(-23, 23)) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010), 
                     labels = c("'80", "'90", "'00", "'10")) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())
  


