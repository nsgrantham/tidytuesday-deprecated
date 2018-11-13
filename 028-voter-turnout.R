library(tidyverse)
library(geofacet)

turnout_messy <- read_csv("data/2018-10-09/voter_turnout.csv")

turnout_messy %>%
  summarize(n_missing_votes = sum(is.na(votes)),  # a lot of missing data
            n_missing_eligible_voters = sum(is.na(eligible_voters)))

turnout_tidy <- turnout_messy %>%
  select(year, state, votes, eligible_voters) %>%
  filter(!is.na(votes), state %in% c(state.name, "District of Columbia")) %>%  # state.name is a base R constant
  mutate(percent_voted = 100 * votes / eligible_voters) %>%
  group_by(year) %>%
  mutate(national_percent_voted = 100 * sum(votes) / sum(eligible_voters)) %>%
  ungroup() %>%
  mutate(state_vs_national = percent_voted - national_percent_voted,
         state_vs_national_category = cut(state_vs_national,
                                          breaks = c(-Inf, -20, -15, -10, -5, -2, 2, 5, 10, 15, 20, Inf),
                                          ordered_result = TRUE))

turnout_tidy %>%
  group_by(state_vs_national_category) %>%
  summarize(n = n())  # number of state election years per category

ggplot(turnout_tidy, aes(year, percent_voted, fill = state_vs_national_category)) + 
  geom_col(width = 1.7, size = 0) +
  facet_geo(~ state) +
  scale_fill_brewer(labels = c("-20", "-15", "-10", " -5", " -2", " 0 ", " +2", " +5", "+10", "+15", "+20"),
                    type = "div", palette = "PuOr", direction = 1, ) +
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010), labels = c("'80", "'90", "'00", "'10")) +
  labs(title = "Minnesota leads the nation in voter turnout in presidential and midterm elections",
       subtitle = "Votes cast per 100 eligible voters in each state in presidential (1980, '84, '88, '92, '96, '00, '04, '08, 2012) and midterm (1982, '86, '90, '94, '98, '02, '06, '10, 2014) elections*",
       caption = "*223 election years in this dataset are missing the number of votes cast, leading to missing bars in many states\nSource: data.world | Graphic: @nsgrantham",
       fill = "Votes cast relative to the national average in a given year",
       x = "Election year", y = "Votes cast per 100 eligible voters") +
  theme_minimal(base_family = "Fira Sans Extra Condensed Light", base_size = 14) +
  theme(legend.direction = "horizontal",
        legend.position = c(0.2, 0.97),
        legend.spacing.x = unit(0.59, "lines"),
        legend.title = element_text(size = 13),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(family = "Fira Sans Extra Condensed", face = "bold", size = 22),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.2)) +
  guides(fill = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) 

ggsave("plots/028-voter-turnout.png", width = 14, height = 9)
