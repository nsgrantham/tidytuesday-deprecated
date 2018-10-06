# Tidy Tuesday -- Week 3: Global mortality
# Neal Grantham (@nsgrantham) -- 2018-05-07
library(tidyverse)
library(readxl)
theme_set(theme_minimal())

percent <- " [(]%[)]"  # the '[]'s tell regex to leave '(' and ')' alone
sdi_levels <- c("High", "High-middle", "Middle", "Low-middle", "Low")
mortality <- read_xlsx("data/2018-04-16/global_mortality.xlsx") %>%
  filter(str_detect(country, "SDI")) %>%
  rename(sociodemographic_index = country) %>%
  mutate(sociodemographic_index = sub(" SDI", "", sociodemographic_index)) %>%
  mutate(sociodemographic_index = factor(sociodemographic_index, levels = sdi_levels)) %>%
  select_if(~ all(!is.na(.))) %>%
  rename_all(~ sub(percent, "", .)) %>%
  gather(cause_of_death, deaths_per_1k, -sociodemographic_index, -year) %>%
  mutate(deaths_per_1k = 10 * deaths_per_1k,
         cause_of_death = replace(cause_of_death, cause_of_death == "Parkinson disease", 
                                  "Parkinson's disease"))  # correct spelling :)

parse_factor_to_numeric <- function(x) as.numeric(levels(x))[x]
breaks <- c(0, 5, 15, 30, 50, 150, 300, 500)

yvals <- mortality %>%
  group_by(cause_of_death) %>%
  summarize(ymax = max(deaths_per_1k),
            yupper = cut(max(deaths_per_1k), breaks, labels = breaks[-1])) %>%
  mutate(yupper = parse_factor_to_numeric(yupper)) %>%
  arrange(desc(ymax))
  
mortality <- mortality %>%
  left_join(yvals, by = "cause_of_death") %>%
  mutate(cause_of_death = factor(cause_of_death, levels = yvals$cause_of_death))

# yellow to indigo
palette <- c("#ffd700", "#fa8775", "#ea5f94", "#cd34b5", "#0000ff")

ggplot(mortality, aes(year, deaths_per_1k, color = sociodemographic_index)) +
  geom_hline(yintercept = 0,  size = 0.3, alpha = 0.5) +
  geom_hline(aes(yintercept = yupper), size = 0.3, alpha = 0.) +
  geom_line() +
  facet_wrap(~ cause_of_death, scales = "free_y") +
  scale_color_manual(values = palette) +
  labs(title = "Global mortality rates (1990 to 2016) by Socio-demographic Index (SDI)",
       x = "Year", y = "Deaths per 1,000 people",
       caption = "Source: ourworldindata.org | Graphic: @nsgrantham") + 
  theme(legend.position = c(0.73, 1.063), legend.direction = "horizontal", 
        legend.title = element_blank(), strip.text.x = element_text(size = 7), 
        axis.text = element_text(size = 7), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank())

ggsave("plots/003-global-mortality.png", width = 12, height = 8)
