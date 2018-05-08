# Tidy Tuesday -- Week 4: Australian salaries
# Neal Grantham (@nsgrantham) -- 2018-04-30
library(tidyverse)
library(ggrepel)
theme_set(theme_minimal())

salary <- read_csv("data/week4_australian_salary.csv") %>%
  select(-one_of("X1", "gender_rank")) %>%
  separate(occupation, into = "occupation", sep = ";", extra = "drop") %>%
  mutate(occupation = str_replace_all(occupation, "\x96", "-")) %>%
  filter(!(occupation %in% c("Occupation not listed")))

income <- salary %>%
  select(-individuals) %>%
  spread(gender, average_taxable_income) %>%
  rename(m_income = Male, f_income = Female)

ratio_breaks <- c(0, 1/4, 1/2, 3/4, 4/3, 2, 4, Inf)
ratio_labels <- c("Women outnumber men at least 4:1", "Women outnumber men at least 2:1", 
                  "Women outnumber men at least 4:3", "Approximately equal gender balance", 
                  "Men outnumber women at least 4:3", "Men outnumber women at least 2:1",
                  "Men outnumber women at least 4:1")
workforce <- salary %>%
  select(-average_taxable_income) %>%
  spread(gender, individuals, fill = 0) %>%
  rename(m_workforce = Male, f_workforce = Female) %>% 
  transform(mf_workforce_ratio = m_workforce / f_workforce) %>%
  transform(mf_workforce_category = cut(mf_workforce_ratio, breaks = ratio_breaks, 
                                        labels = ratio_labels, include.lowest = TRUE))

df <- workforce %>% 
  left_join(income, by = "occupation") %>%
  filter(m_workforce + f_workforce > 100) %>%
  mutate(occupation = replace(occupation, f_income < 110000 & m_income < 175000, NA))

df %>%
    select(mf_workforce_category) %>%
    summary()  # how many counts in each workforce category?

# darkturquoise to white to darkorange
palette <- c("#00ced1", "#85dfe0", "#c6efef", "#ffffff", "#ffe3bd", "#ffc574", "#ffa500")

ggplot(df, aes(m_income, f_income)) + 
  geom_abline(intercept = 0, slope = 1, alpha = 0.5, size = 0.2) +
  geom_point(aes(fill = mf_workforce_category), color = "black", pch = 21, size = 2, stroke = 0.1, na.rm = TRUE) +
  geom_text_repel(aes(label = occupation), size = 2, segment.size = 0.1, point.padding = 0.5, na.rm = TRUE) +
  labs(title = "Australian taxable incomes 2013-14 by gender and occupation",
       x = "Average taxable income among men (AUD)", 
       y = "Average taxable income among women (AUD)",
       caption = "Source: data.gov.au | Graphic: @nsgrantham") +
  scale_fill_manual(name = "", values = palette) +
  scale_x_continuous(labels = scales::dollar, breaks = seq(0, 1000000, by = 100000)) + 
  scale_y_continuous(labels = scales::dollar, breaks = seq(0, 1000000, by = 100000)) + 
  coord_cartesian(xlim = c(0, 610000)) +
  theme(legend.position = c(0.16, 0.843))

ggsave("004-australian-salary/004-australian-salary.png", width = 9, height = 7)