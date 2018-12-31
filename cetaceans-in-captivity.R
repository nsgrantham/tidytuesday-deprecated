library(tidyverse)
library(ggdark)

cetacean <- read_csv("data/2018-12-18/allCetaceanData.csv")

cetacean %>%
  group_by(species) %>%
  tally() %>%
  arrange(desc(n))

# Let's focus on Bottlenose dolphins since they make up
# a majority of the dataset.

bottlenose <- cetacean %>%
  filter(species == "Bottlenose")

bottlenose %>%
  group_by(status) %>%
  tally() %>%
  arrange(desc(n))

bottlenose2 <- bottlenose %>%
  mutate(statusDate = replace_na(statusDate, lubridate::date("2017-05-07"))) %>%
  filter(status %in% c("Died", "Alive", "Released"),
         !is.na(originDate),
         acquisition %in% c("Born", "Capture", "Rescue")) %>%
  mutate(age = lubridate::time_length(lubridate::interval(originDate, statusDate), "years")) %>%
  arrange(desc(originDate)) %>%
  mutate(X1 = fct_inorder(as.factor(X1))) %>%
  sample_frac(1)

bottlenose2 %>%
  filter(status == "Alive") %>%
  group_by(acquisition) %>%
  tally()

ggplot(bottlenose2, aes(X1, color = status)) +
  geom_linerange(aes(ymin = originDate, ymax = statusDate), size=0.2) +
  facet_grid(. ~ acquisition, scales = "free_y", space = "free") +
  scale_color_manual(values=c("#2554C7", "#999999", "#C77324")) +
  labs(title = "Over 1,500 bottlenose dolphins have been held in U.S. facilities since 1949", 
       subtitle = "Of the 396 dolphins alive today, 69 were captured (17%), 310 were born in captivity (78%), and 17 were rescued (5%)",
       x = NULL, y = NULL,
       caption = "Source: | Graphic: @nsgrantham") +
  coord_flip() +
  dark_theme_minimal(base_family = "Fira Sans Extra Condensed Light") +
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        panel.grid.major = element_line(color = "grey30", size = 0.1),
        panel.grid.minor = element_line(color = "grey30", size = 0.1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(2, "line"),
        axis.text.y = element_blank(),
        legend.position = c(0.85, 1.08),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "grey10")
        )
