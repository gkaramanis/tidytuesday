library(tidyverse)
library(here)

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

meets <- ipf_lifts %>% 
  pivot_longer(names_to = "lift", values_to = "weight", best3squat_kg:best3deadlift_kg) %>% 
  mutate(
    # remove numbers from meet_names
    meet_name = str_remove(meet_name, "48th|6th|3rd"),
    # convert negative and NA weights to 0
    weight = if_else(weight < 0 | is.na(weight), 0, weight)
    ) %>%
  # find max weight per meet, date and lift
  group_by(meet_name, date, lift) %>% 
  mutate(weight = max(weight)) %>% 
  distinct(meet_name, date, lift, weight) %>% 
  # remove 0 weights
  filter(weight != 0) %>% 
  # keep meets with more data points
  group_by(meet_name) %>% 
  filter(n() > 15) %>% 
  ungroup() %>% 
  # convert long names to two lines
  mutate(meet_name = str_replace(meet_name, " Championship", "\nChampionship"))

ggplot(meets, aes(date, weight, color = lift, fill = lift)) +
  geom_line(aes(group = lift), size = 1.5) +
  scale_x_date() +
  scale_color_manual(values = c("#003f5c", "#bc5090", "#ffa600"), labels = c("Bench", "Deadlift", "Squat")) +
  guides(color = guide_legend(title = "")) +
  labs(
    title = "Maximum weight lifted in the 9 biggest IPF meets",
    subtitle = "By lift, no distinction between raw and equipped"
  ) +
  facet_wrap(vars(meet_name), ncol = 3) +
  theme_minimal(base_family = "IBM Plex Sans", base_size = 16) +
  theme(
    legend.position = "top",
    plot.title = element_text(family = "IBM Plex Sans Bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(family = "IBM Plex Sans Medium", size = 16, hjust = 0.5),
    strip.text = element_text(family = "IBM Plex Sans Bold", size = 16),
    strip.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey70"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  ggsave(
    here::here("week-41", "figures", "temp", paste0("powerlifting-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
    width = 16, height = 11, dpi = 320
  )
