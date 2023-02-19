library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 14, height = 11, units = "in", dpi = 320)

age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv') %>% 
  mutate(id = row_number(), .before = 1)

actors1 <- age_gaps %>% 
  count(actor_1_name, name = "n1") %>% 
  rename(actor = 1)

actors2 <- age_gaps %>% 
  count(actor_2_name, name = "n2") %>% 
  rename(actor = 1)

actors <- actors1 %>% 
  left_join(actors2) %>% 
  mutate(
    n2 = replace_na(n2, 0),
    n = n1 + n2
  ) %>% 
  arrange(-n) %>% 
  head(9)

top_diff_1 <- age_gaps %>% 
  filter(actor_1_name %in% actors$actor) %>% 
  select(id, actor_name = actor_1_name, actor_age = actor_1_age, release_year, age_difference)

top_diff_2 <- age_gaps %>% 
  filter(actor_2_name %in% actors$actor) %>% 
  mutate(age_difference = -age_difference)  %>% 
  select(id, actor_name = actor_2_name, actor_age = actor_2_age, release_year, age_difference)

actors2 <- age_gaps %>% 
  select(id, actor_1_name, actor_1_age, actor_2_name, actor_2_age)

top_diff <- bind_rows(top_diff_1, top_diff_2) %>% 
  mutate(actor2_age = actor_age - age_difference) %>% 
  left_join(actors2) %>% 
  mutate(
    second_actor = if_else(actor_name == actor_1_name, actor_2_name, actor_1_name)
    ) %>% 
  group_by(actor_name) %>% 
  mutate(
    max_diff = if_else(age_difference == max(age_difference), age_difference, NA),
    min_diff = if_else(age_difference == min(age_difference), age_difference, NA)
  ) %>% 
  ungroup()


f1 <- "Outfit"
f2 <- "Domaine Display"

ggplot(top_diff) +
  geom_segment(aes(x = 1, xend = 2, y = actor_age, yend = actor2_age, color = age_difference > 0), linewidth = 0.35) +
  geom_point(aes(1, actor_age, color = age_difference > 0), size = 0.5) +
  geom_point(aes(2, actor2_age, color = age_difference > 0), size = 0.5) +
  # Max diff, actor 1, age
  geom_text(data = . %>% filter(!is.na(max_diff)), aes(0.9, actor_age, label = actor_age), hjust = 1, family = f1, size = 3, fontface = "bold") +
  # Max diff, actor 2 age and name
  geom_text(data = . %>% filter(!is.na(max_diff)), aes(2.1, actor2_age, label = paste0(second_actor, ", ", actor2_age, " (", -age_difference, ")"), vjust = if_else(actor_name == "Harrison Ford", 1, 0.5)), hjust = 0, family = f1, size = 3, fontface = "bold") +
  # Min diff, actor 1, age
  geom_text(data = . %>% filter(!is.na(min_diff)), aes(0.9, actor_age, label = actor_age), hjust = 1, family = f1, size = 3) +
  # Min diff, actor 2 age and name
  geom_text(data = . %>% filter(!is.na(min_diff)), aes(2.1, actor2_age, label  = paste0(second_actor, ", ", actor2_age, " (", -age_difference, ")"), vjust = if_else(actor_name == "Harrison Ford", 0, 0.5)), hjust = 0, family = f1, size = 3) +
  scale_x_continuous(limits = c(0.8, 3)) +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_color_manual(values = c("#E1A34D", "#167AA2")) +
  facet_wrap(vars(actor_name)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Age difference between movie love interests",
    subtitle = str_wrap("Showing the 9 actors with the most movies in the Hollywood Age Gap database. On the left is the age of each of the 9 actors and on the right the age of their partner. The text labels highlight the biggest age differences for each actor.", 90),
    caption = "Source: Hollywood Age Gap · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_text(face = "bold", family = f2, size = 16),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(hjust = 0.5),
    panel.background = element_rect(color = "grey70", fill = "grey99"),
    plot.margin = margin(10, 20, 10, 20)
  )
