library(tidyverse)

ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

round_stages_list <- c(
  "Qualifying (Regional/City)",
  "Finals (Regional/City)",
  "Qualifying",
  "Semi-Finals",               
  "National Finals - Stage 1",
  "National Finals - Stage 2",
  "National Finals - Stage 3",
  "National Finals - Stage 4"
)

pal <- data.frame(color = c('#e6194B', '#3cb44b', '#000075', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#aaffc3', '#800000')) %>% mutate(row_n = row_number())

obstacles_color <-  ninja_warrior %>%
  count(obstacle_name, sort = TRUE) %>% 
  mutate(row_n = row_number()) %>% 
  left_join(pal) %>% 
  mutate(color = replace_na(color, "grey45"))

nw_obstacles <- ninja_warrior %>% 
  mutate(
    round_stage_n = case_when(
      round_stage == round_stages_list[1] | round_stage == round_stages_list[3] ~ 1,
      round_stage == round_stages_list[2] | round_stage == round_stages_list[4] ~ 2,
      round_stage == round_stages_list[5] ~ 3,
      round_stage == round_stages_list[6] ~ 4,
      round_stage == round_stages_list[7] ~ 5,
      round_stage == round_stages_list[8] ~ 6,
    )
    ) %>%
  group_by(season, round_stage) %>% 
  mutate(
    location_n = as.integer(factor(location))
    ) %>% 
  ungroup() %>% 
  left_join(obstacles_color)

ggplot(nw_obstacles) +
  geom_step(aes(round_stage_n * 10 + obstacle_order, season  + location_n / 7, group = obstacle_name, color = color), size = 0.15) +
  geom_point(aes(round_stage_n * 10 + obstacle_order, season + location_n / 7, color = color), size = 1.75) +
  geom_text(aes(round_stage_n * 10, if_else(round_stage == round_stages_list[1] | round_stage == round_stages_list[2], -0.5, 0), label = round_stage), stat = "unique", size = 3, hjust = 0) +
  scale_y_reverse() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#182240", color = NA)
  ) +
  ggsave(here::here("temp", paste0("ninja-warrior-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 9)

