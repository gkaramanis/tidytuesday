library(tidyverse)
library(ggtext)
library(cowplot)

ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

round_stages_list <- c(
  "Qualifying",
  "Semi-Finals/\nRegional Finals",
  "National Finals\nStage 1",
  "National Finals\nStage 2",
  "National Finals\nStage 3",
  "National Finals\nStage 4"
)

obstacles <- ninja_warrior %>% 
  distinct(season, round_stage, obstacle_name, obstacle_order) %>% 
  add_count(obstacle_name) %>% 
  mutate(
    round_stage = case_when(
      str_detect(round_stage, "Qualifying") ~ "Qualifying",
      str_detect(round_stage, "Semi") ~ "Semi-Finals/\nRegional Finals",
      str_detect(round_stage, "Finals \\(Regional") ~ "Semi-Finals/\nRegional Finals",
      str_detect(round_stage, "National Finals") ~ str_replace(round_stage, " - ", "\n"),
      TRUE ~ round_stage
    ),
    round_stage = fct_relevel(round_stage, round_stages_list)
    ) %>% 
  mutate(color = case_when(
    obstacle_name == "Rope Ladder" ~ "mediumpurple2",
    str_detect(obstacle_name, "Rope") ~ "#D8282B",
    str_detect(obstacle_name, "Ladder") ~ "#284C88",
    TRUE ~ "grey85"
  ) 
  )

# list of rope obstacles
ropes <- obstacles %>%
  filter(str_detect(obstacle_name, "Rope")) %>% 
  filter(obstacle_name != "Rope Ladder") %>% 
  distinct(obstacle_name) %>% 
  pull() %>% 
  paste(collapse = ", ")

# list of ladder obstacles
ladders <- obstacles %>%
  filter(str_detect(obstacle_name, "Ladder")) %>% 
  filter(obstacle_name != "Rope Ladder") %>% 
  distinct(obstacle_name)  %>% 
  pull() %>% 
  paste(collapse = ", ")

p <- ggplot() +
  # gray lines and points
  geom_path(data = subset(obstacles, color == "grey85"), aes(obstacle_order, season, group = obstacle_name, color = color), size = 1.2, lineend = "round") +
  geom_point(data = subset(obstacles, color == "grey85"), aes(obstacle_order, season, color = color), size = 2.5) +
  # ropes and ladders
  geom_path(data = subset(obstacles, color != "grey85"), aes(obstacle_order, season, group = obstacle_name, color = color), size = 1.2, lineend = "round") +
  geom_point(data = subset(obstacles, color != "grey85"), aes(obstacle_order, season, color = color), size = 2.5) +
  # scales, etc
  scale_y_reverse(breaks = 1:10, sec.axis = dup_axis()) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(round_stage), nrow = 1) +
  # title, etc
  labs(
    title = "<span style = 'color:#D8282B;'>Ropes</span><span> and </span><span style = 'color:#284C88;'>Ladders</span>",
    subtitle = "in American Ninja Warrior",
    caption = "Source:Data.World/Sasukepdia | Graphic: Georgios Karamanis",
    x = "Obstacle Order in Round",
    y = "Season"
  ) +
  # theme
  theme_minimal(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "grey96", color = NA),
    panel.spacing.x = unit(0.9, "lines"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 9, family = "JetBrains Mono Bold", hjust = 0),
    plot.title = element_textbox_simple(size = 42, family = "Diamante Bold", hjust = 0.552, color = "#4B5259", width = unit(5, "in")),
    plot.subtitle = element_text(size = 36, family = "Diamante Bold", hjust = 0.5, color = "#4B5259", margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(size = 8, family = "JetBrains Mono", hjust = 1, color = "grey30", margin = margin(-5, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 9, hjust = 0),
    axis.title.y.left = element_text(margin = margin(0, 15, 0, 0), size = 9, hjust = 1),
    axis.title.y.right = element_text(margin = margin(0, 0, 0, 15), size = 9, hjust = 0),
    plot.margin = margin(30, 20, 20, 20)
  ) 

ggdraw(p) +
  # title legend
  draw_line(x = c(0.0, 0.308), y = 0.923, size = 17, color = "#D8282B") +
  draw_line(x = c(0.686, 1), y = 0.923, size = 17, color = "#284C88") +
  draw_label(label = paste0(str_wrap(ropes, 50)), 0.3, 0.924, hjust = 1, size = 7.5, color = "grey96", fontfamily = "JetBrains Mono Bold") +
  draw_label(label = paste0(str_wrap(ladders, 55)), 0.695, 0.924, hjust = 0, size = 7.5, color = "grey96", fontfamily = "JetBrains Mono Bold") +
  # borders
  draw_line(x = c(0.5, 0, 0, 0.5), y = c(1, 1, 0, 0), size = 4, color = "#D8282B") +
  draw_line(x = c(0.5, 1, 1, 0.5), y = c(1, 1, 0, 0), size = 4, color = "#284C88") +
  # annotations
  draw_label(label = paste0("Rope Ladder"), 0.49, 0.68, angle = -90, size = 9, color = "mediumpurple2", fontfamily = "JetBrains Mono Bold") +
  draw_label(label = paste0("Salmon Ladder"), 0.284, 0.21, angle = 90, size = 9, color = "#284C88", fontfamily = "JetBrains Mono Bold") +
  draw_label(label = paste0("Tarzan Rope"), 0.444, 0.673, angle = 90, size = 9, color = "#D8282B", fontfamily = "JetBrains Mono Bold") +
  draw_label(label = paste0("Double Salmon Ladder"), 0.54, 0.58, angle = -90, size = 9, color = "#284C88", fontfamily = "JetBrains Mono Bold") +
  draw_label(label = paste0("Rope Climb"), 0.825, 0.62, angle = -90, size = 9, color = "#D8282B", fontfamily = "JetBrains Mono Bold") +
  draw_label(label = paste0("Bungee Rope Climb"), 0.745, 0.59, angle = -90, size = 9, color = "#D8282B", fontfamily = "JetBrains Mono Bold") +
  
  ggsave(here::here("temp", paste0("ninja-warrior-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 8.5)

