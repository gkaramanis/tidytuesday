library(tidyverse)
library(textreadr)
library(ggbump)
library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 12, height = 8, dpi = 320)

full_paths <- data.frame(
  path = dir(path = here::here(), pattern = "\\.R", recursive = TRUE)
) %>% 
  mutate(
    document = str_remove(base_name(path), ".R"),
    year = parse_number(path)
  )

geoms <- read_dir(path = here::here(), pattern = "\\.R$", recursive = TRUE) %>% 
  left_join(full_paths) %>% 
  mutate(geom = str_extract(content, "geom_\\w+")) %>%
  filter(!is.na(geom), !is.na(year)) %>% 
  count(year, geom) %>% 
  # Add consecutive year grouping
  arrange(geom, year) %>%
  group_by(geom) %>%
  mutate(
    consec_group = data.table::rleid(c(TRUE, diff(year) == 1))
  ) %>%
  group_by(year) %>% 
  mutate(
    freq = n / sum(n),
    total_geoms = sum(n)
  ) %>% 
  ungroup()

top_10 <- geoms %>%
  group_by(year) %>% 
  slice_max(n, n = 10, with_ties = FALSE) %>% 
  arrange(n) %>% 
  mutate(i = factor(row_number())) %>% 
  ungroup()

c1 = "grey97" # background
c2 = "grey20" 

f1 <- "Outfit"

pal <- rev(c("#e6194B", "#3cb44b", "#000000", "#4363d8", "#911eb4", "#f58231", "#42d4f4",  "#f032e6", "#bfef45", "#469990", "#fabed4", "#dcbeff", "#9A6324", "#000075", "#800000", "#aaffc3", "#808000", "#FF7F50", "#20B2AA", "#DDA0DD", "#DAA520"))

# Bar chart
ggplot(top_10, aes(fill = geom)) +
  geom_col(aes(n, i), width = 0.6) +
  geom_text(aes(n + 1, i, label = n), size = 3 , hjust = 0, family = f1, color = c2) +
  geom_text(aes(-2, i, label = geom), size = 3 , hjust = 1, family = f1, fontface = "bold") +
  scale_fill_manual(values = pal) +
  facet_wrap(vars(year)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_text(color = c2),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(4, "lines"),
    plot.background = element_rect(fill = c1, color = NA),
    plot.margin = margin(0, 10, 0, 70)
  ) 


# Modified bump chart
ggplot(top_10, aes(year, i, color = geom, 
                   # Use interaction of geom and consec_group for grouping
                   group = interaction(geom, consec_group))) +
  geom_point(size = 6) +
  geom_bump(size = 3) +
  geom_text(data = top_10 %>% filter(year == min(year)), aes(x = year - 0.1, label = geom), size = 5, hjust = 1, family = f1, fontface = "bold") +
  geom_text(data = top_10 %>% filter(year == max(year)), aes(x = year + 0.1, label = geom), size = 5, hjust = 0, family = f1, fontface = "bold") +
  geom_text(data = top_10 %>% 
              group_by(geom) %>% 
              filter(year == max(year) & !year %in% c(max(top_10$year), min(top_10$year))), 
            aes(x = year, label = geom), nudge_y = 0.4, size = 5, hjust = 0.5, family = f1, fontface = "bold") +
  scale_color_manual(values = pal) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = c1, color = NA),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title = element_blank(),
    plot.margin = margin(10, 130, 10, 100)
  )
  
