library(tidyverse)
library(camcorder)
library(ggrepel)

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

taxonomy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/taxonomy.csv')

gg_record(dir = "temp", device = "png", width = 9, height = 11, units = "in", dpi = 320)

lem_longev <- lemurs %>% 
  distinct(dlc_id, taxon, sex, age_max_live_or_dead_y) %>% 
  filter(sex != "ND") %>% 
  group_by(taxon, sex) %>% 
  summarise(longev = max(age_max_live_or_dead_y, na.rm = TRUE), n = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = taxon, names_from = sex, values_from = c(longev, n)) %>% 
  group_by(taxon) %>% 
  mutate(
    color = if_else(longev_F > longev_M, "#3A8AD8", "#F2A039"),
    taxon_id = cur_group_id()
    ) %>% 
  ungroup() %>% 
  mutate(taxon = if_else(taxon == "CMED", "CMEAD", taxon)) %>% 
  left_join(taxonomy)

f1 = "Fira Sans Compressed"
f2 = "Faune"
col_bg = "grey96"

ggplot(lem_longev) +
  # years axis
  annotate("segment", x = 1.1, xend = 1.9, y = seq(10, 40, by = 1), yend = seq(10, 40, by = 1), alpha = 0.2, size = 0.2) +
  annotate("point", x = 1.5, y = seq(10, 40, by = 5), size = 16, color = col_bg) +
  annotate("text", x = 1.5, y = seq(10, 40, by = 5), label = seq(10, 40, by = 5), size = 8, alpha = 0.2, family = f2, fontface = "bold", color = "darkgreen") +
  # sex annotation
  annotate("text", x = c(0.9, 2.1), y = 11.25, label = c("Female", "Male"), hjust = c(1, 0), family = f2, fontface = "bold", size = 10, alpha = 0.3, color = "darkgreen") +
  # lollipops
  geom_segment(aes(y = longev_F, yend = longev_M, x = 1, xend = 2, color = color)) +
  geom_point(aes(y = longev_F, x = 1, color = color, size = n_F)) +
  geom_point(aes(y = longev_M, x = 2, color = color, size = n_M)) +
  # species labels
  geom_text_repel(data = subset(lem_longev, taxon_id %% 2 != 0),
                  aes(y = longev_M, x = 2, label = latin_name),
                  nudge_x = 0.1, hjust = 0, direction = "y", family = f1,
                  point.padding = 1, segment.size = 0.2, color = "grey15") +
  geom_text_repel(data = subset(lem_longev, taxon_id %% 2 == 0),
                  aes(y = longev_F, x = 1, label = latin_name),
                  nudge_x = -0.1, hjust = 1, direction = "y", family = f1,
                  point.padding = 1, segment.size = 0.2, color = "grey15") +
  # scales, themes, etc.
  scale_x_continuous(limits = c(0.5, 2.5)) +
  scale_size_continuous(range = c(0.5, 5), breaks = seq(0, 120, by = 20), name = "Total number of lemurs hosted at DLC by species and sex") +
  scale_color_identity() +
  labs(
    title = "Longest lived lemurs at the Duke Lemur Center",
    subtitle = "Maximum age recorded in years (living or dead), by species and sex",
    caption = "Source: Duke Lemur Center · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    legend.position = "bottom",
    legend.text = element_text(family = f1, color = "darkgreen"),
    legend.title = element_text(family = f1, color = "darkgreen"),
    legend.margin = margin(10, 0, 0, 0),
    plot.title = element_text(hjust = 0.5, size = 28, family = f2, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(margin = margin(7, 0, 0, 0), hjust = 0.5, size = 18, family = f1, color = "grey30"),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "grey30", family = f1),
    plot.margin = margin(20, 20, 20, 20)
  )

# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" makingof.mp4

# ggsave(here::here("temp", paste0("lemurs-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

