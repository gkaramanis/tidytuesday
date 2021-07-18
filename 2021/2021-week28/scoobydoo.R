library(tidyverse)
library(lubridate)
library(camcorder)
library(shadowtext)
library(colorspace)
library(ggrepel)

# Read in data
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# Colors
pal <- tribble(
  ~"character", ~"color",
  "Velma", "#E8AD64",
  "Scooby", "#846645",
  "Shaggy", "#9FBC76",
  "Daphnie", "#6B4D87",
  "Fred", "#3B69B4"
  )

# Fonts
f1 = "Fira Sans"
f1cc = "Fira Sans Compressed"
f2 = "KyivType Serif"

gg_record(dir = "temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# Get timelines for actual air dates
series <- scoobydoo %>% 
  filter(format == "TV Series") %>% 
  group_by(series_name) %>% 
  arrange(date_aired) %>% 
  summarise(
    start_series = first(date_aired),
    end_series = last(date_aired),
    duration_series = as.numeric(end_series - start_series),
    duration_series = pmax(60, as.numeric(duration_series)), # set a *minimum* value for duration so that 0 (TV specials) becomes 60 and therefore visible
    duration_label = if_else(duration_series != 60, paste0(year(start_series), "-", year(end_series)), as.character(year(start_series))),
    series_label = str_wrap(paste0(series_name, " (", duration_label, ")"), 20)
  ) %>% 
  ungroup()

# Get timelines for actors
voice_actors <- scoobydoo %>% 
  select(date_aired, series_name, title, format, ends_with("_va")) %>% 
  pivot_longer(cols = ends_with("_va"), names_to = "character", values_to = "actor") %>% 
  filter(actor != "NULL") %>% 
  filter(format == "TV Series") %>% # Keep tv series only
  group_by(character, actor) %>% 
  arrange(date_aired) %>% 
  mutate(
    start_va = first(date_aired),
    end_va = last(date_aired),
    duration_va = end_va - start_va
    ) %>% 
  ungroup() %>% 
  select(series_name, character:duration_va) %>%
  distinct() %>% 
  mutate(
    # Y offset for overlapping bars
    nudge_va = case_when(
      actor == "Scott Innes" & character == "shaggy_va" ~ -0.15,
      actor != "Scott Innes" & character == "shaggy_va" ~ 0.15,
      actor == "Carl Steven" & character == "fred_va" ~ -0.15,
      actor != "Carl Steven" & character == "fred_va" ~ 0.15,
      TRUE ~ 0
      ),
    actor = str_replace(actor, "Marry", "Mary"), # Fix name
    actor = if_else(str_detect(actor, "Stef|Berg|Menv"), paste0("⬉ ", actor), actor), # Add arrow to some names
    character = str_to_title(str_remove(character, "_va")),
    # Y offset for character labels depending on number of bar rows
    character_y = case_when(
      character %in% c("shaggy_va", "fred_va") ~ as.numeric(factor(character)),
      TRUE ~ as.numeric(factor(character)) - 0.15
      ),
    char_label_nudge = case_when(
      nudge_va == 0 ~ -0.15,
      TRUE ~ 0
    ),
    # Same for actor names
    va_label_nudge = case_when(
      str_detect(actor, "Stef|Berg|Men") ~ -0.2,
      TRUE ~ 0
    ),
    va_hjust = if_else(va_label_nudge != 0, 0, 0.5),
    duration_va = pmax(60, as.numeric(duration_va)) # set a *minimum* value for duration so that 0 (Mary Kay B) becomes 60 and therefore visible
    ) %>% 
  left_join(pal) %>% 
  left_join(series)
  

# Plot
ggplot(voice_actors, aes(x = start_va + duration_va / 2,
                         y = character_y - nudge_va)) +
  # Bars for actual series air dates
  geom_tile(aes(x = start_series + duration_series/2, y = character_y - nudge_va + 0.12, width = duration_series, fill = darken(color, 0.6)), height = 0.025, stat = "unique", color = NA) +
  # Series names and times
  geom_text_repel(data = subset(voice_actors, character == "Fred"), aes(x = start_series + duration_series/2, y = 2 - nudge_va, label = series_label), stat = "unique", size = 2.5, family = f1cc, lineheight = 0.9, force_pull = 0, nudge_y = -0.025, direction = "y", hjust = 0.5, segment.color = darken("#D98A9E", 0.5), segment.size = 0.3, seed = 42) +
  # Actors bars
  geom_tile(aes(fill = color, color = darken(color, 0.5), width = duration_va - 30), height = 0.2, size = 0.5) +
  # Actor labels
  geom_shadowtext(aes(y = character_y - nudge_va - va_label_nudge, label = actor, hjust = va_hjust, bg.colour = darken(color, 0.5)), family = f1cc, color = "grey97", fontface = "bold", size = 3.5) +
  # Character labels
  geom_shadowtext(aes(x = as.Date("1969-09-01"),
                y = character_y - char_label_nudge,
                label = character),
            stat = "unique", nudge_y = 0.4, hjust = 0, family = f2, fontface = "bold", color = "grey97", size = 7) +
  # Custom grob with shadowtext for title
  annotation_custom(grob = grid.shadowtext("Scooby-Doo TV Show Voice Actors", gp = gpar(cex = 3, col = "#74529C", fontfamily = f2, fontface = "bold"), bg.colour = "#EEB358"), ymin = 0.3) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_date(breaks = seq(as.Date("1970-01-01"), as.Date("2020-01-01"), "10 years"), date_labels = "%Y") +
  scale_y_reverse() +
  coord_cartesian(clip = "off") +
  labs(
    # title = "Scooby-Doo TV Show Voice Actors",
    caption = "Source: Kaggle · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#D98A9E", color = NA),
    legend.position = "none",
    axis.text.x = element_text(family = f2, color = "grey97", size = 12),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = darken("#D98A9E", 0.2), size = 0.3, linetype = "dashed"),
    panel.grid.minor.x = element_line(color = darken("#D98A9E", 0.2), size = 0.2, linetype = "dashed"),
    plot.margin = margin(75, 20, 20, 20),
    # plot.title = element_shadowtext(size = 28, hjust = 0.5, family = f2, face = "bold", color = "#74529C", margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 9, margin = margin(20, 0, 0, 0), color = darken("#D98A9E", 0.7))
  )

# export gif
# gg_playback(frame_duration = 0.15, image_resize = 1080)
# convert to mp4 in terminal
# ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" video.mp4
