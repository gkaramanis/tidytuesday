library(tidyverse)
library(camcorder)
library(colorspace)
library(rvest)

# athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

# Read in from Wikipedia instead of filtering from athletes.csv
# because of typos and missing names in the dataset
gr_athletes <- read_html("https://en.wikipedia.org/wiki/Greece_at_the_Paralympics") %>% 
  html_nodes("table") %>% 
  .[[4]] %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  separate(games, into = c("year", "city"), sep = 4, remove = FALSE) %>% 
  mutate(
    year = as.numeric(year),
    games = str_replace(games, " ", "\n"),
    name = str_replace(name, "(Nikolaos Pananos)", ",\\1,"),
    medal = fct_relevel(medal, c("Gold", "Silver", "Bronze"))
    ) %>% 
  separate_rows(name, sep = ",") %>% 
  add_count(year, name, medal) %>% 
  group_by(name) %>% 
  mutate(min_year = min(year, na.rm = TRUE)) %>% 
  ungroup()

totals_year <- gr_athletes %>% 
  count(year, games, medal) %>% 
  mutate(name = "Total")

gr <- gr_athletes %>%
  bind_rows(totals_year) %>% 
  arrange(min_year) %>% 
  mutate(
    name = fct_inorder(name),
    min_year = if_else(name == "Total", 1980, min_year)
    )

## Totals (not used)
# totals_sport <- gr_athletes %>% 
#   count(sport, medal) %>% 
#   pivot_wider(names_from = medal, values_from = n) %>% 
#   mutate(
#     across(Gold:Bronze, replace_na, 0),
#     Total = Gold + Silver + Bronze
#     ) %>% 
#   add_row(sport = "Total") %>%
#   mutate(
#     Gold = ifelse(sport == "Total", sum(Gold, na.rm = TRUE), Gold),
#     Silver = ifelse(sport == "Total", sum(Silver, na.rm = TRUE), Silver),
#     Bronze = ifelse(sport == "Total", sum(Bronze, na.rm = TRUE), Bronze),
#     Total = ifelse(sport == "Total", sum(Total, na.rm = TRUE), Total)
#     )

# Plot
gg_record(dir = "temp", device = "png", width = 12, height = 12, units = "in", dpi = 320)

f1 = "October Compressed Devanagari"
f2 = "KyiVType Sans"

ggplot(gr, aes(x = year, y = 1, fill = medal)) +
  # medals
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n, y = 0.5, color = medal), position = position_dodge(width = 3.5), stat = "identity", family = f2, size = 3) +
  # athletes
  geom_text(aes(x = min_year - 2.5, y = 0.5, label = name), stat = "unique", hjust = 1, family = f2, color = "grey20", size = 3.5) +
  # scales
  scale_fill_manual(breaks = c("Gold", "Silver", "Bronze"), values = c("gold", "gainsboro", "darkgoldenrod3")) +
  scale_color_manual(breaks = c("Gold", "Silver", "Bronze"), values = darken(c("gold", "gainsboro", "darkgoldenrod3"), 0.5)) +
  scale_x_continuous(breaks = gr$year, limits = c(1970, 2018), labels = gr$games) +
  # facet, coord, theme
  facet_wrap(vars(name), ncol = 1, strip.position = "left", scales = "free_y") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Greek Paralympians",
    subtitle = "A timeline of athletes and medals won in the Summer Paralympics\nSource: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey95", color = NA),
    legend.position = "none",
    axis.text.x = element_text(margin = margin(15, 0, 0, 0), size = 15, color = "grey15", family = f1),
    strip.text.y.left = element_blank(),
    plot.margin = margin(20, 20, 20, 30),
    plot.title = element_text(margin = margin(500, 0, -550, 0), size = 60, family = f1, hjust = 0),
    plot.subtitle = element_text(margin = margin(560, 0, -590, 0), size = 20, family = f1, hjust = 0)
    )


# export gif
# gg_playback(frame_duration = 0.15, image_resize = 1080)
# convert to mp4 in terminal
# ffmpeg -i 2021_08_01_14_53_40.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" olympics_makingof.mp4

# ggsave(here::here("temp", paste0("paralympics-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

