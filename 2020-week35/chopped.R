library(tidyverse)
library(ggforce)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

cuts <- chopped %>%
	filter(str_detect(episode_notes, "cut h")) %>% 
	count(season) %>% 
  mutate(
    n = as.numeric(n),
    n = case_when(
    season == 17 | season == 39 ~ n + 1, # Two cuts in S17E11 and S39E08
    TRUE ~ n
  ))

# Cleaver blade
blade <- data.frame(
  x = c(0.5, 46.5, 46.5, 46, 45.5, 45, 43, 0.5),
  y = c(0, 0, 17.5, 18.5, 19, 19.5, 20, 20),
  color = "#E0E0E0"
  )

# Cleaver handle
handle <- data.frame(
  x = c(-30, 5, 5, -30),
  y = c(20, 20, 14, 14),
  color ="#55555B"
  )

# Colors and fonts
bg <- "#F2B953"
f1 = "Proxima Nova"
f1b = "Proxima Nova Bold"
f2b = "Publico Headline Bold"

# Plot
ggplot(cuts) +
  # Handle
  geom_shape(data = handle, aes(x = x, y = y - 0.5, fill = color), radius = unit(0.5, 'cm')) +
  annotate("point", x = c(-3, -7, -11), y = 16.5, size = 4.5, color = "#E0E0E0") +
  # Blade
  geom_polygon(data = blade, aes(x = x, y = y - 0.5, fill = color)) +
  annotate("point", x = 43.5, y = 16, size = 9, color = bg) +
  # Title on blade
  annotate("tile", x = 23.5, y = 0, height = 2, width = 46, fill = "#F2F3FB", color = NA) +
  # Y axis - cuts per season
  annotate("segment", x = -2, xend = 46, y = 5 * -1:-5, yend = 5 * -1:-5, color = "grey97", size = 0.2) +
  annotate("text", x = -4, y = 5 * -1:-5, label = 1:5, color = "grey97", family = f1, size = 5) +
  annotate("text", x = -8, y = -5, label = "Number of cut injuries", hjust = 1, family = f1b, color = "grey97", size = 6) +
  # X axis - seasons
  annotate("text", x = seq(5, 45, by = 5), y = 2, label = seq(5, 45, by = 5), family = f1, size = 4.5) +
  annotate("tile", x = seq(5, 45, by = 5), y = 0, height = 2, width = 0.25, fill = "#E0E0E0", color = NA) +
  annotate("text", x = -8, y = 2, label = "Season", hjust = 1, family = f1b, size = 6) +
  # Blood!
	geom_col(aes(x = season, y = -5 * n), fill = "#AA0000") +
  # Title
	annotate("text", x = 4, y = 16, label = "Cut injuries in Chopped", hjust = 0, size = 8, family = f2b, vjust = 0.6) +
	annotate("text", x = 4.4, y = 13, label = "Cuts per season, as mentioned in episode notes", hjust = 0, size = 4, family = f1, vjust = 1, lineheight = 0.9) +
  labs(caption = "Source: Kaggle | Graphic: Georgios Karamanis") +
  # Scales and theme
  scale_fill_identity() +
	theme_void() +
  theme(
    plot.background = element_rect(fill = bg, color = NA),
    plot.margin = margin(20, 25, 20, 25),
    plot.caption = element_text(family = f1, margin = margin(40, 0, 0, 0), size = 6.5, vjust = 0)
  ) +
	ggsave(here::here("temp", paste0("chopped-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 6)
