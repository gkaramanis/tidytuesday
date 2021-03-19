library(tidyverse)
library(lubridate)
library(ggstream)
library(wesanderson)
library(colorspace)
library(scales)
library(cowplot)

games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

# Create franchises
franchises <- games %>% 
  mutate(
    month_year = parse_date(paste0(month, " ", year), "%B %Y"),
    franchise = case_when(
      str_detect(gamename, "Grand Theft") ~ "Grand Theft Auto",
      str_detect(gamename, "Sid Meier's") ~ "Civilization",
      str_detect(gamename, "Call of Duty") ~ "Call of Duty",
      str_detect(gamename, "FINAL FANTASY") ~ "Final Fantasy",
      str_detect(gamename, "Assassin's Creed") ~ "Assassin's Creed",
      str_detect(gamename, "NBA 2K") ~ "NBA 2K",
      str_detect(gamename, "Resident Evil") ~ "Resident Evil",
      str_detect(gamename, "Tomb Raider") ~ "Tomb Raider",
      str_detect(gamename, "Tom Clancy") ~ "Tom Clancy's",
      TRUE ~ ""
    )
  ) %>% 
  filter(franchise != "")

nba2k <- franchises %>% 
  # left_join(franchise_games) %>% 
  filter(franchise == "NBA 2K") %>% 
  mutate(peak_avg_diff = peak - avg) %>% 
  pivot_longer(cols = c(avg, peak_avg_diff), names_to = "metric")

# Colors and fonts
pal1 <- wes_palette("Darjeeling1")
pal2 <- darken(pal1, 0.2)
pal <- c(rbind(pal1, pal2))

bg <- "#555665"

f1b <- "Publico Headline Bold"
f2 <- "Proxima Nova"
f2b <- "Proxima Nova Bold"
f2bb <- "Proxima Nova Extrabold"

gr1 <- "grey85"
gr2 <- "grey97"


p <- ggplot(nba2k, aes(x = month_year, y = value, fill = interaction(metric, gamename), label = if_else(metric == "peak_avg_diff", gamename, ""))) +
  geom_stream() +
  geom_stream_label(family = f2bb, size = 6, color = gr2) +
  #title
  annotate("text", x = date("2016-09-01"), y = 75000,
           label = "Average and peak number\nof players at the same time", hjust = 0, size = 9, color = gr2, family = f2b, lineheight = 0.8) +
  # caption
  annotate("text", x = date("2016-12-01"), y = -110000, label = "Source: SteamCharts\nGraphic: Georgios Karamanis", size = 3, color = gr1, hjust = 0, family = f2) +
  # scales, theme
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = label_number_si(), breaks = breaks_width(25000), position = "right") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg, color = NA),
    panel.grid.major.x = element_line(size = 0.5, color = lighten(bg, 0.1)),
    # panel.grid.major.y = element_line(size = 0.35, color = lighten(bg, 0.1)),
    axis.text.x = element_text(family = f1b, color = lighten(bg, 0.8), margin = margin(10, 0, 0, 0), size = 14),
    axis.text.y = element_text(family = f1b, color = lighten(bg, 0.8), size = 12, hjust = 0),
    plot.margin = margin(20, 20, 20, 20)
  )


# legend
pal_leg <- c(desaturate(pal[1:8]), pal[9:10])

l <- ggplot(nba2k, aes(x = month_year, y = value, fill = interaction(metric, gamename), label = if_else(metric == "peak_avg_diff", gamename, ""))) +
  geom_stream() +
  # annotatations
  annotate("linerange", x = date("2021-05-01"), ymin = - 117000, ymax = -38000, color = pal[10], size = 1) +
  annotate("text", x = date("2021-05-01"), y = -28000, angle = 90, 
           label = "average", hjust = 0, vjust = 0.25,
           color = pal[10], family = f1b) +
  annotate("linerange", x = date("2021-08-15"), ymin = - 117000, ymax = 60000,color = pal[9], size = 1) +
  annotate("text", x = date("2021-08-15"), y = 70000, angle = 90,
           label = "peak", hjust = 0, vjust = 0.25,
           color = pal[9], family = f1b) +
  scale_fill_manual(values = pal_leg) +
  scale_y_continuous(labels = label_number_si(), breaks = breaks_width(25000), position = "right") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(legend.position = "none")

# plot with legend and logos
nba2k_logo <- here::here("2021", "2021-week11", "img", "nba_2k.png")
steam_logo <- here::here("2021", "2021-week11", "img", "steam.png")

ggdraw(p) +
  draw_plot(l, 0.1, 0.1, 0.275, 0.225) +
  draw_image(steam_logo, x = -0.4, y = 0.4, scale = 0.08) +
  draw_image(nba2k_logo , x = -0.225, y = 0.4, scale = 0.25) +
  ggsave(here::here("temp", paste0("games-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 7, width = 10)

