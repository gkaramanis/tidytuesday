library(tidyverse)
library(ggsankey)
library(ggflags)
library(patchwork)
library(wesanderson)

# Read in data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')

drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# Fonts
f1 = "Fira Sans Compressed"

# Palette
pal <- wes_palette("Zissou1", 21, "continuous")
pal_mk <- wes_palette("Darjeeling1")

# Set theme
theme_set(
  theme_minimal(base_family = f1) +
    theme(
      legend.title = element_blank(),
      legend.key.size = unit(1, "line"),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "grey97", color = NA),
      plot.margin = margin(10, 10, 10, 10),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30")
    )
)

# Prepare data
drivers_top <- drivers %>%
  filter(position < 6) %>% 
  mutate(
    player_rec = paste0(player, " - ", total, " records"),
    player_rec = fct_reorder(player_rec, -total)
  )

nation_records <- drivers %>%
  mutate(nation = replace_na(nation, "Unknown")) %>% 
  group_by(nation) %>% 
  summarise(nation_total = sum(records, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(-nation_total) %>%
  mutate(nation = fct_reorder(nation, -nation_total)) %>% 
  add_column(code = c("us", "au", "nl", "de", "gb", "ca", "at", "br", "ie", "hr", "no", NA, "fr", "si"))

track_records <- records %>% 
  count(track, type)
  
system_records <- records %>% 
  count(track, system_played)

drivers_pos <- drivers_top %>% 
  distinct(player, position)

records_sankey <- records %>% 
  filter(player %in% drivers_top$player) %>% 
  make_long(player, track) %>% 
  left_join(drivers_pos, by = c("node" = "player")) %>% 
  mutate(color = if_else(!is.na(next_node), pal_mk[position], "grey60"))


# Drivers plot -----------------------------------------------------
dp <- ggplot(drivers_top) +
  geom_point(aes(x = year, y = 0, size = records, fill = records), shape = 21) +
  scale_y_continuous(breaks = c(0, 50, 100, 150), labels = c("0", "", "", "150")) +
  scale_fill_gradientn(colors = pal) +
  facet_wrap(vars(player_rec), ncol = 1) +
  labs(
    title = "The Drivers",
    subtitle = "Top 5 drivers with the most records"
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )


# Nations plot ------------------------------------------------------------
np <- ggplot(nation_records) +
  geom_flag(aes(x = 0, y = 1.1, country = code), size = 9) +
  geom_text(aes(x = 0, y = -0.4, label = nation_total), family = f1, color = "grey40") +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 2)) +
  facet_wrap(vars(nation)) +
  theme_void(base_family = f1) +
  labs(
    subtitle = "Total records by nation (all drivers)"
  ) +
  theme(
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0), color = "grey30")
  )


# Sankey plot -------------------------------------------------------------
skp <- ggplot(records_sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = color, label = node)) +
  geom_sankey() +
  geom_sankey_text(aes(hjust = if_else(x == "track", 1, 0)),
                   family = f1, size = 3) +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = "Mario Kart 64 World Records",
    caption = "Source: Mario Kart World Records · Graphic: Georgios Karamanis"
    ) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 23, family = f1, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )


# Track records -----------------------------------------------------------
tp <- ggplot(track_records) +
  geom_col(aes(x = n, y = track, fill = type), position = "dodge", width = 0.7) +
  scale_x_continuous(limits = c(0, 170)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pal_mk[4:5]) +
  labs(
    title = "The Tracks",
    subtitle = "Records by track and number of laps"
  ) +
  theme(
    legend.position = c(0.875, 0.5)
  )


# System records -----------------------------------------------------------
sp <- ggplot(system_records) +
  geom_col(aes(x = n, y = track, fill = system_played), position = "dodge", width = 0.7) +
  scale_x_continuous(limits = c(0, 170)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = pal_mk[2:3]) +
  labs(
    subtitle = "Records by track and system played"
  ) +
  theme(
    legend.position = c(0.875, 0.55)
  )

p <- (dp/np 
      + plot_layout(heights = c(1.3, 1))) | 
  skp | 
  (tp/sp + 
     plot_layout(heights = c(1, 1.05)))

p +
  ggsave(here::here("temp", paste0("mario-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 12)

# library(rtweet)
# post_tweet(
#   status = "my first rtweet #rstats",
#   media = NULL,
#   token = NULL,
#   in_reply_to_status_id = NULL,
#   destroy_id = NULL,
#   retweet_id = NULL,
#   auto_populate_reply_metadata = FALSE,
#   media_alt_text = NULL,
#   lat = NULL,
#   long = NULL,
#   display_coordinates = FALSE
# )