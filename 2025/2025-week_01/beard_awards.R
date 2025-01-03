library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

# Using a dataset from Kaggle
jb_awards <- read_csv(here::here("2025/2025-week_01/data/james-beard-awards.csv"))

jb_totals <- jb_awards %>% 
  select(-recipient_id) %>% 
  distinct() %>% 
  add_count(recipient_name, name = "total") %>% 
  group_by(recipient_name) %>% 
  arrange(year) %>% 
  mutate(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    years_since_first = year - first_year,
    cumsum_n = row_number()
  ) %>% 
  ungroup()

f1 <- "Radio Canada Condensed"
f2 <- "Proxima Nova"

ggplot(jb_totals, aes(years_since_first, cumsum_n, group = recipient_name)) +
  # Non-highlighted
  geom_step(data = . %>% filter(total < 25), linewidth = 0.15, color = "grey50") +
  # Highlighted
  geom_step(data = . %>% filter(total >= 25 | last_year - first_year >= 30), aes(color = recipient_name), linewidth = 0.5) +
  # All nominations
  geom_point(data = . %>% filter((total >= 25 | last_year - first_year >= 30)), aes(color = recipient_name), size = 1) +
  # Winner stars
  shadowtext::geom_shadowtext(data = . %>% filter((total >= 25 | last_year - first_year >= 30) & award_status == "Winner"), aes(color = recipient_name, label = "★"), bg.color = "grey99", family = f1) +
  # 
  shadowtext::geom_shadowtext(data = . %>% filter((total >= 25 | last_year - first_year >= 30) & year == last_year), aes(color = recipient_name, label = recipient_name), bg.color = "grey99", hjust = 0, nudge_x = 0.5, vjust = 0.2, family = f1, fontface = "bold") +
  scale_linewidth_identity() +
  MetBrewer::scale_color_met_d("Redon") +
  coord_cartesian(clip = "off", xlim = c(0, 37)) +
  labs(
    title = "James Beard Awards",
    subtitle = str_wrap("Tracking the most decorated nominees and those with the longest spans from first to last nomination (1991-2024). Stars (★) indicate winning years, points show all nominations. Featuring Alan Richman (35 nominations, 17 journalism wins), Steve Dolinsky (27 nominations, 13 broadcast wins), Commander's Palace (6 nominations over 30 years, Service & Restaurant wins), Ruth Reichl (12 nominations, Lifetime Achievement & journalism award), and John B. Shields (5 nominations spanning 1991-2024, from Chesapeake Bay Cookbook to Smyth's Outstanding Chef nomination).", 148),
    caption = "Source: James Beard Foundation · Graphic: Georgios Karamanis",
    x = "Years since first nomination",
    y = "Cumulative nominations"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 18, face = "bold", family = f1)
  ) 
