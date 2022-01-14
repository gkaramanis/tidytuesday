library(tidyverse)
library(spatstat)
library(ggtext)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 12, height = 6, units = "in", dpi = 320)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

colony_aggr <- colony %>% 
  filter(state == "United States") %>% 
  mutate(
    colony_n = case_when(
      year == 2019 & months == "April-June" ~ 2665880,
      TRUE ~ colony_n
    ),
    colony_lost = case_when(
      year == 2019 & months == "April-June" ~ 355270,
      TRUE ~ colony_lost
    )
  ) %>% 
  group_by(year) %>% 
  summarise(n = sum(colony_n, na.rm = TRUE), n_lost = sum(colony_lost, na.rm = TRUE),  months) %>% 
  ungroup() %>% 
  distinct(year, n, n_lost) %>% 
  arrange(year) %>% 
  mutate(
    n_pct_of_max = round(n / max(n * 1.05, na.rm = TRUE) * 100, 1),
    lost_pct = round(n_lost / max(n * 1.05, na.rm = TRUE) * 100, 1)
    )

s = 100

bees <- data.frame(
  x = c(0, s/3, s/3, 0),
  y = c(0, 0, s, s)
)

hex <- data.frame(hextess(owin(poly = bees), 2, trim = FALSE)) %>% 
  group_by(Tile) %>% 
  mutate(
    xm = mean(x),
    ym = mean(y)
  ) %>% 
  ungroup() %>% 
  arrange(ym, xm) %>% 
  group_by(ym, xm) %>% 
  mutate(i = cur_group_id()) %>% 
  ungroup() %>% 
  select(x, y, Tile, i) %>% 
  mutate(pct = round(i / max(i) * 100, 1))

colony_hex <- colony_aggr %>% 
  rowwise() %>% 
  mutate(hex = list(hex)) %>% 
  ungroup() %>% 
  unnest(hex) %>% 
  mutate(
    color = case_when(
      n_pct_of_max - lost_pct > pct ~ "darkorange",
      n_pct_of_max > pct ~ "#9235F1"
    ),
      label1 = ifelse(year == 2015, paste0(round(n/10^6, 1), " million total"), paste0(round(n/10^6, 1), " m")),
      label2 = ifelse(year == 2015, paste0(lost_pct, "% lost"), paste0(lost_pct, "%"))
    )

f1 = "General Sans"

ggplot(colony_hex) +
  geom_polygon(aes(x, y, group = i, fill = color), color = "grey97", size = 0.25) +
  geom_richtext(aes(s/6, -15, label = paste0("**", label1, "**<br>", label2)), stat = "unique", family = f1, color = "grey10", size = 5, vjust = 1, fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_identity() +
  coord_fixed(clip = "off", xlim = c(-5, 40)) +
  facet_wrap(vars(year), nrow = 1) +
  labs(
    title = "U.S. honey bee colony <span style = 'color:#9235F1;'>losses</span>",
    caption = "Source: USDA · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 18) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_markdown(margin = margin(0, 0, 20, 0), family = f1, face = "bold"),
    plot.caption = element_text(margin = margin(40, 0, 0, 0), size = 11, color = "grey40"),
    strip.text = element_text(color = "darkorange2")
  )

