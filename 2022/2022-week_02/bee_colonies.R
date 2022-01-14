library(tidyverse)
library(spatstat)
library(ggtext)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 12, height = 12, units = "in", dpi = 320)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

plot_f <- function(st = "United States") {
  state_title = ifelse(st == "United States", "U.S.", st) # Used for state name in title
  
  colony_st <- colony %>% 
  filter(state == st) %>% 
  select(year, months, colony_n, colony_lost_pct) %>%
  mutate(
    months = fct_inorder(months),
    # Calculate number of total colonies as percentage of the max value (the biggest bar's height is always 100 
    n_pct_of_max = round(colony_n / max(colony_n, na.rm = TRUE) * 100, 1),
    # Calculate lost colonies as percentage of the bar for each period
    lost_pct = ifelse(!is.na(colony_lost_pct), round(n_pct_of_max * colony_lost_pct / 100, 2), 0)
  )

s = 100

# This defines the "shape" of the bar
bees <- data.frame(
  x = c(0, s/3, s/3, 0),
  y = c(0, 0, s, s)
)

# Fill "prototype" bar with hexagons and number the hexagons as percentage values to fill later
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

# Make a bar for every value in the colonies dataset
colony_hex <- colony_st %>% 
  rowwise() %>% 
  mutate(hex = list(hex)) %>% 
  ungroup() %>% 
  unnest(hex) %>% 
  mutate(
    color = case_when(
      # Fill total colonies hexagons (without the lost colonies) with orange
      n_pct_of_max - lost_pct > pct ~ "darkorange",
      # Fill lost colonies hexagons with purple
      n_pct_of_max > pct ~ "#9235F1"
    ),
    # Number labels
    label1 = case_when(
      is.na(colony_n) ~ "<span style = 'color:#888888;'>no data",
      year == 2015 & str_starts(months, "Jan") ~ paste0(round(colony_n/10^3, 1), " K total"),
      TRUE ~ paste0(round(colony_n/10^3, 1), " K")
      ),
    label2 = case_when(
      is.na(colony_n) ~ " ",
      lost_pct == 0 ~ "missing data",
      year == 2015 & str_starts(months, "Jan") ~ paste0(colony_lost_pct, "% lost"),
      TRUE ~ paste0(colony_lost_pct, "%")
    )
  )

f1 = "General Sans"

ggplot(colony_hex) +
  geom_polygon(aes(x, y, group = i, fill = color), color = "grey97", size = 0.25) +
  geom_richtext(aes(s/6, -10, label = paste0("**", label1, "**<br>", label2)), stat = "unique", family = f1, color = "grey10", size = 5, vjust = 1, fill = NA, label.color = NA, label.padding = unit(rep(0, 4), "pt")) +
  scale_fill_identity() +
  coord_fixed(clip = "off", xlim = c(-10, 45), ylim = c(-20, 105)) +
  facet_grid(cols = vars(year), rows = vars(months)) +
  labs(
    title = paste0(state_title, " honey bee colony <span style = 'color:#9235F1;'>losses</span>"),
    subtitle = "Lost honey bee colonies as percentage of the total number of colonies\nat the start of each 3-month period",
    caption = "Source: USDA · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 18) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_markdown(margin = margin(0, 0, 7, 0), family = f1, face = "bold", size = 30),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0), family = f1, size = 16),
    plot.caption = element_text(margin = margin(40, 0, 0, 0), size = 11, color = "grey40", hjust = 0),
    strip.text = element_text(color = "darkorange2", face = "bold"),
    strip.text.y = element_text(hjust = 0, size = 13)
  )

ggsave(here::here("2022", "2022-week_02", "plots", paste0("bee_colonies-", st, ".png")), width = 12, height = 12, dpi = 320)
}

# Plot U.S.
# plot_f() # United States

# Plot individual states and US
# all_states <- unique(colony$state)
# lapply(all_states, plot_f)

plot_f("Vermont")
