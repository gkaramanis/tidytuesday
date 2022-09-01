library(tidyverse)
library(camcorder)
library(scales)
library(patchwork)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 10, units = "in", dpi = 320)

pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv') %>% 
  janitor::clean_names()

pell_state <- pell %>% 
  filter(year %in% c(2007, 2012, 2017)) %>% 
  group_by(year, state) %>% 
  summarise(
    awards_state = sum(award),
    recip_state = sum(recipient),
  ) %>% 
  arrange(-awards_state) %>% 
  mutate(state = fct_reorder(state, awards_state)) %>% 
  mutate(
    x = cumsum(lag(recip_state/2, default = 0) + recip_state/2),
    label = if_else(awards_state > 5e8, state, NULL),
    col = ifelse(awards_state > 1e9, state, "grey50")
  ) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    all_states_awards = sum(awards_state),
    all_states_recip = sum(recip_state),
    totals_label = paste0("In ", year, ", a total of ", dollar(all_states_awards, scale_cut = cut_short_scale()), " was awarded to ", number(all_states_recip, scale_cut = cut_short_scale()), " recipients across all states")
    ) %>% 
  ungroup()

f1 <- "Outfit"
f2 <- "Sora"

pal <- c("#e6194B", "#3cb44b", "#000075", "#F1D411", "#9A6324", "#4363d8", "#f58231", "#469990" , "grey70")

p <- ggplot(pell_state) +
  # Bars
  geom_tile(aes(x = x, y = awards_state/2, width = recip_state - 5e3, height = awards_state, fill = col)) +
  # State
  geom_text(aes(x = x, y = awards_state, label = label, size = awards_state), nudge_y = 3e8, family = f1, color = "grey10") +
  # Recipients
  geom_text(data = pell_state %>% filter(col != "grey50"), aes(x = x, y = awards_state/2, label = paste0(round(recip_state/1e3), "K"), size = awards_state/2), color = "white", family = f1) +
  # Totals
  geom_text(aes(x = all_states_recip, y = 0, label = totals_label), hjust = 1, vjust = 1, stat = "unique", nudge_y = -1e8, family = f2, size = 3, color = "maroon") +
  # Scales, etc
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  facet_wrap(vars(year), ncol = 1, strip.position = "left") +
  coord_cartesian(expand = TRUE, clip = "off") +
  labs(caption = "Source: U.S. Department of Education · Graphic: Georgios Karamanis") +
  theme_minimal(base_family = f2, base_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "maroon"),
    strip.placement = "outside",
    strip.text = element_text(margin = margin(0, 10, 0, 0), face = "bold", size = 14, color = "grey50"),
    panel.spacing = unit(2, "lines"),
    plot.caption = element_text(size = 8, color = "grey30")
  )

annot <- tribble(
  ~x, ~y, ~label, ~size, ~face, ~color,
  0,  1, "Pell Grants by state", 10, "bold", "maroon",
  0,  1.8, "The height of the bars shows the total amount awarded per state, while the width shows the total number of recipients. Highlighted with color are the states that received more than $1B and with labels those that received more than $500M", 4, "plain", "black"
) %>% 
  ggplot() +
  geom_text(aes(x, y, label = str_wrap(label, 50), fontface = face, size = size, color = color), hjust = 1, vjust = 1, family = f2) +
  scale_size_identity() +
  scale_color_identity() +
  scale_x_continuous(limits = c(-1, 0)) +
  scale_y_reverse(limits = c(5, 1)) +
  theme_void()

p + 
  inset_element(annot, 0.4, 0.7, 1, 1)

