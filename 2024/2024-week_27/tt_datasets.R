library(tidyverse)
library(ggbump)
library(ggtext)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

tt_urls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-02/tt_urls.csv')

tt_dom_ranked <- tt_urls %>% 
  filter(type == "source") %>% 
  count(year, domain, sort = TRUE) %>% 
  group_by(year) %>% 
  arrange(-n) %>% 
  mutate(
    year_total = sum(n), 
    rank = row_number(),
    label = paste0(domain, " (", n, ")")
    ) %>% 
  ungroup()

tt_dom_year <- tt_dom_ranked %>% 
  distinct(year, year_total) %>% 
  mutate(label = case_when(
    year == min(year) ~ paste0("**", year, "**<br>", year_total, " sources"),
    TRUE ~ paste0("**", year, "**<br>", year_total)
  ))

highl <- c("github",
           "wikipedia",
           "kaggle",
           "ourworldindata",
           "census",
           "bbc",
           "data",
           "nasa",
           "imdb"
           )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- MetBrewer::met.brewer("Redon", n = 9)

p <- ggplot(tt_dom_ranked, aes(x = year, y = rank, group = domain, size = if_else(domain %in% highl, 1.2, 0.15), color = if_else(domain %in% highl, domain, NA))) +
  geom_bump() +
  geom_point(shape = 21, fill = "white", stroke = 1.2) +
  shadowtext::geom_shadowtext(data = . %>% filter(n > 1), aes(label = label), size = 3.8, bg.color = "white", bg.r = 0.2, family = f1b, fontface = "bold") +
  geom_richtext(data = tt_dom_year, aes(x = year, y = 41, label = label, group = 1L), size = 4, family = f1b, lineheight = 1.1, color = "black", label.color = NA, fill = NA) +
  scale_y_reverse(breaks = c(1, 10, 20, 30)) +
  scale_x_continuous(breaks = 2018:2024) +
  scale_size_identity() +
  # scale_color_brewer(palette = "Dark2", na.value = "grey70") +
  scale_color_manual(values = pal, na.value = "grey80") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Top domains of TidyTuesday data sources",
    subtitle = str_wrap("Domains of the URLs of data sources by year, showing the names of domains used at least twice. Selected domains are highlighted with color. GitHub is consistently the top domain every year.", 130),
    caption = "Source: TidyTuesday · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey60"),
    plot.title = element_text(family = f2, face = "bold"),
    plot.margin = margin(10, 30, 10, 10)
  )
  
tt_dom_total_top <- tt_urls %>% 
  filter(type == "source") %>%
  count(domain, sort = TRUE) %>% 
  mutate(domain = fct_reorder(domain, n)) %>% 
  slice_max(order_by = n, n = 5) %>% 
  mutate(left = if_else(n > 20, TRUE, FALSE))

t <- ggplot(tt_dom_total_top, aes(x = n, y = domain, label = paste0(domain, " (", n, ")"))) +
  geom_col(width = 0.8, fill = "#a7a6ba") +
  geom_text(data = . %>% filter(left), family = f1b, color = "white", hjust = 1, size = 3.4, fontface = "plain", nudge_x = -1, lineheight = 0.9) +
  geom_text(data = . %>% filter(!left), family = f1b, color = "black", hjust = 0, size = 3.4, fontface = "plain", nudge_x = 1, lineheight = 0.9) +
  labs(title = "Total top 5 sources") +
  theme_void(base_family = f1) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.9)
  )
  
p +
  inset_element(t, left = 0.78, bottom = 0.12, right = 1.02, top = 0.32)

