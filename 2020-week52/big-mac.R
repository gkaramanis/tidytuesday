library(tidyverse)
library(lubridate)
library(scales)
library(cowplot)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

mapping <- read_csv(here::here("2020-week52", "data", "mapglyphs-mapping.csv"))

big_dotplot <- big_mac %>% 
  filter(date == min(date) | date == max(date)) %>% 
  mutate(year = year(date))

# https://stackoverflow.com/questions/48545286/geom-dotplot-with-text-instead-of-dots
big_dotplot_h <-hist(big_dotplot$usd_raw, plot = FALSE)
big_dotplot$x_bin <- cut(big_dotplot$usd_raw, breaks = big_dotplot_h$breaks, label = FALSE)
big_dotplot$x_mid <- big_dotplot_h$mids[big_dotplot$x_bin]
big_dotplot$y <- 1

big_dotplot_bin <- big_dotplot %>% 
  group_by(year, x_bin) %>%
  mutate(
    y = cumsum(y),
    y = y - median(y)
    ) %>% 
  ungroup() %>% 
  mutate(
    country = case_when(
      name == "Euro area" ~ "Europe",
      name == "Britain" ~ "United Kingdom",
      TRUE ~ name
    ),
    color = if_else(x_mid > 0, "#1D3D67", "#9E4361")
    ) %>% 
  left_join(mapping) 

col_bg <- "#EBE2DD"
col3 <- "#B9B2B5"
col4 <- "#E7C9BA"
f1 <- "IBM Plex Sans Text"
f2 <- "Canela Text"

p <- ggplot(big_dotplot_bin) +
  geom_text(aes(x = x_mid, y = y, label = icon, color = color), family = "MapGlyphs", size = 6) +
  geom_text(aes(x = x_mid, y = y, label = name), size = 2, vjust = 1, nudge_y = -0.04, family = f1) +
  geom_vline(xintercept = 0, color = col4) +
  facet_wrap(vars(year), ncol = 1, strip.position = "left") +
  scale_color_identity() +
  scale_x_continuous(labels = label_percent()) +
  labs(
    title = "The Big Mac index",
    caption = "Source: The Economist | Graphic: Georgios Karamanis\nMap Icons by MapGlyphs.com",
    x = "Raw index, relative to the US dollar"
    ) +
  theme_minimal(base_family = f2) +
  theme(
    plot.background = element_rect(fill = col_bg, color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(size = 26),
    plot.caption = element_text(margin = margin(5, 0, -5, 0)),
    strip.text = element_text(size = 18),
    panel.spacing = unit(-3, "lines"),
    axis.line.x = element_line(color = col3),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
    )

ggdraw(p) +
  draw_label(label = "The Big Mac index is an informal way of measuring the purchasing power\nparity (PPP) between two currencies. The graph shows the exchange rate (binned)\nbetween each country's currency and the US dollar, in 2000 and 2020.", x = 0.42, y = 0.925, hjust = 0, vjust = 0, fontfamily = f2, size = 10, lineheight = 1) 

ggsave(here::here("temp", paste0("big-mac-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 10, height = 10)

