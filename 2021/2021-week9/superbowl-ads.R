library(tidyverse)
library(ggimage)
library(cowplot)
library(wesanderson)

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv') %>% 
  rename("product < 10\"" = "show_product_quickly", "sex" = "use_sex") %>% 
  mutate(brand = recode(brand, "Hynudai" = "Hyundai"))


# by year -----------------------------------------------------------------
year_attr <- youtube %>% 
  add_count(year, name = "year_total") %>% 
  pivot_longer(cols = funny:sex, names_to = "attribute") %>% 
  filter(value) %>% 
  group_by(year, attribute) %>% 
  add_count(value) %>% 
  ungroup() %>% 
  distinct(year, attribute, n, year_total) %>% 
  mutate(pct = n / year_total)

# by brand ----------------------------------------------------------------
brands_attr <- youtube %>% 
  add_count(brand, name = "brand_total") %>% 
  pivot_longer(cols = funny:sex, names_to = "attribute") %>% 
  filter(value) %>% 
  group_by(brand, attribute) %>% 
  add_count(value) %>% 
  ungroup() %>% 
  distinct(brand, attribute, n, brand_total) %>% 
  mutate(pct = n / brand_total) %>% 
  mutate(
    color = case_when(
      pct > 0.74 ~ "red",
      pct > 0.49 ~ "blue",
      TRUE ~ "grey20"
    ),
    image = paste0(here::here("2021", "2021-week9", "img", "brands"),"/", brand, ".png")
  ) %>% 
  group_by(brand) %>% 
  mutate(
    attr_n = as.numeric(fct_reorder(attribute, pct)),
    attr_n = (7 - max(attr_n)) + attr_n
  ) %>% 
  ungroup() 

## Plots
# colors and fonts
pal1 <- colorspace::darken(rev(RColorBrewer::brewer.pal(8, "Dark2")))
pal2 <- wes_palette("Zissou1", 10, type = "continuous")

bg_col = "grey97"
title_col = "#113365"
g1_col = "grey20"
g2_col = "grey35"
g3_col = "grey45"

f1 = "October Condensed Devanagari"
f2b = "Publico Headline Bold"

# plot 1, by brand -----------------------------------------------------------
p1 <- ggplot(brands_attr) +
  geom_hline(yintercept = 0.5, size = 0.3, color = g2_col, alpha = 0.4) +
  geom_image(aes(y = 0.6, x = 2.5, image = image), size = 0.6) +
  geom_col(aes(y = pct, x = attr_n, fill = attribute, alpha = if_else(pct > 0.49, 1, 1)), width = 0.8) +
  geom_text(aes(y = 0, x = attr_n, label = if_else(pct > 0.49, attribute, NULL)), hjust = 0, vjust = 0.5, nudge_y = 0.01, color = bg_col, family = f1) +
  scale_y_continuous(breaks = c(0.5), labels = scales::percent) +
  scale_alpha_identity() +
  scale_fill_manual(values = pal1) +
  coord_flip() +
  facet_wrap(vars(brand), nrow = 2) +
  labs(
    title = "Super Bowl Commercials*",
    subtitle = "Attribute appearence as percentage of total ads aired by brand\nHighlighted are the features that appear in more than 50% of a brand's ads"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.height = unit(0.5, "line"),
    legend.margin = margin(10, 0, 0, 0),
    strip.text = element_blank(),
    axis.text.x = element_text(margin = margin(8, 0, 0, 0), color = g3_col),
    plot.title = element_text(size = 38, hjust = 0.5, margin = margin(20, 0, 20, 0), family = f2b, color = title_col),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(0, 0, 13, 0), size = 12),
    plot.margin = margin(0, 20, 20, 20)
  )

# plot 2, by year ------------------------------------------------------------
p2 <- ggplot(year_attr) +
  geom_tile(aes(x = year, y = 0, group = attribute, fill = pct), color = NA) +
  geom_text(aes(x = 1999.2, y = 0, label = attribute), stat = "unique", color = g3_col, hjust = 1, vjust = 0.4, family = f1) +
  geom_text(aes(x = 2020.8, y = 0, label = attribute), stat = "unique", color = g3_col, hjust = 0, vjust = 0.4, family = f1) +
  scale_fill_gradientn(colors = pal2, labels = scales::percent) +
  facet_wrap(vars(attribute), ncol = 1) +
  coord_cartesian(clip = "off") +
  xlim(1997, 2023) +
  labs(
    subtitle = "Attribute appeareance as percentage of total ads aired by year",
    caption = "* 233 ads from the 10 brands that aired the most spots · Source: FiveThirtyEight & superbowl-ads.com · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(color = g3_col),
    legend.key.height = unit(0.5, "line"),
    legend.margin = margin(10, 0, 0, 0),
    axis.text.x = element_text(margin = margin(8, 0, 0, 0), color = g3_col),
    strip.text = element_blank(),
    panel.spacing.y = unit(0.35, "line"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(0, 0, 13, 0), size = 12),
    plot.caption = element_text(hjust = 0.5, margin = margin(30, 0, 0, 0), color = g2_col),
    plot.margin = margin(20, 0, 10, 0)
  )

# cowplot ---------------------------------------------------------------
plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 0.6)) 

ggsave(here::here("temp", paste0("superbowl-ads-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 9, width = 9)

