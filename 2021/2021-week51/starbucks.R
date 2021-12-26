library(tidyverse)
library(camcorder)
library(MetBrewer)

gg_record(dir = "temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

caff <- starbucks %>% 
  distinct(product_name, size, serv_size_m_l, sugar_g, caffeine_mg) %>% 
  group_by(product_name) %>% 
  mutate(max_caff = max(caffeine_mg)) %>% 
  ungroup() %>% 
  filter(max_caff > 100) %>%
  arrange(max_caff) %>% 
  mutate(
    product_name = fct_reorder(str_to_title(product_name), max_caff),
    serv_size_m_l = case_when(
      size == "solo" ~ 22,
      size == "doppio" ~ 44,
      size == "triple" ~ 65,
      size == "quad" ~ 89,
      TRUE ~ serv_size_m_l
    )
    )

f1 = "Porpora"
f2 = "Montserrat"

pal <- rev(met.brewer("OKeeffe2", type = "continuous"))
bg_col <- colorspace::lighten(pal[7], 0.8)
                              
ggplot(caff) +
  geom_vline(xintercept = 400, color = "coral", linetype = "dashed") +
  geom_point(aes(x = caffeine_mg, y = product_name, size = serv_size_m_l, fill = sugar_g), shape = 21, color = bg_col, stroke = 0.25) +
  annotate("text", x = 415, y = "Espresso Con Panna", label = "The FDA recommends\na maximum daily\nintake of 400 mg\ncaffeine", hjust = 0, family = f1, color = "coral", lineheight = 0.95, vjust = 0.8) +
  annotate("text", x = 185, y = "Caramel Frappuccino Blended", label = "How much caffeine?", size = 10, family = f2, hjust = 0, fontface = "bold") +
  annotate("text", x = 185, y = "Caffè Vanilla Frappuccino Blended", label = "Caffeine, sugar and serving size of\nStarbucks Drinks (showing drinks that\ncontain at least 100 mg caffeine)", size = 5, family = f2, hjust = 0, vjust = 1) +
  annotate("text", x = 185, y = "Iced Green Tea Latte", label = "Source: Starbucks - Graphic: Georgios Karamanis", size = 3.5, family = f1, hjust = 0, vjust = 1) +
  scale_size_continuous(range = c(0.5, 5.5), name = "Serving Size (ml)", guide = guide_legend(override.aes = list(fill = pal[3]))) +
  scale_fill_stepsn(colors = pal, name = "Sugar (g)") +
  scale_x_continuous(sec.axis = dup_axis(), limits = c(0, 500)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.91, 0.47),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(20, 30, 20, 20),
    axis.title = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0.5, "lines"),
    axis.text.x = element_text(margin = margin(5, 0, 0, 0))
  )
