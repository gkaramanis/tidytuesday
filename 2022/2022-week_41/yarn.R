library(tidyverse)
library(truchet)
library(MetBrewer)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)

yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

knit <- function(x, max_yard_r, ymax) {
  expand.grid(x = (2 * x - 1):(2 * x) + 0.7 * x - 1, y = max_yard_r - 1:ymax) %>% 
    mutate(scale_p = 0.5, tiles = "dl") %>%
    st_truchet_ms() %>% 
    st_truchet_dissolve()
}

top_yarn <- yarn %>% 
  filter(!discontinued) %>% 
  slice_max(order_by = yardage, n = 10) %>% 
  mutate(
    yard_r = round(yardage / 1e3),
    m_r = round(yardage * 0.9144),
    max_yard_r = max(yard_r)
    ) %>% 
  select(name, yardage, yard_r, max_yard_r, m_r) %>% 
  mutate(i = row_number()) %>% 
  rowwise() %>% 
  mutate(
    truchet = list(knit(i, max_yard_r, yard_r))
  )

f1 <- "Outfit"
f2 <- "Style Script"

top_yarn %>% 
  unnest(truchet) %>% 
  ggplot() +
  annotate("segment", x = -4, xend = 28, y = 32.2, yend = 32.3, size = 4, lineend = "round", color = "grey20") +
  annotate("point", x = -4, y = 32.2, size = 10, color = "grey20") +
  geom_sf(aes(group = name, geometry = geometry, fill = name, color = after_scale(colorspace::darken(fill, 0.2)))) +
  geom_richtext(aes(x = 2 * i + 0.7 * i - 1.5,
                    y = max_yard_r - yard_r - 1.1,
                    label = paste0(str_replace(str_wrap(paste0("**", name, "**"), 15), "\n", "<br>"), "<br>", scales::number(m_r), " m")),
                stat = "unique", family = f1, size = 2.3, vjust = 1, fill = NA, label.color = NA) +
  scale_fill_manual(values = met.brewer("Cross", 10, direction = -1)) +
  labs(
    title = "How much yarn in a bundle?",
    subtitle = "The top 10 longest yarns on ravelry.com",
    caption = "Source: ravelry.com · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, size = 32, family = f2),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.9)
  )
  
