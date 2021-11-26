library(tidyverse)
library(camcorder)
library(afrilearndata)

gg_record(dir = "temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

afripop_df <- afripop2020 %>% 
  as.data.frame(xy = TRUE) %>% 
  rename(pop = 3) %>% 
  filter(!is.na(pop)) %>% 
  mutate(
    pop2 = case_when(
      pop <= 500 ~ pop,
      TRUE ~ 600
      )
    )

f1 = "Porpora"
f2 = "Publico Headline"

pal <- c(viridis::turbo(10), "yellow")

ggplot(afripop_df) +
  geom_tile(aes(x, y, fill = pop2)) +
  annotate("text", -6, -5, label = "Africa", family = f2, size = 20, fontface = "bold", color = "grey97") +
  scale_fill_gradientn(colors = pal, breaks = seq(0, 600, 100), labels = c(seq(0, 500, 100), ">500")) +
  guides(fill = guide_colorbar(title = "Population density\n(people/km²)", label.position = "left", title.hjust = 0.5)) +
  labs(caption = "Data: afrilearndata · Graphic: Georgios Karamanis") +
  coord_fixed() +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = c(0.22, 0.27),
    legend.key.width = unit(0.5, "line"),
    legend.title = element_text(margin = margin(0, 0, 10, -20), color = "grey85", lineheight = 1.1),
    legend.text = element_text(color = "grey85"),
    plot.background = element_rect(fill = "#58507E", color = NA),
    plot.caption = element_text(hjust = 1, color = "grey80"),
    plot.margin = margin(10, 10, 10, 10)
  )

