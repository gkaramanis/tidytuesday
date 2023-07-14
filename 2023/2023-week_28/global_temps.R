library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')

glob_temp_l <- global_temps %>% 
  pivot_longer(Jan:Dec, names_to = "month") %>% 
  mutate(month = fct_inorder(month))
  
pal <- cetcolor::cet_pal(name = "d1a", n = 7)

annot <- tibble(
  x = "Apr",
  y = seq(1880, 2020, 20)
)

f1 <- "Outfit"
f2 <- "Ruda"

ggplot(glob_temp_l) +
  geom_tile(aes(month, Year, fill = value, color = value)) +
  shadowtext::geom_shadowtext(data = annot, aes(x = x, y = y, label = paste0("·\n", y)), family = f2, bg.color = alpha("white", 1), fontface = "bold", color = "black", size = 3.5, vjust = 0.8, lineheight = 0.9) +
  scale_fill_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  scale_color_gradientn(colors = pal, rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  scale_y_continuous(limits = c(1875, 2023)) +
  coord_polar(start = -pi/12) +
  labs(
    title = "Global Surface Temperature, 1880-2023",
    subtitle = "Shown as deviation from the 1951-1980 means",
    caption = str_wrap("Source: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies · Graphic: Georgios Karamanis"),
    fill = "°C",
    color = "°C"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    legend.key.width = unit(0.7, "lines"),
    legend.key.height = unit(2, "lines"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(family = f2, size = 12, face = "bold"),
    axis.text.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(10, 10, 10, 10)
  )
  
