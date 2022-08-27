library(tidyverse)
library(ggtext)
library(ggrepel)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)


chips <- readr::read_csv(here::here("2022/2022-week_34/data/chip_dataset.csv")) %>% 
  janitor::clean_names()

chips_console <- chips %>% 
  mutate(
    console = case_when(
      str_detect(product, "Playstation") ~ "Playstation",
      str_detect(product, "Xbox") ~ "Xbox"
    ),
    model = case_when(
      str_detect(product, "Playstation 2") ~ "Playstation 2",
      str_detect(product, "Playstation 3") ~ "Playstation 3",
      str_detect(product, "Playstation 4") ~ "Playstation 4",
      str_detect(product, "Playstation 5") ~ "Playstation 5",
      str_detect(product, "Portable") ~ "Portable",
      str_detect(product, "Vita") ~ "Portable",
      str_detect(product, "Xbox 360") ~ "Xbox 360",
      str_detect(product, "Xbox One") ~ "Xbox One",
      str_detect(product, "Xbox Series X") ~ "Xbox Series X",
      str_detect(product, "Xbox Series S") ~ "Xbox Series S"
    )
    ) %>% 
  filter(release_date != "NaT") %>% 
  filter(!is.na(console)) %>% 
  mutate(
    label = str_remove(product, "^\\w+"),
    label = str_remove(label, " GPU.*"),
    label = paste0(label, "\n", process_size_nm, "nm"),
    label = str_replace(label, "Playstation", "PlayStation"),
    release_date = as.Date(release_date),
    col = if_else(console == "Xbox", "#367C21", "#1D3E87")
    )

f1 <- "Outfit"
f2 <- "Sora"

ggplot(chips_console) +
  geom_point(aes(x = release_date, y = tdp_w/die_size_mm_2, color = col, size = process_size_nm), shape = 15) +
  geom_line(aes(x = release_date, y = tdp_w/die_size_mm_2, group = interaction(console, model), color = col), size = 0.25) +
  geom_text_repel(aes(x = release_date, y = tdp_w/die_size_mm_2, label = label, color = colorspace::lighten(col, 0.2)), size = 3.5, family = f1, bg.color = "white", seed = 11, lineheight = 0.9) +
  geom_richtext(data = NULL, aes(x = as.Date("2021-01-01"), y = 1.15, label = "**<span style='color:#1D3E87;'>PlayStation</span>** and **<span style='color:#367C21;'>Xbox</span>**<br>GPU efficiency<br><span style = 'font-size:22pt'>in watts per mm²</span><br><span style = 'font-size:8pt; color:gray40'>Source: The CHIP Dataset · Graphic: Georgios Karamanis</span>"), family = f2, size = 9, vjust = 1, hjust = 1, fill = NA, label.color = NA) +
  scale_size_area(max_size = 8) +
  scale_x_date(date_labels = "%Y", minor_breaks = "1 year") +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) 
