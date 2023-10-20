library(tidyverse)
library(ggbeeswarm)
library(ggimage)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')

tas <- taylor_album_songs %>% 
  mutate(
    album_name = fct_reorder(album_name, album_release),
    release_year = year(album_release),
    nm = tolower(str_remove(album_name, " \\(.+\\)")),
    img = paste0(here::here("2023/2023-week_42/covers/"), nm, ".png")
  ) 

p <- ggplot(tas) +
  geom_beeswarm(aes(x = danceability, y = "y", color = album_name), size = 5, method = "hex", cex = 3) +
  scale_color_viridis_d(option = "cividis", direction = -1) +
  theme_gray() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  
pp <- ggplot_build(p)

pp_df <- data.frame(x = pp$data[[1]]$x, 
                    y = pp$data[[1]]$y,
                    r = 0.03,
                    img = tas$img,
                    track_name = tas$track_name
) %>% 
  arrange(x) %>% 
  mutate(id = row_number())

f1 <- "Outfit"
f2 <- "Humane"

ggplot(pp_df, aes(x, y, image = img, label = paste0(" ", track_name, " "))) +
  # Gridlines
  annotate("segment", x = seq(0.3, 0.9, 0.1), xend = seq(0.3, 0.9, 0.1), y = 1.25, yend = 0.78, color = "#664265", linetype = "dotted", linewidth = 0.7) +
  # Title
  annotate("label", x = 0.283, y = 0.78, label = "Taylor Swift songs\ndanceability ⇢", color = "white", family = f2, fontface = "bold", size = 20, hjust = 0.07, fill = "#7c6683", label.size = 0, label.padding = unit(0.5, "lines"), lineheight = 0.69) +
  # Axis text
  annotate("text", x = seq(0.3, 0.9, 0.1), y = 0.7, label = seq(0.3, 0.9, 0.1), color = "#CED4E4", family = f2, size = 14) +
  # Song titles
  ggrepel::geom_label_repel(data = . %>% filter(x > 0.85 | x < 0.31), family = f1, direction = "y", nudge_y = 0.15, label.size = 0, fill = "#AB9FB9", segment.color = "#664265", color = "#3C1E3B", segment.size = 0.4, label.padding = 0.25, label.r = 0.5) +
  # Album covers
  geom_image(size = 0.035) +
  scale_x_continuous(limits = c(0.27, 0.92)) +
  coord_cartesian(clip = "off") +
  labs(
    # title = " Taylor Swift songs",
    # subtitle = "Subtitle",
    caption = "Source: taylor R package · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#7c6683", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 60, family = f2, margin = margin(10, 0, -20, 0), color = "white"),
    plot.caption = element_text(color = "#3C1E3B", size = 10, margin = margin(20, 0, 0, 0))
  )
