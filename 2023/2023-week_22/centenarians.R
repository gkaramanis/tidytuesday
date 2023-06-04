library(tidyverse)
library(ggbeeswarm)
library(packcircles)
library(ggtext)
library(ggrepel)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 7.8, units = "in", dpi = 320)

centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

cent_bee <- centenarians %>% 
  group_by(gender) %>%
  arrange(-age) %>% 
  mutate(
    g_rank = row_number(),
    name = fct_reorder(name, age)
    ) %>% 
  ungroup() 

p <- ggplot(cent_bee) +
  geom_beeswarm(aes(age, "group"), groupOnX = FALSE) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )

pp <- ggplot_build(p)

pp_df <- data.frame(x = pp$data[[1]]$x, 
                    y = pp$data[[1]]$y,
                    r = 0.03,
                    place = cent_bee$place_of_death_or_residence,
                    gender = cent_bee$gender,
                    name = cent_bee$name,
                    age = cent_bee$age,
                    still_alive = cent_bee$still_alive,
                    g_rank = cent_bee$g_rank
                    ) %>% 
  arrange(x) %>% 
  mutate(id = row_number())

pp_repel <- circleRepelLayout(pp_df, wrap = FALSE, sizetype = "area")

pp_repel_out <- circleLayoutVertices(pp_repel$layout)

blue <- RColorBrewer::brewer.pal(name = "Paired", n = 10)[2]
red <- RColorBrewer::brewer.pal(name = "Paired", n = 10)[6]

pp_plot <- pp_repel_out %>% 
  left_join(pp_df, by = "id") %>% 
  mutate(
    x = x.x,
    y = y.x,
    color = case_when(
      place == "United States" ~ blue,
      place == "Japan" ~ red,
      TRUE ~ "grey50"
    ),
    color2 = if_else(still_alive == "alive", color, colorspace::lighten(color, 0.8))
  ) %>% 
  group_by(id) %>% 
  mutate(
    x0 = mean(x),
    y0 = mean(y)
  ) %>% 
  ungroup()

# centenarians %>% 
#   count(gender, place_of_death_or_residence == "Japan" | place_of_death_or_residence == "United States") 

f1 <- "Outfit"
f2 <- "DIN Condensed"

ggplot(pp_plot) +
  annotate("text", x = c(110.3, 113.3), y = c(8, 4.5), label = c("Men", "Women"), family = f2, hjust = 1, size = 6, color = "grey40") +
  geom_label_repel(data = . %>% filter(g_rank <= 3), aes(x0, y0 + 3.5 * as.numeric(factor(gender)), label = paste0(name, " · ", round(age, 1), " years"), fill = color, segment.color = color), direction = "y", nudge_y = 2, family = f2, segment.size = 0.2, label.size = 0.2, stat = "unique", color = "white", point.padding = 2, size = 4.5) +
  geom_polygon(aes(x, y + 3.5 * as.numeric(factor(gender)), group = id, fill = color2)) +
  geom_polygon(aes(x, y + 3.5 * as.numeric(factor(gender)), group = id, color = alpha(color, 0.6)), fill = NA, linewidth = 0.5) +
  scale_x_continuous(breaks = seq(110, 124, 2), limits = c(110, 124)) +
  scale_y_continuous(limits = c(3, 10)) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed() +
  labs(
    title = glue::glue("More than half of the oldest people live or lived in the <span style='color:{blue}'>US</span> or <span style='color:{red}'>Japan</span>"),
    subtitle = glue::glue("61 of the top 100 verified oldest women and 51 of the top 100 verified oldest men have the US or Japan as their place of death or residence.<br>Gray represents people from other countries. Solid circles (<span style='color:{blue}'>●</span><span style='color:{red}'>●</span><span style='color:grey50'>●</span>) show people that are still alive."),
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_markdown(face = "bold", size = 15),
    plot.subtitle = element_markdown(margin = margin(0, 0, 20, 0), size = 13, lineheight = 1.1),
    plot.caption = element_text(margin = margin(20, 0, 0, 0), size = 11, color = "grey40"),
    plot.margin = margin(20, 0, 20, 0)
  )

