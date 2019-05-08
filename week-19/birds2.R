library(tidyverse)
library(colorblindr)

# read data
bird_collisions <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv"
  )

# plot - a lot of borrowing from Thomas Mock ;)
# https://github.com/jthomasmock/tidytuesday_projects/blob/master/2019/2019-02-09/tennis_grandslams.R

birdPlot <- bird_collisions %>%
  group_by(species) %>%
  arrange(date) %>%
  mutate(rn = row_number()) %>%
  mutate(
    colour = case_when(
      species == "albicollis" ~ "#FF2B4F",
      species == "hyemalis" ~ "#003399",
      species == "melodia" ~ "#3686d3",
      species == "georgiana" ~ "#fcab27",
      species == "aurocapilla" ~ "#88398a",
      T ~ "gray60"
    )
  )

ggplot(birdPlot,
       aes(
         date,
         rn,
         group = species,
         color = colour,
         fill = colour,
         alpha = (colour != "gray60")
       )) +
  geom_step(aes(size = colour == "#FF2B4F")) +
  geom_point(
    data = . %>%
      group_by(species) %>%
      top_n(1, rn) %>%
      arrange(desc(rn)) %>%
      head(5),
    shape = 21,
    aes(col = colour),
    size = 1,
    stroke = 0.5
  ) +
  geom_text(
    data = . %>% group_by(species) %>%
      top_n(1, rn) %>%
      arrange(desc(rn)) %>%
      head(5) %>%
      mutate(
        first_initial = str_sub(genus, 1, 1),
        specLabel = paste0("  ", first_initial, ". ", species, ": ", rn)
      ),
    aes(label = specLabel, hjust = 1.1),
    size = 2,
    family = "IBM Plex Mono"
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.4, 1), guide = F) +
  scale_size_manual(values = c(0.2, 0.5), guide = F) +
  scale_x_date(
    limits = c(as.Date('1980-01-01'), as.Date('2017-12-31')) ,
    date_labels = "%Y",
    date_breaks = "5 years",
    expand = c(0, 0)
  ) +
  scale_y_continuous(position = "right", expand = expand_scale(add = c(0, 100))) +
  labs(title = "White-throated sparrow, the super collider of Chicago",
       subtitle = "Cumulative lethal collisions, 1980-2017",
       caption = "\nSource: Winger et al, 2019 | Graphic: Georgios Karamanis / @geokaramanis") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.6), "cm"),
    panel.grid = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.3),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.x = element_line(color = "#212121", size = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x = element_line(size = 0.3, color = "#212121"),
    text = element_text(family = "IBM Plex Sans", size = 6),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(vjust = 2)
  ) +
  ggsave("./birds2.png", width = 4, height = 4)
