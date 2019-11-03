library(tidyverse)
library(here)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

cp_hect<- nyc_squirrels %>% 
  mutate(
    height = above_ground_sighter_measurement,
    height = case_when(
      height == "FALSE" ~ 0,
      !is.na(height) ~ as.numeric(height)/3.281
    )
  )  %>% 
  filter(!is.na(height)) %>% 
  group_by(hectare) %>%
  # max height
  slice(which.max(height)) %>% 
  ungroup %>% 
  mutate(
    NS = substring(hectare, 0, 2),
    EW = substring(hectare, 3)
    ) %>% 
  select(NS, EW, height) %>% 
  spread(EW, height) %>% 
  gather(EW, height, -NS)

ggplot(cp_hect) +
  geom_tile(aes(NS, EW, fill = height)) +
  # gridlines / can't make it right :(
  # annotate("segment", x = 0, xend = 42, y = seq(10.5, 50.5, 10), yend = seq(10.5, 50.5, 10), alpha = 1, size = 0.1) +
  # annotate("text", x = 6, y = seq(11, 61, 10), label = seq(0, 50, 10), hjust = 0,  family = "IBM Plex Mono") +
  geom_tile(aes(x = NS, y = 21 + height/4, height = 21 + height/2, fill = height), alpha = 1) +
  # annotation for max height
  annotate("text", 18, 59, label = "Squirrel observed at the top of\na tree, at an estimated height\nof 54.9m", hjust = 0, vjust = 1, family = "IBM Plex Mono") +
  annotate("segment", x = 17.5, y = 58.6, xend = 15, yend = 58.6, color = "darkgreen", size = 0.5) +
  
  coord_fixed() +
  scale_fill_gradient(low = "#1A512E", high = "#A2D240",
    na.value = "deepskyblue",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title = "Height in meters")) +
  labs(title = "High Squirrels",
    subtitle = "Estimated height where squirrels were observed during the 2018\nNYC Squirrel Census. Only showing max height for every hectare",
    caption = "Source: NYC Squirrel Census | Graphic: Georgios Karamanis") +
  theme_void(base_family = "IBM Plex Sans") +
  theme(
    legend.position = "bottom",
    plot.margin = margin(20, 20, 20, 20),
    # plot.background = element_rect(fill = "lightgreen", color = NA),
    plot.title = element_text(size = 22, family = "IBM Plex Sans Bold", hjust = 0, margin = margin(0, 0, 10, 0)),
    plot.subtitle = element_text(size = 15, hjust = 0, margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 9, margin = margin(20, 0, 0, 0))
    ) +
  ggsave(
    here::here("week-44", "plots", "temp", paste0("nyc-squirrels-height", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 12, width = 7
  )

