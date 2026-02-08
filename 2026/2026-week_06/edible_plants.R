library(tidyverse)
library(bdftools)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 13, height = 8, units = "in", dpi = 320)

edible_plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv')

edible_temps <- edible_plants |> 
  select(common_name, cultivation, temperature_germination, temperature_growing) |>
  summarize(
    common_name, cultivation,
    temp_germ_low = str_extract(temperature_germination, "^[^-]+"),
    temp_germ_high = str_extract(temperature_germination, "[^-]+$"),
    temp_grow_low = str_extract(temperature_growing, "^[^-]+"),
    temp_grow_high = str_extract(temperature_growing, "[^-]+$")
  ) |>
  mutate(
    across(starts_with("temp_"), as.numeric)
  ) |> 
  filter(!if_any(starts_with("temp_"), is.na))

fontfile <- system.file("spleen-5x8.bdf", package = "bdftools", mustWork = TRUE)
myfont <- read_bdf(fontfile)

edible_bdf <- edible_temps |> 
  rowwise() |> 
  mutate(common_bdf = list(bdf_create_df(myfont, common_name))) |> 
  ungroup()

set.seed(99) # change for different sample

edible_sample <- edible_bdf |> 
  filter(temp_grow_high - temp_grow_low > 2) |> 
  slice_sample(n = 2, by = cultivation)

# pal <- MetBrewer::met.brewer("Johnson", direction = -1)
pal <- rcartocolor::carto_pal(name = "Geyser")
f1 <- "DIN Condensed"

col_bg <- "#303D2F"

edible_plot <- edible_sample |> 
  unnest(common_bdf) |>
  # remap x for each plant to start at lower growing temp and end to higher growing temp
  group_by(common_name) |>
  mutate(
    x = scales::rescale(x,
      to = c(unique(temp_grow_low), unique(temp_grow_high)),
      from = range(x)
    ),
    x_min = min(x),
    x_max = max(x),
    x_mid = (x_min + x_max) / 2
  ) |>
  ungroup() |> 
  mutate(common_name = fct_reorder(common_name, x_mid))
  
ggplot(edible_plot, aes(x, y, fill = x)) +
  geom_tile(color = col_bg) +
  geom_text(aes(x = x_min - 1, y = 3, label = x_min, color = x_min), hjust = 1, stat = "unique", size = 5) +
  geom_text(aes(x = x_max + 1, y = 3, label = x_max, color = x_max), hjust = 0, stat = "unique", size = 5) +
  geom_text(aes(x = x_mid, y = -1.2, label = cultivation), stat = "unique", size = 4, color = "#A8B5A0") +
  scale_fill_gradientn(colours = pal, rescaler = ~ scales::rescale_mid(.x, mid = 15)) +
  scale_color_gradientn(colours = pal, rescaler = ~ scales::rescale_mid(.x, mid = 15)) +
  scale_x_continuous(expand = c(0.06, 0.0)) +
  scale_y_continuous(expand = c(0.12, 0.0)) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(common_name), ncol = 3, dir = "v") +
  labs(
    title = "Growing temperatures of edible plants",
    subtitle = str_wrap("Sample of edible plants, ordered by the mid point of their growing temperature range. Range shown for each plant is between the minimum and maximum growing temperatures in degrees Celsius.", 100),
    caption = "Source: The Edible Plant Database (EPD) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FEF9F0", color = NA),
    strip.text = element_blank(),
    plot.title = element_text(size = 24, face = "bold", color = col_bg, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 16, color = col_bg, margin = margin(b = 10)),
    plot.caption = element_text(size = 11, color = col_bg, margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = col_bg, color = NA)
  )
  
record_polaroid()
