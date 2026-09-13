library(tidyverse)
library(countrycode)
library(wbstats)
library(ggcirclepack)
library(shadowtext)
library(marquee)
library(rnaturalearth)
library(ggpp)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# cafe <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cafe.csv')
cappuccino_index <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-09-08/cappuccino_index.csv')

fmt_minutes <- function(x) {
  m <- round(x)
  case_when(
    m == 0 ~ "0",
    m < 60 ~ paste0(as.character(m), "m"),
    m %% 60 == 0 ~ paste0(m %/% 60, "h"),
    .default = paste0(m %/% 60, "h ", m %% 60, "m")
  )
}

ci_income <- cappuccino_index |>
  filter(n > 9) |>
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) |>
  left_join(
    wb_countries() |>
      filter(income_level != "Aggregates") |>
      select(iso3c, income_level),
    by = c("iso3" = "iso3c")
  ) |>
  mutate(
    income_level = case_when(
      country %in% c("Taiwan", "Puerto Rico") ~ "High income",
      .default = income_level
    ),
    income = factor(
      case_when(str_detect(income_level, "middle") ~ "Middle income", .default = income_level),
      levels = c("Low income", "Middle income", "High income")
    ),
    country_label = paste0(str_replace(country, "\\s", "\n"), "\n", fmt_minutes(index))
  )

f1 <- "Outfit"
f2 <- "Futura"

pal <- MetBrewer::met.brewer("Johnson", direction = -1)

# Inset maps
world <- ne_countries(scale = "small", returnclass = "sf") |>
  filter(sovereignt != "Antarctica")

world_income <- world |>
  left_join(ci_income |> distinct(iso3, income), by = c("iso_a3" = "iso3"))

make_map_grob <- function(inc) {
  p <- ggplot() +
    geom_sf(data = world, fill = "grey85", color = "white", linewidth = 0.05) +
    geom_sf(data = filter(world_income, income == inc),
            fill = pal[2], color = "white", linewidth = 0.05) +
    coord_sf(crs = "+proj=robin") +
    theme_void() +
    theme(panel.background = element_blank(), plot.background = element_blank())
  ggplotGrob(p)
}

inset_levels <- levels(droplevels(ci_income$income))

map_insets <- tibble(
  income = factor(inset_levels, levels = levels(ci_income$income)),
  npcx = c(0.1, 0.9),
  npcy = Inf,
  grob = map(inset_levels, make_map_grob)
)

ggplot(ci_income, aes(id = country, area = index, fill = index)) +
  geom_circlepack() +
  geom_circlepack_text(geom = "shadowtext", aes(label = country_label, bg.colour = after_scale(fill)), color = "grey97", family = f2, lineheight = 0.9) +
  geom_grob_npc(data = map_insets, aes(npcx = npcx, npcy = npcy, label = grob), vp.width = 0.3, vp.height = 0.15) +
  scale_fill_stepsn(colors = pal, breaks = c(0, 15, 30, 60, 120, 180), labels = fmt_minutes, limits = c(0, NA), name = "Time per cup", guide = guide_colorsteps(even.steps = FALSE)) +
  scale_size_area(max_size = 11) +
  coord_fixed(clip = "off") +
  facet_wrap(vars(income)) +
  labs(
    title = "Who can afford the coffee they make?",
    subtitle = "Baristas everywhere make the same drink, but not all can afford to buy one. James Hoffmann surveyed cafés in 87 countries, collecting cappuccino prices and hourly wages. Each circle shows how many minutes a barista must work to earn a cup, for the 36 countries with at least ten responses. The median is 19 minutes in high-income countries and 73 in middle-income ones. A barista in India works 17 times longer than one in Australia for the same cup.",
    caption = "Source: James Hoffmann · Graphic: Georgios Karamanis"
  ) +
  guides(size = "none") +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.key.width = unit(3.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(family = f2, size = 12, color = "grey10", face = "bold", margin = margin(20, 0, 3, 0)),
    plot.title = element_text(size = 18, family = f1, face = "bold"),
    plot.subtitle = element_marquee(width = 0.98, margin = margin(5, 0, 15, 0), family = f1, size = 13, color = "grey10", lineheight = 1),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )
