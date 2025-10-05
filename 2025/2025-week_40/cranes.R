library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8.5, units = "in", dpi = 320)

cranes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv')

cr <- cranes |> 
  arrange(date) |> 
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = FALSE),
    week = isoweek(date),
    day = day(date)
  )

# Chart
f1 <- "Graphik"

p1 <- ggplot(cr |> filter(year >= 2002) |> filter(month %in% c("March", "April"))) +
  geom_col(aes(day, observations, fill = observations)) +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("OKeeffe2", direction = 1), na.value = "white", limits = c(0, 30e3), labels = scales::number) +
  facet_grid(vars(year), vars(month), switch = "y") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text.y.left = element_text(angle = 0),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#E3F2FD", color = NA)
  )
  
p2 <- ggplot(cr |> filter(year >= 2002) |> filter(!month %in% c("March", "April"))) +
  geom_col(aes(day, observations, fill = observations)) +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("OKeeffe2", direction = 1), na.value = "white", limits = c(0, 30e3), labels = scales::number) +
  facet_grid(vars(year), vars(month), switch = "y") +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text.y.left = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#E3F2FD", color = NA)
  )

# Maps
hornborga <- st_read(here::here("2025/2025-week_40/data/hornborgasjön.geojson"))
ssweden <- rnaturalearthdata::countries50 %>%
  dplyr::filter(admin == "Sweden")

# ymin <- 58.208
ymin <- 58.191
ymax <- ymin + 0.17

m1 <- ggplot(hornborga) +
  geom_sf(fill = "#A6CEE3") +
  # marquee::geom_marquee(aes(x = 13.49, y = ymax, label = "**Cranes at Lake Hornborga**  \nA spectacular spring gathering of thousands of migrating cranes in Sweden"), hjust = 0, vjust = 1, family = f1, width = 1, lineheight = 0.9, size = 3.8) +
  marquee::geom_marquee(aes(x = 13.49, y = ymin, label = "Each spring, Lake Hornborga hosts up to 30 000 cranes that stop to rest and feed on their way north. The cranes gather at 'Trandansen' (literally 'the crane dance' in Swedish), the southern tip of the lake, where they are provided with grain to protect local farmland. The peak occurs in late March or early April, with daily counts sometimes exceeding 20 000 birds. Visitors can witness the cranes’ trumpeting calls and ritual dances from dawn to dusk. After their stay, most cranes continue north to breed in Sweden and Norway."), hjust = 0, vjust = 0, family = f1, width = 0.96, size = 3) +
  geom_text(aes(x = 13.555, y = 58.32, label = "Lake\nHornborga"), family = f1, size = 4.5, hjust = 0.5, vjust = 0, color = "white", lineheight = 0.95) +
  geom_point(aes(x = 13.487, y = 58.276), shape = 21, fill = "black", color = "white", size = 2.5, stroke = 0.5) +
  shadowtext::geom_shadowtext(aes(x = 13.488, y = 58.277, label = "Trandansen"), family = f1, size = 3, hjust = 0, vjust = 0, color = "black", bg.color = "white") +
  coord_sf(clip = "off", expand = FALSE) +
  ylim(c(ymin, ymax)) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = NA, color = NA)
  )

m2 <- ggplot(sweden) +
  geom_sf(linewidth = 0.1, fill = "#F7F4EA") +
  geom_point(aes(x = 13.22, y = 58.35), shape = 21, fill = "#A6CEE3", color = "white", size = 2.5, stroke = 0.5) +
  shadowtext::geom_shadowtext(aes(x = 17, y = 65, label = "Sweden"), family = f1, size = 3, hjust = 0.5, vjust = 0, color = "black", bg.color = "white") +
  theme_void()

m <- m1 + inset_element(m2, left = -0.1, right = 0.5, top = 1, bottom = 0.8)

# Combine charts and map
p1 + m + p2 +
  plot_layout(widths = c(2, 1.5, 3), guides = "collect") +
  plot_annotation(
    title = "Cranes at Lake Hornborga",
    subtitle = str_wrap("Each spring, thousands of cranes stop at the lake during their migration. The charts show daily observations during March and April (left) and weekly observations during August to October (right), 2002-2024. Counts may be missing on days with poor weather conditions.", width = 140),
    caption = "Source: Länsstyrelsen VG län · Graphic: Georgios Karamanis"
  ) &
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(0.5, "lines"),
    legend.title = element_blank(),
    legend.text.position = "top",
    plot.title = element_text(family = f1, face = "bold", size = 18),
    plot.subtitle = element_text(family = f1),
    plot.caption = element_text(family = f1)
  )

