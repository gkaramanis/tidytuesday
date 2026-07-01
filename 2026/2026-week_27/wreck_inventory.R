library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 6.975, height = 8, units = "in", dpi = 320)

wreck_inventory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-30/wreck_inventory.csv')

# Other sources:
# https://inishowenmaritime.com/museum-themes/atlantic-wars-at-sea/
# https://www.hydro-international.com/content/article/shining-a-spotlight-on-irish-sea-shipwrecks

sf_use_s2(FALSE)
target_crs <- 2157

ireland <- rnaturalearth::ne_countries(scale = 10) |>
  filter(admin == "Ireland") |>
  st_transform(target_crs)

# Marine boundary
# https://www.marineregions.org/eezdetails.php?mrgid=5681&zone=eez
ie_marine_raw <- read_sf("https://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:eez&filter=%3CFilter%3E%3CPropertyIsEqualTo%3E%3CPropertyName%3Emrgid_eez%3C/PropertyName%3E%3CLiteral%3E5681%3C/Literal%3E%3C/PropertyIsEqualTo%3E%3C/Filter%3E")

ie_marine <- ie_marine_raw |>
  st_set_crs(4326) |>
  st_transform(target_crs)

wrecks_sf <- wreck_inventory |>
  filter(!is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(target_crs)

wrecks_bbox <- wrecks_sf |>
  st_bbox() |>
  st_as_sfc() |>
  st_buffer(50e3)

bb <- st_bbox(wrecks_bbox)
bb["xmax"] <- bb["xmax"] + 150e3

# Europe for context
europe <- rnaturalearth::ne_countries(scale = 10) |>
  filter(continent == "Europe" & admin != "Ireland") |>
  st_transform(target_crs) |>
  st_crop(bb |> st_as_sfc() |> st_buffer(100e3))

bb_pr <- bb |> 
  st_as_sfc() |>
  st_transform(4326) |>
  st_bbox()

# Bathymetric data
ie_bathy_raw <- marmap::getNOAA.bathy(lon1 = bb_pr$xmin - 1, lon2 = bb_pr$xmax + 1, lat1 = bb_pr$ymin - 1, lat2 = bb_pr$ymax + 1, resolution = 4, keep = TRUE)

ie_bathy <- ie_bathy_raw |> 
  marmap::as.raster() |> 
  terra::rast() |> 
  terra::project(paste0("EPSG:", target_crs))

# Water bodies
# https://data.gov.ie/dataset/lakes-amp-reservoirs-national-250k-map-of-ireland/resource/c9cb71fa-14ac-4c20-b9df-093e93f503d8?inner_span=True
ie_lakes <- read_sf("https://data-osi.opendata.arcgis.com/api/download/v1/items/0081128602fa45f49fe4f56e159040b3/geojson?layers=0") |> 
  st_transform(target_crs)

rivers10 <- rnaturalearth::ne_download(scale = 10, type = "rivers_europe", category = "physical")

ie_rivers <- rivers10 |> 
  st_transform(target_crs) |> 
  st_crop(bb)

#Ports
ie_ports <- tribble(
  ~name, ~lon, ~lat,
  "Dublin", -6.2000, 53.3475,
  "Cork", -8.3100, 51.8330,
  "Shannon Foynes", -9.1080, 52.6110,
  "Rosslare", -6.3389, 52.2536,
  "Waterford", -6.9900, 52.2480
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(target_crs)

ie_ports_extra <- tribble(
  ~name, ~lon, ~lat,
  "Derry (Foyle Port)", -7.2600, 55.0200,
  "Greencastle", -6.9870, 55.2020
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(target_crs)

# Sea names
sea_labels <- tribble(
  ~name, ~lon, ~lat, ~size,
  "ATLANTIC OCEAN", -18.5, 51.5, 5,
  "CELTIC SEA", -7, 50.5, 4,
  "IRISH SEA", -4.5, 54, 3
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(target_crs)

# RMS Leinster
leinster <- wreck_inventory |>
  filter(wreck_no == "W02039") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(target_crs) |>
  mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

f1 <- "DIN Condensed"
f2 <- "Inclusive Sans"

# Plot
ggplot() +
  tidyterra::geom_spatraster(data = ie_bathy) +
  geom_sf(data = europe, fill = "grey99", color = "grey99") +
  ggpattern::geom_sf_pattern(data = ie_marine, color = NA, fill = NA, pattern_density = 0.01, pattern_spacing = 0.01, pattern_color = alpha("#e7dcc0", 0.8)) +
  geom_sf(data = ireland, fill = "#e7dcc0", color = "black") +
  geom_sf(data = ie_lakes, fill = "#9ecae1", color = NA) +
  geom_sf(data = ie_rivers, color = "#9ecae1") +
  geom_sf_text(data = sea_labels, aes(label = name, size = size), family = f1, fontface = "italic", color = "#4a6f82") +
  geom_sf(data = wrecks_sf, color = "#b1402f", size = 0.8, alpha = 0.6) +
  ggforce::geom_mark_circle(data = leinster, aes(x = X, y = Y, x0 = X - 0.5e5, y0 = Y - 6e5, label = "RMS Leinster, 1918", description = "Torpedoed by the German submarine UB-123, she sank with the loss of more than 500 of those aboard, the Irish Sea's worst single loss of life. The wreck now lies buried in sand about 25 metres down."), expand = 0, color = "grey20", con.colour = "grey20", label.family = c(f1, f2), label.fontsize = c(12, 9), label.fill = alpha("grey99", 0.85)) +
  geom_sf(data = ie_ports, size = 3, color = "black", shape = "✪") +
  ggrepel::geom_text_repel(data = ie_ports, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, bg.color = "#e7dcc0") +
  geom_sf(data = ie_ports_extra, size = 1, fill = "white", shape = 21) +
  ggrepel::geom_text_repel(data = ie_ports_extra, aes(label = name, geometry = geometry), stat = "sf_coordinates", family = f1, bg.color = "white") +
  tidyterra::scale_fill_hypso_c(palette = "wiki-2.0_bathy") +
  scale_size_identity() +
  coord_sf(expand = FALSE, xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  labs(
    title = "Shipwrecks off Ireland",
    subtitle = str_wrap("The hatched band marks Ireland's maritime boundary. Most wrecks lie on the shallow shelf, a seabed still being mapped by sonar surveys that keep finding more. They cluster in the north, where wartime ships gathered and many were sunk by mines and submarines, and along the busy shipping routes into Dublin, Cork and Waterford.", 88),
    caption = "Source: Wreck Inventory of Ireland Database (WIID) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(family = f1, size = 25, face = "bold", margin = margin(b = 8, r = 10, l = 10)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 7,  r = 10, l = 10)),
    plot.caption = element_text(size = 9, margin = margin(t = 6, r = 10, l = 10), hjust = 0),
    plot.margin = margin(10, 0, 10, 0)
  )
  
record_polaroid()
