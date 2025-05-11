library(tidyverse)
library(sf)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 8, units = "in", dpi = 320)

# Fetch data from the CSV download link at https://grant-watch.us/nsf-data.html
raw_nsf_terminations <- readr::read_csv("https://drive.usercontent.google.com/download?id=1TFoyowiiMFZm73iU4YORniydEeHhrsVz&export=download")

# Clean the data
nsf_terminations <- raw_nsf_terminations |> 
  janitor::clean_names() |> 
  mutate(usaspending_obligated = stringi::stri_replace_first_fixed(usaspending_obligated, "$", "") |> 
           readr::parse_number()) |> 
  mutate(in_cruz_list = !is.na(in_cruz_list)) |> 
  mutate(grant_number = as.character(grant_number)) 


# https://catalog.data.gov/dataset/congressional-districts5
# https://geodata.bts.gov/datasets/usdot::congressional-districts/about
cd <- read_sf("https://stg-arcgisazurecdataprod.az.arcgis.com/exportfiles-273-8762/NTAD_Congressional_Districts_1928759543695387395.geojson?sv=2018-03-28&sr=b&sig=EFx2DKSbrpPa3EE6L%2BfiPXSpI9saui4wKuvqCrMZgo4%3D&se=2025-05-11T08%3A18%3A56Z&sp=r")

cd_sf <- cd %>%
  select(state = STATEFP, district_code = CD119FP, party = PARTY) %>%
  mutate(district = paste0(state, district_code)) %>%
  select(district, party)

nsf_districts <- nsf_terminations %>%
  select(state = org_state, org_district, usaspending_obligated) %>%
  # In a few cases, the recipient's state is different to the org state, set them to the org state
  mutate(
    state = case_when(
      state != str_sub(org_district, 1, 2) ~ str_sub(org_district, 1, 2),
      TRUE ~ state
    )
  ) %>% 
  group_by(state, org_district) %>%
  summarise(spending = sum(usaspending_obligated)) %>%
  ungroup() %>%
  left_join(tidycensus::fips_codes %>% select(state, state_code) %>% distinct()) %>%
  mutate(district = paste0(state_code, str_remove(org_district, state))) %>%
  left_join(cd_sf) %>%
  `st_geometry<-`("geometry")


# https://docs.google.com/spreadsheets/d/13XkF59JKzvw4SeSq5mbgIFrJfYjK4amg9JoQE5e9grQ/edit?gid=1250379179#gid=1250379179
cd_hex <- read_sf(here::here("2025/2025-week_19/data/HexCDv31/")) %>% 
  st_make_valid()

spending_hex <- cd_hex %>% 
  left_join(nsf_districts %>% st_drop_geometry(), by = c("GEOID" = "district"))

spike_base_width <- 0.5

hex_c <- spending_hex %>% 
  filter(spending > 0) %>% 
  mutate(
    x_start_coord = st_coordinates(st_centroid(st_geometry(.)))[,1],
    y_start_coord = st_coordinates(st_centroid(st_geometry(.)))[,2],
    s_line_len = spending * 6 / max(spending), 
    x_tip = x_start_coord, 
    y_tip = y_start_coord + s_line_len 
  ) %>%
  st_drop_geometry() %>%
  select(
    GEOID, party, spending, 
    s_line_len,             
    x_start_coord, y_start_coord, x_tip, y_tip
  ) %>%
  mutate(
    geometry = st_sfc(
      pmap(
        .l = list(x_start_coord, y_start_coord, x_tip, y_tip),
        .f = ~ {
          # V1: bottom-left, V2: bottom-right, V3: top-center (tip)
          list(matrix(c(..1 - spike_base_width / 2, ..2,  # V1_x, V1_y
                     ..1 + spike_base_width / 2, ..2,  # V2_x, V2_y
                     ..3, ..4,                        # V3_x (tip_x), V3_y (tip_y)
                     ..1 - spike_base_width / 2, ..2), # V1_x, V1_y (close polygon)
                   ncol = 2, byrow = TRUE)) %>% 
            st_polygon()
        }
      ),
      crs = st_crs(spending_hex)
    )
  ) %>%
  st_as_sf() %>%
  select(GEOID, party, spending, scaled_spike_height = s_line_len, x_tip, y_tip) 

pal <- list(
  bg = "#F8F8F7",    
  text = "#333333",  
  dem = "#0072B2",   
  rep = "#D55E00",   
  fill = "#E0E0E0" 
)

f1 <- "Inter Display"
f2 <- "Sofia Sans Extra Condensed"

f_mil <- function(x) paste0("$", round(x / 1e6, 1), "M")
f_bil <- function(x) paste0("$", round(x / 1e9, 1), "B")

# Calculate total number of grants and total sum
total_grants_count <- nrow(nsf_terminations)
total_grants_sum <- sum(nsf_terminations$usaspending_obligated, na.rm = TRUE)

m <- ggplot() +
  geom_sf(data = spending_hex, fill = pal$fill, color = "white") + 
  geom_sf(data = hex_c, aes(fill = party), color = NA, alpha = 0.9) +
  shadowtext::geom_shadowtext(data = hex_c %>% slice_max(order_by = spending, n = 10), aes(x_tip, y_tip, label = f_mil(spending), color = party), family = f2, fontface = "bold", bg.color = pal$bg, nudge_y = 0.5, size = 5, check_overlap = TRUE) + 
  scale_fill_manual(values = c("D" = pal$dem, "R" = pal$rep)) + 
  scale_color_manual(values = c("D" = pal$dem, "R" = pal$rep)) + 
  labs(
    title = "NSF grant terminations disproportionately affect Democratic districts",
    subtitle = str_wrap(glue::glue("Unprecedented National Science Foundation grant terminations by the Trump administration, part of a major restructuring and budget cuts, are impacting scientific research and education. As of {format(Sys.Date(), format = '%d %b %Y')}, {scales::number_format(big.mark = ' ')(total_grants_count)} grants, totalling {f_bil(total_grants_sum)}, were cancelled without appeal. Data, crowdsourced by Grant Watch due to lack of official release, shows funding lost aggregated per congressional district by party."), 105)
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal$bg, color = NA), 
    plot.title = element_text(face = "bold", size = 18, color = pal$text),
    plot.subtitle = element_text(size = 13, color = pal$text)
  )

p <- nsf_districts %>% 
  st_drop_geometry() %>% 
  group_by(party) %>% 
  summarise(spending = sum(spending, na.rm = TRUE)) %>% 
  ggplot() +
  geom_col(aes(spending, party, fill = party)) +
  geom_text(aes(spending, party, label = f_mil(spending)), family = f2, fontface = "bold", size = 4.5, nudge_x = -2e6, hjust = 1, color = "white") +
  geom_text(data = . %>% filter(is.na(party)), aes(spending, party, label = "Non-voting districts"), family = f2, fontface = "bold", size = 4.5, nudge_x = 2e6, hjust = 0, color = pal$text) +
  scale_x_continuous(breaks = seq(100e6, 400e6, 100e6), labels = f_mil) +
  scale_fill_manual(values = c("D" = pal$dem, "R" = pal$rep)) + 
  labs(
    caption = "Sources: Grant Watch, Data.gov & The Downballot · Graphic: Georgios Karamanis"
      ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal$bg, color = NA), 
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(color = pal$text, hjust = 0) 
  )
  

m / p +
  plot_layout(heights = c(10, 1)) &
  theme(
    plot.background = element_rect(fill = pal$bg, color = NA),
    plot.margin = margin(5, 5, 5, 5)
  )