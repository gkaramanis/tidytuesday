library(tidyverse)
library(treemapify)
library(geofacet)
library(RColorBrewer)
library(patchwork)
library(marquee)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 14, height = 9, units = "in", dpi = 320)

source(here::here("2026/2026-week_03/afica-cleaning-script.R"))

# function to shorten names
shorten_names <- function(country) {
  country <- str_replace(country, "Democratic Republic", "DR")
  country <- str_replace(country, "Republic", "R.")
  country <- str_replace(country, "São Tomé and Principe", "S. Tomé & Príncipe")
  country <- str_replace(country, "Equatorial Guinea", "Eq. Guinea")
}

africa_languages <- africa |> 
  mutate(
    name = case_when(
      country == "Ivory Coast" ~ "Côte d'Ivoire",
      country == "Cape Verde" ~ "Cabo Verde",
      country == "São Tomé and Príncipe" ~ "São Tomé and Principe",
      country == "Guinea Bissau" ~ "Guinea-Bissau",
      TRUE ~ country
    )
  ) |> 
  mutate(
    name = shorten_names(name),
    label = case_when(
      native_speakers >= 1e7 ~ paste0(language, "\n", round(native_speakers / 1e6, 1), "M"),
      TRUE ~ language
    )
    )

f1 <- "DIN Condensed"
f2 <- "Sofia Sans Extra Condensed"
f3 <- "Source Serif Pro"

pal  <- c(
  brewer.pal(name = "Set2", n = 8),
  brewer.pal(name = "Paired", n = 8)
  )

africa_grid_wrapped <- africa_countries_grid1 |> 
  as_tibble() |> 
  mutate(name = shorten_names(name))

p1 <- africa_languages |> 
  distinct(family, language, native_speakers, label) |> 
  ggplot(aes(area = native_speakers, fill = family, subgroup = family, label = label)) +
  geom_treemap(color = "white") +
  geom_treemap_text(family = f2) +
  geom_treemap_subgroup_text(family = f1) +
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Africa's extraordinary linguistic diversity",
    subtitle = "**Treemap ↓** African languages by number of native speakers, grouped by language family. **Map →** Main languages for each country (not proportional).  
    Africa has over 2 000 languages, with {#A3C487 **Niger–Congo**} (435 million native speakers, 377 languages), {#66C2A5 **Afroasiatic**} (333 million, 21), and {#33A02C **Nilo-Saharan**} (44 million, 69) among the largest families.   
    Most countries are multilingual, and the 'main' language is often just one among many. Some languages serve as lingua francas (e.g., {#A3C487 **Swahili**}, {#66C2A5 **Hausa**}, {#66C2A5 **Arabic**}), and many countries have multiple official or national languages.",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 22, family = f3, face = "bold"),
    plot.subtitle = element_marquee(size = 12, family = f3, width = 1.01, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, size = 10, family = f3, color = "grey30", margin = margin(t = 10))
  )

p2 <- ggplot(africa_languages, aes(area = 1, fill = family, subgroup = language, label = language)) +
  geom_treemap(color = "white") +
  geom_treemap_subgroup_text(family = f2) +
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off") +
  facet_geo(vars(name), grid = africa_grid_wrapped, labeller = labeller(name = "x")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title = "FAMILY")) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(color = "grey19", size = 10, margin = margin(0, 7, 0, 3)),
    legend.title = element_text(color = "grey19", size = 12),
    legend.key.size = unit(0.7, "lines"),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(size = 11),
    strip.clip = "off",
    plot.margin = margin(0, 10, 0, 30)
  )
  
wrap_elements(p1) + wrap_elements(p2) +
  plot_layout(widths = c(1.5, 2)) &
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.margin = margin(5, 10, 5, 5)
  ) 

# record_polaroid()
