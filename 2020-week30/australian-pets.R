library(tidyverse)
library(janitor)
library(lubridate)
library(ggimage)
library(futurevisions)
library(colorspace)
library(sf)
library(cowplot)

animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv') %>% 
  clean_names()

# Read in population data -------------------------------------------------
population <- read_csv(here::here("2020-week30", "data", "population.csv")) %>% 
  set_names(c("area", "population"))

noisy <- animal_complaints %>% 
  left_join(population, by = c("electoral_division" = "area")) %>%
  filter(complaint_type == "Noise" & electoral_division != "Unallocated") %>%
  select(-suburb) %>%
  mutate(
    div_n = as.numeric(str_extract(electoral_division, "\\d+")),
    date_received = as.Date(parse_date_time(date_received, orders = c("m Y"))),
    year = year(date_received),
		month_n = month(date_received),
		month_i = str_sub(month(month_n, label = TRUE), 1, 1)
    ) %>%
  group_by(div_n, year, month_n) %>% 
  mutate(
    total = n(),
    pct = total/population
    ) %>% 
  ungroup() %>% 
  group_by(div_n, year) %>% 
  mutate(year_total = n()) %>% 
  ungroup() %>% 
  distinct(electoral_division, div_n, year, month_n, month_i, total, year_total, pct) %>%
	filter(year < 2020 & year > 2013) %>% 
  mutate(
    label_year = "Total\nby year",
    label_year_x = -2,
    label_year_y = 2013,
    label_month = "Complaints per capita\nby month",
    label_month_x = 12,
    label_month_y = 2013
  ) %>% 
  add_row(div_n = 5.3, electoral_division = "") %>% 
  add_row(div_n = 5.7, electoral_division = " ") %>% 
  mutate(electoral_division = fct_reorder(electoral_division, div_n))

# Palette -----------------------------------------------------------------
pal <- futurevisions("grand_tour")

# Month labels ------------------------------------------------------------
months = data.frame(x = 1:12, y = 2020) %>% 
  mutate(label = month(x, abbr = FALSE, label = TRUE))

# Midpoints for monthly per capita and yearly complaints, used in  --------
mid_pct = (max(noisy$pct, na.rm = TRUE) - min(noisy$pct, na.rm = TRUE))/2 + min(noisy$pct, na.rm = TRUE)
mid_year = (max(noisy$year_total, na.rm = TRUE) - min(noisy$year_total, na.rm = TRUE))/2 + min(noisy$year_total, na.rm = TRUE)

# Plot --------------------------------------------------------------------
tiles <- ggplot(noisy) +
	geom_tile(aes(month_n, year, height = 0.5, width = 0.9 * pct/max(noisy$pct, na.rm = TRUE), fill = pct), colour = NA) +
  geom_text(aes(month_n, 2019.6, label = month_i), check_overlap = TRUE, family = "Proxima Nova Light", size = 2) +
  geom_text(aes(-2, year, label = year_total, colour = year_total), check_overlap = TRUE, family = "Produkt", size = 2) +
	geom_text(aes(label_year_x, label_year_y, label = label_year), family = "Proxima Nova Light", size = 2, lineheight = 0.8, check_overlap = TRUE) +
  geom_text(aes(label_month_x, label_month_y, label = label_month), family = "Proxima Nova Light", size = 2, lineheight = 0.8, check_overlap = TRUE, hjust = 1) +
  labs(
    title = "Noisy Dogs",
    subtitle = "Complaints about noise caused by dogs in Townsville, Australia, 2014-2019",
    caption = "Source: data.gov.au | Graphic: Georgios Karamanis"
  ) +
	scale_x_continuous(limits = c(-5, 12.5), breaks = 1:12) +
	scale_y_reverse(limits = c(2020, 2012), breaks = 2019:2014, expand = c(0, 0)) +
# Fill scale for tiles ----------------------------------------------------
	scale_fill_steps2(low = pal[2], mid = pal[6], high = pal[4], midpoint = mid_pct) +
# Colour scale for year totals --------------------------------------------
  scale_colour_steps2(low = pal[2], mid = pal[6], high = pal[4], midpoint = mid_year) +
  facet_wrap(vars(electoral_division), ncol = 4) +
  theme_void() +
  theme(
    legend.position = "none",
		plot.background = element_rect(fill = NA, colour = NA),
		plot.title = element_text(hjust = 0.5, family = "Publico Headline Bold", size = 14, margin = margin(0, 0, 8, 0)),
		plot.subtitle = element_text(hjust = 0.5, family = "Produkt", margin = margin(0, 0, 10, 0)),
		plot.caption = element_text(hjust = 0.5, family = "Produkt", size = 6, margin = margin(10, 0, 0, 0)),
		strip.text = element_text(family = "Produkt", margin = margin(10, 0, 0, 0)),
		axis.text.y = element_text(family = "Proxima Nova Light", size = 5),
    plot.margin = margin(20, 20, 20, 20)
  )

# Source script for map ---------------------------------------------------
source(here::here("2020-week30", "australian-pets-sf.R"))

ggdraw(map) +
  draw_plot(tiles) +
  ggsave(here::here("2020-week30", "plots", "temp", paste0("australian-pets-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 8, height = 8)
 
