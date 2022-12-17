library(tidyverse)
library(camcorder)
library(lubridate)
library(geofacet)
library(MetBrewer)
library(ggtext)

gg_record(dir = "temp", device = "png", width = 10, height = 7, units = "in", dpi = 320)

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

us_grid <- us_state_grid1 %>% 
  rename("state" = "code")

extra_grid <- expand_grid(col = 0:12, row = 0:8) %>% 
  filter(col %in% c(0, 12) | row %in% c(0, 8))


airmen_state <- airmen %>% 
  mutate(
    grad_year = year(graduation_date),
    state = case_when(
      state == "In" ~ "IN",
      state == "CN" ~ "CT",
      state == "KN" ~ "KY",
      TRUE ~ state
    )
  ) %>% 
  # filter(!is.na(state) & !is.na(grad_year)) %>%
  count(state) %>%
  left_join(us_grid, by = "state") 

airmen_known <- airmen_state %>% 
  filter(!is.na(name))

airmen_other_na <- airmen_state %>% 
  filter(is.na(name)) %>% 
  summarise(n = sum(n)) %>% 
  mutate(
    state = "Other Countries<br>or Unknown",
    row = 7,
    col = 1
  )

all_names <- paste0(airmen$name, collapse = " · ")

f1 = "Jefferies"
f2 = "Jefferies"

# col1 = "#9A3241" # My red
# col2 = "#D2AB3C" # My yellow
col_red = "#dc143c"
col_brown = "#654321"
col_yellow = "#ffd700"
col_blue = "#4682b4"
col_tan = "#d2b48c"
col_green = "#00aa00"
col_pink = "#ffc0cb"



col_red = "#AD312E"
col_yellow = "#F0C448"
col_brown = "#BE6436"

col_text = "grey97"

ggplot(airmen_known) +
  # Known state
  geom_point(aes(x = col, y = row, size = n, color = n), alpha = 1) +
  geom_richtext(aes(x = col, y = row + 0.025, label = paste0("**", state, "**<br>", n)), size = 4.5, color = col_text, family = f1, fill = NA, label.color = NA) +
  # Other countries or unknown
  geom_point(data = airmen_other_na, aes(x = col, y = row, size = n, color = n), alpha = 1) +
  geom_richtext(data = airmen_other_na, aes(x = col, y = row + 0.025, label = paste0("**", state, "**<br><br><span style='font-size:10pt'>", n, "<span>")), size = 2, color = col_text, family = f1, fill = NA, label.color = NA) +
  # Scale, themes and stuff
  scale_size_continuous(range = c(15, 28)) +
  # scale_color_gradient(low = col_tan, high = col_yellow) +
  scale_color_gradientn(colors = c(col_yellow, col_brown, col_red)) +
  # scale_color_gradientn(colors = c(col_green, col_brown, col_pink, col_red)) +
  coord_cartesian(expand = FALSE) +
  xlim(0.25, 11.75) +
  ylim(7.75, 0.25) +
  labs(
    # title = "Tuskegee Airmen by home state"
    # caption = "Source: Tuskegee Airmen Challenge · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(color = "grey10", family = f2, size = 30, margin = margin(7, 0, 3, 0), hjust = 0.5),
    plot.caption = element_text(color = "grey97", family = f2, size = 12, margin = margin(5, 0, 5, 0), hjust = 0.5),
    # plot.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "grey97", color = NA)
  )

ggsave("~/Desktop/tuskegee.svg")
