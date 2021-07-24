library(tidyverse)
library(lubridate)
library(patchwork)
library(ggtext)

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

west_grid <- geofacet::us_state_grid1 %>% 
  filter(code %in% c("AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"))

d_lvl <- drought %>% 
  mutate(
    drought_lvl = fct_relevel(drought_lvl, c("None", "D0", "D1", "D2", "D3", "D4")),
    year = year(valid_start),
    week = week(valid_start)
    ) %>% 
  left_join(west_grid, by = c("state_abb" = "code")) %>% 
  filter(!is.na(name))

f1 = "Founders Grotesk Condensed"

# Set theme
theme_set(
  theme_minimal(base_family = f1) +
    theme(
      plot.background = element_rect(fill = "#F3F5F8", color = NA),
      strip.text = element_text(family = f1, face = "bold", size = 12),
      axis.title = element_blank(),
      panel.grid = element_line(color = "grey90", size = 0.1)
    )
  )

# Plot function
plot_f <- function(d_level, color) {
  d_lvl %>% 
    filter(drought_lvl == d_level) %>% 
    ggplot(aes(x = week,
               y = area_pct,
               color = if_else(year == 2021, color, paste0("grey", 90 - (year - 2000))),
               size = if_else(year == 2021, 0.35, 0.25),
               group = year)) +
    geom_line() +
    scale_color_identity() +
    scale_size_identity() +
    scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 53)) +
    facet_wrap(vars(name))
}

p1 <- plot_f("D3", "red")
p2 <- plot_f("D4", "darkred")

p <- p1 | p2

p + plot_annotation(
    title = "<span style = 'color:red;'>Extreme</span> and <span style = 'color:darkred;'>exceptional</span> drought in the West",
    subtitle = ("Percent of land area in extreme and exceptional drought conditions by week of year for 2021 (red and dark red) and previous years (gray)"),
    caption = "Source: U.S. Drought Monitor · Graphic: Georgios Karamanis",
    theme = theme(
      plot.margin = margin(20, 20, 20, 20),
      plot.title = element_textbox_simple(size = 26, face = "bold"),
      plot.subtitle = element_text(size = 15, margin = margin(5, 0, 10, 0))
    )
  )

# ggsave(here::here(paste0("drought-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 10)
