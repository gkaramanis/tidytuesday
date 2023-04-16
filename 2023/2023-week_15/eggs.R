library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
  
eggs <- eggproduction %>% 
  mutate(
    prod = interaction(prod_process, prod_type),
    prod = case_when(
      prod == "all.hatching eggs" ~ "Hatching eggs (All)",
      prod == "all.table eggs" ~ "Table eggs (All)",
      prod == "cage-free (non-organic).table eggs" ~ "Cage-free table eggs (Non-organic)",
      prod == "cage-free (organic).table eggs" ~ "Cage-free table eggs (Organic)",
      TRUE ~ prod
    )
    ) %>% 
  mutate(year = year(observed_month), month = month(observed_month, label = TRUE), prod) 
 
f1 <- "Outfit"
f2 <- "Saira"

lbl <-  data.frame(y = 2016:2021) %>% 
  mutate(x = "Jan")

ggplot(eggs) +
  geom_tile(aes(month, year, fill = n_eggs), color = "grey99") +
  geom_text(aes(month, 2022, label = month), family = f2, size = 3, stat = "unique") +
  shadowtext::geom_shadowtext(data = lbl, aes(x, y, label = y, color = if_else(y == 2016, "white", "white")), size = 2, family = f2, bg.color = alpha("grey20", 0.4)) +
  scale_y_continuous(limits = c(2015, 2022)) +
  scale_color_identity() +
  scale_fill_viridis_c(option = "turbo", labels = scales::unit_format(unit = "B", scale = 1e-9), name = NULL) +
  coord_polar(clip = "off") +
  facet_wrap(vars(prod)) +
  labs(
    title = "Number of eggs produced by month and type of production process and housing, 2016-2021",
    caption = "Source: The Humane League's US Egg Production dataset · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.key.width = unit(0.5, "line"),
    legend.key.height = unit(2, "line"),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(family = f1, margin = margin(0, 0, 20, 0), face = "plain"),
    plot.caption = element_text(size = 8, color = "grey30"),
    plot.margin = margin(10, 10, 10, 10)
  )
  
