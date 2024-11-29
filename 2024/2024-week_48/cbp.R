library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 5.6, units = "in", dpi = 320)

# cbp_resp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_resp.csv')

cbp_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_state.csv')

us <- usmap::us_map() %>% 
  select(state = abbr)

cbp_fmua_ukr <- cbp_state %>% 
  filter(demographic == "FMUA") %>%
  filter(citizenship == "UKRAINE") %>% 
  group_by(state, fiscal_year, month_abbv) %>% 
  reframe(n = sum(encounter_count, na.rm = TRUE)) %>% 
  mutate(
    month = str_to_title(month_abbv),
    month = fct_reorder(month, match(month, month.abb))
    )

us_ukr_map <- us %>% 
  left_join(cbp_fmua_ukr) %>% 
  filter(!is.na(month_abbv)) 
  
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

pal <- MetBrewer::met.brewer("VanGogh3")[1:6]

ggplot(us_ukr_map) +
  geom_sf(data = us, fill = "grey99", linewidth = 0.05) +
  geom_sf(aes(fill = log(n)), linewidth = 0.2, color = "#353231") +
  scale_fill_stepsn(
    colors = pal,
    labels = function(x) scales::number(round(exp(x))),
    breaks = log(c(10, 100, 1000, 10000)),
    name = "Number of encounters (logarithmic scale)"
  ) +
  facet_grid(cols = vars(month), rows = vars(fiscal_year), switch = "y") +
  labs(
    title = "Ukrainian Family Unit Encounters at US Borders",
    subtitle = str_wrap("Monthly encounters of Ukrainian family units by state from 2021-2023, showing increased migration following Russia's invasion of Ukraine in February 2022.", 140),
    caption = "Source: U.S. Customs and Border Protection (CBP) · Graphic: Georgios Karamanis",
    fill = ""
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.border = element_rect(fill = NA, color = "#353231", linewidth = 0.3),
    legend.position = "top",
    legend.key.width = unit(4, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(10, 0, 10, 0),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    strip.text.x = element_text(margin = margin(0, 0, 5, 0), color = "#353231", family = f1b),
    strip.text.y = element_text(margin = margin(0, 5, 0, 0), color = "#353231", family = f1b),
    plot.margin = margin(10, 15, 10, 15),
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold", family = f2),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0), size = 12),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )
  
 