library(tidyverse)
library(countrycode)
library(waffle)
library(cowplot)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

plastics_continent <- plastics %>% 
  filter(year == 2020) %>% 
  mutate(continent = countrycode(country, origin = "country.name", destination = "continent")) %>% 
  # filter(!is.na(continent)) %>%
  filter(parent_company != "Grand Total") %>%
  pivot_longer(cols = empty:pvc, names_to = "type", values_to = "total") %>% 
  group_by(continent, type) %>% 
  summarise(total = sum(total)) %>% 
  ungroup() %>% 
  mutate(type = fct_relevel(type, c("hdpe", "ldpe", "pet", "pp", "ps", "pvc", "o", "empty")))


p <- ggplot(plastics_continent) +
  geom_waffle(aes(fill = type, values = total/100), n_rows = 10, color = "grey95", radius = unit(1, "pt")) +
  geom_text(aes(x = 5, y = 5, label = continent), stat = "unique", size = 18, hjust = 0, family = "Druk Wide Medium Italic", alpha = 0.95, color = "#8c92ac") +
  scale_fill_brewer(palette = "Set3", guide = guide_legend(nrow = 1, title = NULL), labels = c("HDPE", "LDPE", "PET", "PP", "PS", "PVC", "Other", "Empty")) +
  scale_x_continuous(labels = function(x) x * 10 * 100) +
  # coord_fixed() +
  facet_wrap(vars(continent), ncol = 1, strip.position = "bottom") +
  labs(
    title = "Number of plastics recorded by Break Free From Plastic’s audits",
    subtitle = "by continent and type of plastic for 2020",
    caption = "Source: Break Free From Plastic | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Produkt Regular", base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(10, 0, 0, 0),
    plot.background = element_rect(fill = NA, color = NA),
    axis.ticks.x = element_line(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 0, 20, 0),
    plot.title = element_text(size = 22, hjust = 0.5, family = "Produkt Medium", color = "grey10"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(0, 0, 20, 0), color = "grey10"),
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(20, 0, 0, 0), color = "grey50")
    )

bg <- here::here("2021", "2021-week4", "img", "bottles-w.png")

ggdraw() + 
  draw_image(bg, scale = 1.1) +
  draw_plot(p) 

ggsave(here::here("temp", paste0("plastics-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8.2, width = 12)

