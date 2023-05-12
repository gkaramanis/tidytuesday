library(tidyverse)
library(geomtextpath)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')

surv_pl <- surveys %>% 
  filter(!is.na(species)) %>% 
  filter(pregnant == "P" | lactation == "L") %>% 
  select(censusdate, month, species, pregnant, lactation, hfl, wgt) %>% 
  pivot_longer(pregnant:lactation, names_to = "status", values_to = "value") %>% 
  add_count(species) %>% 
  filter(n > 60) %>% 
  count(species, month, value) %>% 
  filter(!is.na(value)) %>% 
  complete(species, month, value, fill = list(n = 0))
  
fake <- surv_pl %>% 
  filter(month == 12) %>% 
  mutate(month = 0)

surv_pl_fake <- surv_pl %>% 
  bind_rows(fake) %>% 
  arrange(month, species) %>% 
  left_join(species)

f1 <- "Saira"

pal <- MetBrewer::met.brewer("Isfahan2")

ggplot(surv_pl_fake) +
  geom_area(aes(month, 50), fill = pal[4], alpha = 0.3) +
  annotate("segment", x = c(1:12), xend = c(1:12), y = 0, yend = 50, color = "purple4", linewidth = 0.1, alpha = 0.25) +
  geom_hline(yintercept = seq(0, 50, 10), alpha = 0.25, linewidth = 0.1, color = "purple4") +
  geom_text(data = data.frame(y = c(20, 40)), aes(x = 4, y = y, label = y), alpha = 0.25, color = "purple4", size = 3, family = f1) +
  geom_area(aes(month, n, fill = value, color = after_scale(colorspace::darken(fill, 0.1))), size = 0.1) +
  geom_texthline(aes(yintercept = 42, label = commonname), stat = "unique", family = f1, hjust = 0.01, linewidth = 0, size = 3, color = "purple4") +
  scale_x_continuous(limits = c(0, 12), breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(-5, 50)) +
  scale_fill_manual(values = c(pal[5], pal[2])) +
  facet_wrap(vars(species)) +
  coord_polar(start = -pi/4, clip = "off") +
  labs(
    title = glue::glue("<span style='color:{pal[2]}'>**Pregnant**</span> and <span style='color:{pal[5]}'>**lactating**</span> rodents observed by month at the site of The Portal Project"),
    subtitle = "Showing the 9 species with the most observations",
    caption = "Source: The Portal Project · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_blank(),
    axis.text.x = element_text(size = 9, color = alpha("grey20", 0.4)),
    plot.title = element_markdown(margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_markdown(margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(size = 7, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(15, 15, 10, 15)
  )

