library(tidyverse)
library(camcorder)
library(geomtextpath)
library(ggforce)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

decade_routes <- slave_routes %>% 
  mutate(
    decade = year_arrival - year_arrival %% 10
  ) %>% 
  group_by(decade) %>% 
  summarise(
    total = sum(n_slaves_arrived, na.rm = TRUE),
    routes = n()
    ) %>% 
  ungroup() 

peak <- decade_routes %>% 
  filter(between(decade, 1750, 1820)) %>% 
  summarise(total = sum(total))

f1 <- "Quotes Caps"
f2 <- "Sora"

# String for chain
chain = paste(rep("o-", 315), collapse = "")

ggplot(decade_routes, aes(routes, total)) +
  # Annotation for 1750-1820
  geom_mark_circle(aes(filter = between(decade, 1750, 1820), description = str_wrap(paste(round(peak$total/1e6, 1), "million people were transferred between 1750 and 1820"), 22)), color = "salmon3", label.colour = "white", con.colour = "salmon3", label.fill = NA, size = 0.4, con.size = 0.2, label.fontsize = 9, label.family = f2, label.hjust = 1, con.type = "none", linetype = "dotted") +
  # Decade markers
  geom_point(aes(size = if_else(decade %% 100 == 0, 5, 2.5)), color = "salmon1") +
  # Chain
  geom_textpath(label = chain, size = 2.5, fontface = "bold", color = "grey85") +
  # Decade labels
  ggrepel::geom_text_repel(data = subset(decade_routes, decade %% 50 == 0), aes(label = decade), nudge_x = 100, nudge_y = -10000, hjust = 0, size = 8, family = f1, point.padding = 0.75, segment.size = 0.2, color = "white") +
  # Subtitle
  annotate("text", 50, 475000,
    label = str_wrap(
      "More than 12 million enslaved Africans were forcibly transferred across the Atlantic by European colonizers over a span of 400 years. Approximately 1.2–2.4 million died during the voyage. The graphic shows the number of slaves transported by decade (of a total of about 5 million people), as documented in the records of more than 36 000 voyages between 1514 and 1866.", 55), hjust = 0, vjust = 1, size = 4, colour = "white", family = f2) +
  # Title
  annotate("text", 50, 5.12e5, label = "TRANS-ATLANTIC SLAVE TRADE", hjust = 0, vjust = 1, size = 10, colour = "coral1", family = f1) +
  # Caption
  annotate("text", 1550, 1e4,
    label = "SOURCE: SLAVE VOYAGES & WIKIPEDIA · GRAPHIC: GEORGIOS KARAMANIS", hjust = 0, vjust = 1, size = 2.5, colour = "grey50", family = f2) +
  scale_size_identity() +
  scale_x_continuous(labels = scales::label_number_auto(), breaks = c(1e3, 2e3, 3e3)) +
  scale_y_continuous(labels = scales::label_number_auto(), breaks = seq(1e5, 5e5, 1e5)) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Voyages per decade",
    y = "Slaves transferred per decade"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "grey10", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    panel.grid = element_line(color = "grey25"),
    panel.grid.major = element_line(size = 0.3),
    panel.grid.minor = element_line(size = 0.15),
    axis.text = element_text(color = "grey60"),
    axis.title = element_text(color = "grey90"),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  )
  
