library(tidyverse)
library(ggforce)
library(ggtext)
library(futurevisions)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

tbi_deaths <- tbi_age %>% 
  filter(type == "Deaths" & age_group != "0-17" & age_group != "Total") %>% 
  na.omit() %>% 
  group_by(age_group) %>% 
  mutate(freq = number_est/sum(number_est)) %>% 
  top_n(1, wt = freq) %>% 
  ungroup() %>% 
  mutate(
    age_group = fct_relevel(age_group,
                            c("0-4", "5-14", "15-24",
                              "25-34", "35-44", "45-54",
                              "55-64", "65-74", "75+")),
    age_n = as.numeric(as.factor(age_group)),
    inj_n = as.numeric(as.factor(injury_mechanism))
  )

ggplot(tbi_deaths, aes(x = c(0.5, 2, 5, 6, 6, 10, 11, 10, 11),
                       y = c(4, 6, 4, 4, 6, 5, 4, 3, 0),
                       group = -1L, fill = injury_mechanism)) +
  geom_voronoi_tile(max.radius = 5, size = 0.75, colour = "black") +
  # geom_text(aes(label = age_group),
  #           colour = "white", family = "IBM Plex Sans",
  #           stat = 'delvor_summary', switch.centroid = TRUE) +

  # 0-4 ----------------------------------------------------------------
  annotate("richtext", x = -6, y = 2,
           label = "<span style='color: #DB3A2F'>ASSAULT</span><br><span style='color:grey30'>0-4 yr: 55%</span>",
           hjust = 1, family = "IBM Plex Sans Condensed Bold",
           size = 5, fill = NA, label.color = NA) +
  # annotate("segment", x = -5, y = -2, xend = -2, yend = 2, size = 1,
  #          colour = "grey20", lineend = "round") +
  # 5-24 --------------------------------------------------------------------
  annotate("richtext", x = 5, y = -5,
           label = "<span style='color: #275D8E'>VEHICLE CRASHES</span><br><span style='color:grey30'>5-14 yr: 38%<br>15-24 yr: 53%</span>",
           hjust = 1, family = "IBM Plex Sans Condensed Bold",
           size = 5, fill = NA, label.color = NA) +
  # annotate("segment", x = 4, y = -3, xend = 4, yend = 1, size = 1,
  #          colour = "grey20", lineend = "round") +
  # 25-64 --------------------------------------------------------------------
  annotate("richtext", x = 26, y = 9,
           label = "<span style='color: #EAB33A'>INTENTIONAL SELF-HARM</span><br><span style='color:grey30'>25-34 yr: 41%<br>35-44 yr: 45%<br>45-54 yr: 45%<br>55-64 yr: 42%</span>",
           hjust = 1, family = "IBM Plex Sans Condensed Bold",
           size = 5, fill = NA, label.color = NA) +
  # annotate("segment", x = 13, y = 6, xend = 18, yend = 8, size = 1,
  #          colour = "grey20", lineend = "round") +
  # 65+ --------------------------------------------------------------------
annotate("richtext", x = 25, y = -8,
         label = "<span style='color: #902A57'>UNINTENTIONAL FALLS</span><br><span style='color:grey30'>65-74 yr: 36%<br>75+ yr: 67%</span>",
         hjust = 1, family = "IBM Plex Sans Condensed Bold",
         size = 5, fill = NA, label.color = NA) +
  # annotate("segment", x = 17, y = -5, xend = 11, yend = -1, size = 1,
  #          colour = "grey20", lineend = "round") +
  labs(
    title = "Leading causes of traumatic brain injury-related\ndeaths by age group",
    caption = "Source: CDC | Graphic: Georgios Karamanis"
  ) +
  coord_fixed(xlim = c(-5, 20), ylim = c(-10, 14), clip = "off") +
  scale_fill_manual(values = futurevisions("mars")) +
  theme_void(base_family = "JetBrains Mono") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "IBM Plex Serif Bold", size = 18, margin = margin(10, 0, 0, 0)),
    plot.caption = element_text(size = 6, hjust = 0),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("2020-week13", "plots", "tbi.png"), dpi = 300, width = 8, height = 6)
  