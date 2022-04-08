library(tidyverse)
library(camcorder)
library(stringi)
library(MetBrewer)

gg_record(dir = "temp", device = "png", width = 9, height = 13, units = "in", dpi = 320)

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

news_orgs_chr <- news_orgs %>% 
  filter(!is.na(year_founded)) %>% 
  group_by(year_founded) %>% 
  summarise(
    name = paste0(publication_name, collapse = ""),
    name = tolower(stri_replace_all_charclass(name, "\\p{WHITE_SPACE}", "")),
    chars = str_split(name, "")
    ) %>% 
  ungroup() %>% 
  unnest(chars) %>% 
  select(-name) %>% 
  # unnest_tokens(chars, name, "characters") %>% 
  group_by(year_founded, chars) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(year_founded) %>% 
  mutate(freq = round(n / sum(n), 2)) %>% 
  ungroup() %>% 
  mutate(
    chars = fct_rev(chars),
    type = case_when(
      chars %in% as.character(0:9) ~ "Numbers",
      chars %in% c(letters, "ā", "é", "í") ~ "Letters",
      TRUE ~ "Other"
    )
    )

annot <- tribble(
  ~x, ~y, ~type, ~label,
  1998, "4", "Numbers", "Numbers start appearing\nin 2004"
)

arrow <- tribble(
  ~x, ~xend, ~y, ~yend, ~type,
  1999, 2002, "4", "4", "Numbers"
)

f1 = "Futura"
f2 = "Helvetica Neue"

ggplot(news_orgs_chr) +
  geom_tile(aes(x = year_founded, y = chars, fill = freq), color = NA) +
  geom_text(data = annot, aes(x, y, label = label), family = f2, size = 6, color = "grey30", hjust = 1, lineheight = 0.95) +
  geom_segment(data = arrow, aes(x = x, xend = xend, y = y, yend = yend), arrow = arrow(length = unit(0.05, "npc")), color = "grey30") +
  scale_fill_stepsn(colors = met.brewer("Tam"), breaks = seq(0, 0.3, 0.05)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10), minor_breaks = 1958:2021) +
  scale_y_discrete(position = "right") +
  ggforce::facet_col(vars(type), space = "free", scale = "free_y") +
  theme_minimal(base_family = f1, base_size = 16) +
  guides(fill = guide_colorsteps(show.limits = TRUE, title.position = "top", title = "by year founded", title.hjust = 0.5)) +
  labs(
    title = "Frequency of characters in names of local news organizations",
    caption = "Source: Project Oasis · Graphic: Georgios Karamanis"
  ) +
  theme(
    # legend.position = c(0.22, 0.39),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key.height = unit(0.8, "line"),
    legend.key.width = unit(3, "line"),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "#C5D3D9", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 11, family = f2, color = "black"),
    strip.text = element_text(size = 16, face = "bold"),
    plot.title = element_text(hjust = 0.5, margin = margin(10, 0, -5, 0), size = 20),
    plot.caption = element_text(hjust = 0.5, color = "grey20", margin = margin(15, 0, 0, 0)),
    panel.grid = element_line(color = "#396A76", size = 0.05),
    panel.grid.major = element_line(size = 0.1),
    plot.margin = margin(10, 20, 10, 20)
  )

