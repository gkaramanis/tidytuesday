library(tidyverse)
library(ggfx)
library(ragg)

employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

f1 = "Founders Grotesk Condensed"
f1b = "Founders Grotesk Condensed Bold"
f2 = "Graphik"

employed_sex <- employed %>% 
  filter(!is.na(industry_total)) %>% 
  filter(race_gender != "TOTAL") %>% 
  filter(race_gender == "Men" | race_gender == "Women") %>% 
  distinct(industry, race_gender, industry_total, year) %>% 
  pivot_wider(names_from = race_gender, values_from = industry_total) %>% 
  group_by(year, industry) %>% 
  mutate(ratio = Women/(Men + Women)) %>% 
  ungroup() %>% 
  filter(year == 2020) %>% 
  mutate(
    industry = str_replace_all(industry, "and", "&"),
    industry = str_replace_all(industry, ", &", ","),
    industry = str_replace_all(industry, "except private", "excl. priv."),
    industry = fct_reorder(toupper(industry), ratio),
    l_mm = 25.4 * strwidth(industry, family = f1b, units = "inches"),
    s = 4.9 * max(l_mm)/l_mm,
    h = s / 10
  ) %>% 
  arrange(ratio) %>% 
  mutate(y = cumsum(lag(h/2, default = 0) + h/2))

bg_col = "grey93"
pal <- c("#448D88", "#7900f1", "#DD644E") # men, women, orange

agg_png(here::here("temp", paste0("employed-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), res = 300, height = 8, width = 6, units = "in")

p <- ggplot(employed_sex) +
  geom_vline(xintercept = 0, color = pal[3], size = 0.6) +
  as_reference(
    geom_tile(aes(x = -0.35 + 0.7 * ratio/2, y = y, width = 0.7 * ratio, height = 0.95 * h)),
    id = "women_r"
  ) +
  as_reference(
    geom_tile(aes(x = 0.35 - 0.7 * (1-ratio)/2, y = y, width = 0.7 * (1 - ratio), height = 0.95 * h)),
    id = "men_r"
  ) +
  with_blend(
    geom_text(aes(x = 0, y = y, label = industry, size = s), family = f1, fontface = "bold", color = pal[2]),
    bg_layer = "women_r",
    blend_type = "in"
  ) +
  with_blend(
    geom_text(aes(x = 0, y = y, label = industry, size = s), family = f1, fontface = "bold", color = pal[1]),
    bg_layer = "men_r",
    blend_type = "in"
  ) +
  # annotations
  annotate("text", x = -0.4, y = 16, label = "More women  ⇢", angle = 90, color = pal[2], family = f1, fontface = "bold", size = 6) +
  annotate("text", x = 0.4, y = 11.1, label = "More men  ⇢", angle = 270, color = pal[1], family = f1, fontface = "bold", size = 10) +
  # caption
  annotate("text", x = -0.57, y = 0, label = "Source: BLS | Graphic: Georgios Karamanis", angle = 90, family = f2, hjust = 0, color = "grey30", size = 3) +
  scale_size_identity() +
  scale_x_continuous(limits = c(-0.6, 0.6)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = toupper("Sex ratio by industry, 2020")
    ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.title = element_text(family = f2, face = "bold", hjust = 0.5, size = 25, margin = margin(5, 0, 20, 0), color = pal[3]),
    plot.margin = margin(20, 0, 20, 0)
  )

print(p)

dev.off()
