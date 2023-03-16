library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

first_words <- drugs %>% 
  # Count drug names in each group and filter groups with >30 brand names
  add_count(pharmacotherapeutic_group) %>% 
  filter(!is.na(pharmacotherapeutic_group)) %>% 
  filter(n > 30) %>% 
  # Get the first word of the brand name
  mutate(
    first_word = stringr::word(medicine_name, 1),
    ) %>% 
  # Separate brand names with more names separated by "/"
  separate_longer_delim(first_word, "/") %>% 
  # Get first letter of first word
  mutate(first_letter = str_sub(first_word, 1, 1), .before = 3) %>% 
  # Keep unique combinations
  distinct(pharmacotherapeutic_group, first_word, first_letter)

# Calculate regular frequencies by drug group
first_letters <- first_words %>% 
  count(pharmacotherapeutic_group, first_letter) %>% 
  group_by(pharmacotherapeutic_group) %>% 
  mutate(
    p = n / sum(n) * 100,
    pharmacotherapeutic_group = str_remove(pharmacotherapeutic_group, "\\,")
    ) %>% 
  ungroup()

f1 = "DIN Condensed"
f2 = "Outfit"

pal <- rev(MetBrewer::met.brewer("Greek"))

ggplot(first_letters) +
  geom_segment(aes(x = "A", xend = "Z", y = 0.6, yend = 0.6), linewidth = 0.2, color = "purple4") +
  geom_text(aes(first_letter, 1, label = first_letter, size = p, color = p), family = f1) +
  geom_text(aes("M", 2.5, label = pharmacotherapeutic_group), stat = "unique", family = f2, size = 3.5, color = "purple4") +
  scale_y_continuous(limits = c(-1, 3)) +
  scale_size_continuous(range = c(1, 15)) +
  scale_color_stepsn(colors = pal) +
  facet_wrap(vars(pharmacotherapeutic_group), ncol = 3) +
  labs(
    title = "A is for Actrapid, Z for Zyprexa",
    subtitle = "First letter relative frequencies in drug brand names. For pharmacotherapeutic groups with more than 30 drug names.",
    caption = "Source: European Medicines Agency 💊 Graphic: Georgios Karamanis",
    color = "% of brand names starting with the letter"
  ) +
  guides(size = FALSE, color = guide_colorsteps(show.limits = TRUE, title.position = "top", title.hjust = 0.5)) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = f2) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.5, "line"),
    legend.key.width = unit(2.8, "line"),
    legend.title = element_text(color = "purple4"),
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    panel.spacing.x = unit(2, "lines"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "purple4"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(5, 0, 20, 0), color = "purple4"),
    plot.caption = element_text(color = "purple4")
  )
  
