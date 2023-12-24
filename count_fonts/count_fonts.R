library(tidyverse)
library(textreadr)
library(camcorder)

gg_record(here::here("tidytuesday-temp"), width = 13, height = 8, dpi = 320)

# get full paths of scripts and year
full_paths <- data.frame(
  path = dir(path = here::here(), pattern = "\\.R", recursive = TRUE)
  ) %>% 
  mutate(
    document = str_remove(base_name(path), ".R"),
    year = parse_number(path)
  ) %>% 
  filter(!is.na(year))

families <- read_dir(path = here::here(), pattern = "\\.R$", recursive = TRUE) %>%
  left_join(full_paths) %>% 
  group_by(document) %>% 
  filter(any(str_detect(content, "((font)*family|f\\d.*) (=|<-) "))) %>%
  ungroup() %>% 
  mutate(
    # Extract raws with "fontfamily =" or "family = " 
    fam1 = str_extract(content, "(font)*family = .+?(,|\\))"),
    # Extract variables with font names, e.g. f1 = "Font"
    fam2 = str_extract(content, "f\\d[a-z0-9]* (=|<-)+ \".+\""),
    # Create new column and extract font names within quotes
    fam = case_when(
      !is.na(fam1) ~ str_extract(fam1, "\".+\""),
      !is.na(fam2) ~ str_extract(fam2, "\".+\"")
    ),
    # remove quotes
    fam = str_remove_all(fam, "\"")
  ) %>% 
  filter(!is.na(fam) & !is.na(year)) %>% 
  distinct(document, fam, year) %>% 
  count(year, fam) %>% 
  group_by(year) %>% 
  mutate(
    freq = round(n / sum(n) * 100, 1),
    total_fams = sum(n),
    face = tolower(word(fam, -1))
    )  %>% 
  arrange(-n)

top_10 <- families %>%
  # Uncomment to subset, otherwise keep all fonts
  slice_max(freq, n = 10, with_ties = FALSE) %>%
  group_by(year) %>% 
  arrange(freq) %>% 
  mutate(i = factor(row_number())) %>% 
  ungroup()

c1 = "grey97" # background
c2 = "grey20" 

ggplot(top_10 ) +
  geom_col(aes(freq, i, fill = word(fam, 1)), width = 0.6) +
  geom_text(aes(freq + 2, i, label = paste0(freq, "%")), size = 5, hjust = 0, family = "Atkinson Hyperlegible", color = c2) +
  geom_text(aes(-3, i, label = fam, family = fam, color = word(fam, 1)), hjust = 1, size = 5.5) +
  scale_x_continuous(breaks = c(0, 20, 40), limits = c(-60, 55)) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(year), scales = "free_y") +
  theme_minimal(base_family = "Atkinson Hyperlegible") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = c1, color = NA),
    plot.margin = margin(20, 20, 20, 90),
    strip.text = element_text(size = 25)
  ) 
