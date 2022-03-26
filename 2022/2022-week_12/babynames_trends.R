library(tidyverse)

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

options(scipen=999)

names_ratio <- babynames %>% 
  mutate(sex_full = case_when(
    sex == "F" ~ "female",
    sex == "M" ~ "male"
  )) %>% 
  pivot_wider(id_cols = c(year, name), values_from = n, names_from = sex_full) %>% 
  mutate(ratio = female/male) %>% 
  # keep only names given to both girls and boys:
  filter(!is.na(ratio)) %>% 
  # remove unknown
  filter(name != "Unknown")

f1 <- "Porpora"
f2 <- "Produkt"

names_switches <- names_ratio %>% 
  mutate(
    log_ratio = log10(ratio)
  ) %>% 
  group_by(name) %>% 
  mutate(
    switches = sum(diff(sign(log_ratio)) != 0),
    biggest_switch = max(log_ratio) - min(log_ratio),
    first_year = min(year, na.rm = TRUE),
    first_ratio = ratio[year == first_year],
    last_year = max(year, na.rm = TRUE),
    last_ratio = ratio[year == last_year]
  ) %>% 
  ungroup()

p <- function(var) {
  names_plot <- names_switches %>% 
    distinct(name, {{var}}) %>% 
    slice_max({{var}}, n = 15) %>%
    left_join(names_switches) %>% 
    mutate(name = fct_reorder(name, -{{var}})) 
  
  ggplot(names_plot) +
    geom_hline(yintercept = 0, size = 0.25) +
    geom_area(aes(year, log_ratio, group = name), size = 0.5, fill = "grey90", color = NA) +
    geom_col(aes(year, log_ratio, fill = ifelse(ratio > 1, "#7900f1", "#448D88")), width = 1.15) +
    geom_area(aes(year, log_ratio, group = name), size = 0.5, fill = NA, color = "black") +
    scale_fill_identity() +
    facet_wrap(vars(name), ncol = 3) +
    theme_void(base_family = f1, base_size = 14) +
    labs(
      caption = "Source: babynames R package · Graphic: Georgios Karamanis"
    ) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      plot.background = element_rect(fill = "grey97", color = NA),
      plot.title = element_text(family = f2, size = 14, hjust = 0.5, margin = margin(0, 0, 20, 0)),
      plot.caption = element_text(size = 10, margin = margin(20, 0, 0, 0), hjust = 0.5, color = "grey40"),
      strip.text = element_text(margin = margin(0, 0, 5, 0)),
      plot.margin = margin(20, 20, 20, 20)
    )
  }

# Plot 1
p(biggest_switch) +
  labs(title = "Biggest Switches")

ggsave(here::here("2022/2022-week_12/plots/babynames-swings_biggest.png"), dpi = 320, width = 8, height = 8)

# Plot 2
p(switches) +
  labs(title = "Most Switches")

ggsave(here::here("2022/2022-week_12/plots/babynames-swings_most.png"), dpi = 320, width = 8, height = 8)
