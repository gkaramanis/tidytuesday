library(tidyverse)
library(gt)
library(gtUtils)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

college_admissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-10/college_admissions.csv')

att <- college_admissions %>% 
  # First, calculate the overall average attendance rate for each tier
  group_by(tier) %>%
  mutate(tier_avg_attend = sum(rel_attend * attend_level, na.rm = TRUE) / sum(attend_level, na.rm = TRUE)) %>%
  # Now group by tier and income level
  group_by(tier, par_income_lab) %>% 
  summarise(
    # Calculate weighted average attendance rate
    weighted_rel_att = sum(rel_attend * attend_level, na.rm = TRUE) / sum(attend_level, na.rm = TRUE),
    # Use the first (they should all be the same within a tier) tier average for normalization
    tier_avg_attend = first(tier_avg_attend)
  ) %>% 
  # Normalize the weighted attendance rate
  mutate(normalized_weighted_rel_att = weighted_rel_att / tier_avg_attend) %>%
  # Remove the intermediate calculation column
  select(-tier_avg_attend) %>%
  ungroup()

att %>% 
  select(tier, "Parent income (percentile)" = par_income_lab, weighted_rel_att) %>% 
  pivot_wider(names_from = tier, values_from = weighted_rel_att) %>% 
  gt() %>% 
  gt_color_pills(`Highly selective private`, digits = 2) %>% 
  gt_color_pills(`Highly selective public`, digits = 2) %>% 
  gt_color_pills(`Ivy Plus`, digits = 2) %>% 
  gt_color_pills(`Other elite schools (public and private)`, digits = 2) %>% 
  gt_color_pills(`Selective private`, digits = 2) %>% 
  gt_color_pills(`Selective public`, digits = 2) %>% 
  tab_header(
    title = "Who goes where? College attendance across income levels",
    subtitle = "Test-score-adjusted attendance rates by college tier, relative to average across income groups. Rates show the proportion of students attending each college type within parent income bins, normalized to highlight disparities."
  ) %>% 
  gt_theme_athletic() %>% 
  cols_width(everything() ~ px(120)) %>% 
  gt_border_bars_top("#1c2632", bar_height = 5) %>%
  gt_border_bars_bottom("#1c2632", bar_height = 25, text = "Source: Opportunity Insights · Graphic: Georgios Karamanis", text_weight = "normal", text_size = 12) %>% 
  gt_save_crop(here::here("2024/2024-week_37/plots/college_admissions.png"))
