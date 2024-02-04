library(tidyverse)
library(gt)
library(gtExtras)

groundhogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv')

predictions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv')

preds_list <- predictions %>% 
  mutate(shadow = case_match(
    shadow,
    TRUE ~ 0,
    FALSE ~ 1,
    NA ~ 0.5
  )) %>% 
  group_by(id) %>% 
  reframe(preds = list(shadow))

preds_pct <- predictions %>%
  group_by(id) %>% 
  count(shadow) %>% 
  filter(!is.na(shadow)) %>% 
  mutate(
    total = sum(n),
    early = n / total * 100
    ) %>% 
  filter(!shadow)

groundhogs %>% 
  filter(!is_groundhog) %>% 
  left_join(preds_list) %>% 
  left_join(preds_pct) %>% 
  arrange(-early, -n) %>% 
  mutate(region_country = paste0(region, ", ", country)) %>% 
  select(image, name, type, city, region_country, total, preds, early) %>%
  mutate(early = replace_na(early, 0)) %>% 
  gt() %>% 
  tab_header(
    title = "Non-groundhog day",
    subtitle = html("The table shows how many times animals (or things) that are not groundhogs predicted an early <span style='color:#475D41'><b>spring</b></span><br>or six more weeks of <span style='color:#81BAD1'><b>winter</b></span> on Groundhog day.")
  ) %>% 
  gt_img_circle(image, height = 46, border_color = NA) %>% 
  gt_merge_stack(name, type, font_weight = c("bold", "normal"), small_cap = FALSE, palette = c("black", "grey60")) %>% 
  gt_merge_stack(city, region_country, font_weight = c("normal", "normal"), small_cap = FALSE, palette = c("black", "grey60")) %>% 
  gt_plt_bar_pct(early, scaled = TRUE, labels = TRUE, fill = "#65885C") %>%
  gt_plt_winloss(preds, palette = c("#475D41", "#81BAD1", "gray90")) %>% 
  gt_theme_538() %>% 
  cols_label(
    image = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
    name = "",
    city = "",
    early = "% early spring",
    preds = "predictions",
    .fn = md
  ) %>% 
  cols_width(
    name ~ px(240),
    city ~ px(180),
    early ~ px(120)
  ) %>% 
  tab_source_note(source_note = "Source: groundhog-day.com · Table: Georgios Karamanis") %>% 
  gtsave_extra(here::here("2024/2024-week_05/plots/groundhogs_full.png"))
