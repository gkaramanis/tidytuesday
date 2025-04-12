library(tidyverse)
library(geofacet)
library(legendry)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

care_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv')

psych_ed_diff <- care_state %>% 
  filter(measure_id %in% c("OP_18b", "OP_18c")) %>% 
  select(state, measure_name, score) %>%
  pivot_wider(names_from = measure_name, values_from = score) %>%
  rename(
    "ED_median" = 2,
    "ED_psych_median" = 3
  ) %>%
  mutate(
    time_diff = ED_psych_median - ED_median,
    time_diff_pct = ED_psych_median / ED_median,
    time_diff_hm = seconds_to_period(time_diff * 60),
    time_diff_h = hour(time_diff_hm),
    time_diff_m = minute(time_diff_hm),
    time_diff_label = case_when(
      time_diff_h > 0 & time_diff_m > 0~ paste0(time_diff_h, "h ", time_diff_m, "m"),
      time_diff_h > 0 & time_diff_m == 0 ~ paste0(time_diff_h, "h"),
      TRUE ~ paste0(time_diff_m, "m")
    ),
    time_diff_label = paste0("+", time_diff_label)
  )
  
range(psych_ed_diff$ED_median, na.rm = TRUE)
range(psych_ed_diff$ED_psych_median, na.rm = TRUE)
range(psych_ed_diff$time_diff_pct, na.rm = TRUE)

pal <- colorspace::sequential_hcl(palette = "SunsetDark", n = 5, rev = TRUE)
dark_teal <- "#234154"

f1 <- "Poppins"
f2 <- "Sofia Sans Extra Condensed"

ggplot(psych_ed_diff, aes(x = time_diff, y = state, label = time_diff_label, fill = time_diff)) +
  geom_col() +
  geom_segment(aes(x = 0, xend = max(psych_ed_diff$time_diff, na.rm = TRUE), y = 1.9), linewidth = 1, color = dark_teal) +
  geom_text(aes(x = 0, y = 1.8, label = state), hjust = 0, vjust = 1, family = f1, fontface = "bold", color = dark_teal) +
  geom_text(data = . %>% filter(time_diff >= 90), hjust = 1, nudge_x = -5, color = "white", family = f2, fontface = "bold") +
  geom_text(data = . %>% filter(time_diff < 90), hjust = 0, nudge_x = 8, color = "black", family = f2,  fontface = "bold") +
  scale_fill_gradientn(colours = pal,
                       breaks = c(0, 60, 120, 180), 
                       labels = c("0", "+1h", "+2h", "+3h"),
                       name = "Distribution of additional time spent",
                       guide = compose_sandwich(
                         middle = gizmo_histogram(just = 0),
                         text = "axis_base")
  ) +
  coord_cartesian(clip = "off") +
  facet_geo(vars(state), scales = "free_y", grid = us_state_with_DC_PR_grid2) +
  labs(
    title = "Mental health patients spend up to 2x time in the ED",
    subtitle = str_wrap("Mental health patients face additional wait times of 30 minutes to 3.5 hours on top of non-psychiatric ED visits. The most severe delays are in Delaware (+3h 21m, total 7h), Maryland (+3h, total 7h 11m), and Puerto Rico (+3h 20m, total 8h 22m).", 126),
    caption = "Source:  Centers for Medicare and Medicaid Services · Graphic: Georgios Karamanis",
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.margin = margin(10, 0, 20, 0),
    plot.background = element_rect(fill = "#FDFBF7", color = NA),
    strip.text = element_blank(),
    panel.spacing.x = unit(0.8, "lines"),
    panel.spacing.y = unit(1, "line"),
    plot.margin = margin(10, 15, 10, 15),
    plot.title = element_text(size = 18, face = "bold", color = dark_teal),
    plot.subtitle = element_text(margin = margin(4, 0, 15, 0), lineheight = 1, size = 13),
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(20, 0, 0, 0))
  )
  
record_polaroid()
