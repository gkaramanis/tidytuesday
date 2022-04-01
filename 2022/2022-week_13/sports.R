library(tidyverse)
library(geofacet)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

sports_total <- sports %>% 
  mutate(
    sports_bf = case_when(
      sports == "Basketball" ~ sports,
      sports == "Football" ~ sports,
      TRUE ~ "Other"
    )
    ) %>% 
  group_by(year, state_cd, sports_bf) %>% 
  summarise(total_exp_sport = sum(total_exp_menwomen, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(year, state_cd) %>% 
  mutate(
    total_exp_all = sum(total_exp_sport),
    perc_exp = total_exp_sport / total_exp_all * 100
    ) %>% 
  ungroup() %>% 
  filter(!is.na(state_cd))

f1 <- "Futura"
f2 <- "Fira Sans Compressed"

ggplot(sports_total) +
  geom_area(aes(year, perc_exp, fill = sports_bf), size = 0.25) +
  geom_hline(yintercept = 50, size = 0.1, alpha = 0.7) +
  scale_fill_manual(values = c(colorspace::desaturate("darkorange", 0.4), "tan", "grey52"), name = NULL) +
  scale_y_continuous(breaks = 50, labels = "50%") +
  scale_x_continuous(breaks = c(2015, 2017, 2019), labels = c("2015", "'17", "'19")) +
  facet_geo(vars(state_cd)) +
  labs(
    title = "College spending on basketball and football vs other sports",
    subtitle = "Percentage of total expeditures for men and women by state",
    caption = "Source: Equity in Athletics Data Analysis · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    axis.title = element_blank(),
    axis.text.x = element_text(family = f2),
    panel.spacing.x = unit(1.2, "lines"),
    axis.ticks = element_line(size = 0.2),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), hjust = 0.5, color = "grey60")
  )


ggsave(here::here("temp", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 8, width = 11)
