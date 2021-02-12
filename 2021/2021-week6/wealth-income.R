library(tidyverse)

income_distribution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')
  
income_dist <- income_distribution %>% 
  filter(str_detect(race, "Alone$")) %>% 
  filter(year == 1990 | year == 2019) %>%
  mutate(
    income_bracket = fct_inorder(income_bracket),
    income_bracket = fct_recode(income_bracket, 
                                "Under $15K" = "Under $15,000",
                                "$15K-25K" = "$15,000 to $24,999",
                                "$25K-35K" = "$25,000 to $34,999",
                                "$35K-50K" = "$35,000 to $49,999",
                                "$50K-75K" = "$50,000 to $74,999",
                                "$75K-100K" = "$75,000 to $99,999",
                                "$100K-150K" = "$100,000 to $149,999",
                                "$150K-200K" = "$150,000 to $199,999",
                                "$200K and over" = "$200,000 and over"),
    race = str_remove(race, " Alone")
    ) 

income_change <- income_dist %>% 
  pivot_wider(id_cols = c(race, income_bracket), names_from = year, values_from = income_distribution, names_prefix = "year_") %>% 
  mutate(change = year_2019 - year_1990)

income_mm <- income_dist %>% 
  distinct(race, year, income_mean, income_mean_moe, income_median, income_med_moe) %>% 
  mutate(
    mean_bracket = as.factor(findInterval(income_mean, c(15000, 25000, 35000, 50000, 75000, 100000, 150000, 200000))),
    median_bracket = as.factor(findInterval(income_median, c(15000, 25000, 35000, 50000, 75000, 100000, 150000, 200000))),
    mean_bracket = fct_recode(mean_bracket,
      # "Under $15K" = "0",
      # "$15K-25K" = "1",
      # "$25K-35K" = "2",
      "$35K-50K" = "3",
      "$50K-75K" = "4",
      "$75K-100K" = "5",
      "$100K-150K" = "6",
      # "$150K-200K" = "7",
      # "$200K and over" = "8"
      ),
    median_bracket = fct_recode(median_bracket,
      # "Under $15K" = "0",
      # "$15K-25K" = "1",
      # "$25K-35K" = "2",
      "$35K-50K" = "3",
      "$50K-75K" = "4",
      "$75K-100K" = "5",
      # "$100K-150K" = "6",
      # "$150K-200K" = "7",
      # "$200K and over" = "8"
      ),
    ) %>% 
  pivot_wider(id_cols = c(race), names_from = year, values_from = c(mean_bracket, median_bracket))

f1 = "Domaine Display"
f1m = "Domaine Display Medium"
f1b = "Domaine Display Bold"
f2 = "Proxima Nova"

ggplot(data = income_change) +
  # Income changes
  geom_segment(aes(x = year_1990, y = income_bracket,
                   xend = year_2019, yend = income_bracket,
                   color = if_else(change > 0, "#146B3A", "#EA4630")),
               arrow = arrow(length = unit(0.04, "npc")), size = 1) +
  geom_text(aes(x = year_1990, y = income_bracket,
                label = if_else(abs(change) > 4, year_1990, NULL)), nudge_y = -0.275, size = 3, family = f1, color = "grey10") +
  geom_text(aes(x = year_2019, y = income_bracket,
                label = year_2019, color = if_else(change > 0, "#146B3A", "#EA4630")),
            nudge_y = -0.275, size = 3, family = f1m) +
  # Mean and median arrows
  # geom_segment(data = income_mm, aes(x = 24, y = mean_bracket_1990,
  #                  xend = 24, yend = mean_bracket_2019),
  #              arrow = arrow(length = unit(0.04, "npc")), size = 1, color = "grey60") +
  geom_segment(data = income_mm, aes(x = 24, y = median_bracket_1990,
                   xend = 24, yend = median_bracket_2019),
               arrow = arrow(length = unit(0.04, "npc")), size = 1, color = "grey60") +
  # geom_text(data = income_mm, aes(x = 24, y = mean_bracket_1990, label = "mean"), angle = 90, nudge_y = -0.25, hjust = 1, color = "grey60", family = f1m) +
  geom_text(data = income_mm, aes(x = 24, y = median_bracket_1990, label = "median"), angle = 90, nudge_y = -0.25, hjust = 1, color = "grey60", family = f1m) +
  scale_color_identity() +
  facet_wrap(vars(race), nrow = 1) +
  labs(
    title = "Change in income distribution for Asians, blacks and whites",
    subtitle = str_wrap("The arrows show the change between 1990 and 2019. Compared by median, Asians rank as the highest earning racial and ethnic group in 2019 but the percentage of highest-income Asians has almost tripled since 1990. The percentage of lowest-income blacks fell from 24.3% to 17.2% but is by far the biggest percentage of the three groups.", 114),
    caption = "Source: Urban Institute & US Census | Graphic: Georgios Karamanis",
    x = "Percentage",
    y = NULL
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f0f0f1", color = NA),
    plot.margin = margin(20, 20, 10, 20),
    panel.grid.major.x = element_line(color = "#b9babd", size = 0.15),
    panel.grid.minor.x = element_line(color = "#b9babd", size = 0.08),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.text.y = element_text(family = f1m),
    strip.text = element_text(family = f1b, size = 12),
    panel.spacing.x = unit(3, "lines"),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0), family = f2),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), size = 8)
  ) +
  ggsave(here::here("temp", paste0("wealth-income-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 10, height = 8)
