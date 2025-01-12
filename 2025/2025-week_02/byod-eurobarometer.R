library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

read_and_clean <- function(x) {
  household_raw <- rio::import(x, sheet = "QD4_3", skip = 8) 
  
  household_raw %>% 
    select(1:5) %>% 
    select(-1) %>% 
    mutate(country_code = colnames(.[2])) %>% 
    select(country_code, "response" = 1, "total" = 2, "men" = 3, "women" = 4) %>% 
    filter(response %in% c("Total 'Agree'", "Total 'Disagree'")) %>% 
    mutate(across(total:women, as.numeric))
}

household <- list.files(path = here::here("2025/2025-week_02/special-eurobarometer-data/"), full.names = TRUE, pattern = "^ST.+xlsx$") %>% 
  map_df(~read_and_clean(.))

household_plot <- household %>% 
  filter(response == "Total 'Agree'") %>% 
  mutate(
    difference = women - men,
    men = if_else(difference == 0, men + 0.00075, men),
    women = if_else(difference == 0, women - 0.00075, women)
    ) %>% 
  pivot_longer(men:women, names_to = "gender", values_to = "ratio") %>%
  mutate(
    country = countrycode::countrycode(country_code, origin = "eurostat", destination = "country.name"),
    country = fct_reorder(country, total)
  ) %>% 
  group_by(country) %>% 
  mutate(
    mid_ratio = mean(ratio),
    highest_ratio = if_else(difference > 0, "women", "men")
  ) %>% 
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(household_plot, aes(ratio, country)) +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_tile(aes(x = mid_ratio, width = abs(difference) - 0.0125, fill = highest_ratio), height  = 0.15, alpha = 0.6, stat = "unique", show.legend = FALSE) +
  geom_point(aes(fill = gender), alpha = 0.8, size = 5, shape = 21, color = "grey99") +
  shadowtext::geom_shadowtext(data = . %>% filter(gender != highest_ratio), aes(label = country), stat = "unique", hjust = 1, nudge_x = -0.015, family = f1b, color = "grey30", bg.color = "grey99", vjust = 0.45) +
  scale_x_continuous(labels = scales::percent_format(), breaks = seq(0.2, 1, 0.1), minor_breaks = seq(0.15, 1, 0.05), limits = c(0.1, 0.75)) +
  scale_fill_manual(values = c("#4CAF50", "#9C27B0"), guide = guide_legend(reverse = TRUE)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "'Overall, men are naturally less competent than women to perform household tasks'",
    subtitle = str_wrap("In 10 EU countries, a majority of both men and women believe men are naturally less competent at household tasks. Hungary has the highest share of people who hold this view, while Denmark has the lowest. In Hungary and Slovakia, both genders share this view equally. In Czechia and Latvia, there’s a 10-percentage-point gap favoring women’s belief. Finland shows the largest difference overall, with 28% of men vs. 16% of women holding this view.", 120),
    caption = "Source: Special Eurobarometer 545 - Gender Stereotypes (2024) · Graphic: Georgios Karamanis",
  color = "",
  fill = ""
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 10),
    axis.text.y = element_blank(),
    plot.title = element_text(face = "bold", family = f2),
    plot.subtitle = element_text(lineheight = 1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0)),
    plot.margin = margin(15, 15, 10, 15)
  )

