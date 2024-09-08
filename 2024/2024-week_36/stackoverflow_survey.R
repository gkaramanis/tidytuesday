library(tidyverse)
library(ggstats)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

qname_levels_single_response_crosswalk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-03/qname_levels_single_response_crosswalk.csv')

stackoverflow_survey_questions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-03/stackoverflow_survey_questions.csv')

stackoverflow_survey_single_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-03/stackoverflow_survey_single_response.csv')

qfilter <- function(q) {
  qname_levels_single_response_crosswalk %>% 
    filter(qname == q) %>% 
    select(level, label) %>% 
    rename_with(~q, level) %>% 
    rename_with(~paste0(q, "_label"), label)
}

age_levels <- c("Under 18 years old",
                "18-24 years old",
                "25-34 years old",
                "35-44 years old",
                "45-54 years old",
                "55-64 years old",
                "65 years or older",
                "Prefer not to say"
                ) %>% 
  str_remove(., " years( old)*")

ai_sent_levels <- c("Very unfavorable",
                    "Unfavorable",
                    "Indifferent/Unsure",
                    "Favorable",
                    "Very favorable")


so <- stackoverflow_survey_single_response %>% 
  left_join(qfilter("age")) %>% 
  left_join(qfilter("main_branch")) %>% 
  left_join(qfilter("ai_sent")) %>% 
  mutate(age_label = str_remove(age_label, " years( old)*")) %>% 
  # filter(ai_sent_label != "Unsure") %>%
  mutate(ai_sent_label = case_when(
    ai_sent_label == "Indifferent" | ai_sent_label == "Unsure" ~ "Indifferent/Unsure",
    TRUE ~ ai_sent_label
  )) %>% 
  mutate(
    age_label = fct_relevel(age_label, age_levels),
    ai_sent_label = fct_relevel(ai_sent_label, ai_sent_levels)
  ) %>% 
  count(age_label, main_branch_label, ai_sent_label)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

gglikert(so, ai_sent_label, y = age_label, facet_cols = vars(main_branch_label), facet_label_wrap = 34, weights = n, add_labels = FALSE, totals_fontface = "plain", totals_size = 3, width = 0.5) +
  geom_vline(xintercept = 0, linewidth = 0.15, linetype = "dashed") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "How favorable is your stance on using AI tools as part of your development workflow?",
    subtitle = str_wrap("The 2024 Stack Overflow Developer Survey, with over 65,000 respondents, featured questions across seven key sections, including AI. The chart summarizes these AI-specific responses by age group and professional coding level. It displays aggregated positive and negative stances on a Likert scale, with Indifferent and Unsure responses combined. Responses were predominantly positive and fairly consistent across professional coding levels and age groups, except for those who chose not to state their age. However, this difference could be due to the small sample size in the latter group.", 140),
    caption = "Source: 2024 Stack Overflow Annual Developer Survey · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "#833A6F", face = "bold"),
    axis.text.x = element_blank(),
    strip.text = element_text(vjust = 0, color = "#833A6F", face = "bold"),
    panel.spacing.x = unit(1, "line"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(),
    plot.margin = margin(10, 10, 10, 10)
  )
