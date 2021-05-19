library(tidyverse)
library(scales)
library(RColorBrewer)
library(colorspace)
library(cowplot)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

surv <- survey %>% 
  mutate(
    age = case_when(
      how_old_are_you == "under 18" | how_old_are_you == "18-24" ~ "under 24",
      how_old_are_you == "55-64" | how_old_are_you == "65 or over" ~ "55 or over",
      TRUE ~ how_old_are_you
    ),
    age = fct_relevel(age, c("under 24", "25-34", "35-44", "45-54", "55 or over")),
    gender = if_else(gender == "Prefer not to answer" | gender == "Other or prefer not to answer", "Other or no answer", gender)
    ) %>% 
  filter(!is.na(gender)) %>% 
  group_by(age, gender) %>%
  summarise(count = n()) %>%
  mutate(
    age_count = sum(count),
    prop = count/sum(count)
    ) %>%
  ungroup()

pal <- rev(brewer.pal(4, "Set2"))

f1m <- "Fira Sans Compressed Medium"
f2 <- "Fira Sans Condensed"
f2m <- "Fira Sans Condensed Medium"
f3 <- "Proxima Nova"
f3b <- "Proxima Nova Bold"
f4 <- "Source Serif Pro"
f4b <- "Source Serif Pro Bold"

p <- ggplot(surv, aes(x = age, y = prop, width = age_count, fill = gender)) +
  # bars
  geom_bar(stat = "identity", position = "fill", colour = "grey97", size = 0.3) +
  # % labels
  geom_text(aes(label = percent(prop, accuracy = 1), size = prop, color = gender), position = position_stack(vjust = 0.5), show.legend = FALSE, family = f1m) +
  # age groups (top)
  geom_text(aes(x = age, y = 1.03, label = age), stat = "unique", family = f4, size = 4.5, color = "grey30") +
  # total number (bottom)
  geom_text(aes(x = age, y = -0.03, label = number(age_count, big.mark = " ", accuracy = 1)), stat = "unique", family = f4, size = 4.5, color = "grey30") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = darken(pal, 0.7)) +
  scale_size_continuous(range = c(1.5, 7.5)) +
  coord_cartesian(clip = "off") +
  facet_grid(~age, scales = "free_x", space = "free_x") +
  labs(
    title = "Ask a Manager Survey",
    subtitle = "Gender of participants by age group (rounded percentages, may add upp to more than 100%)",
    caption = "Source: Ask A Manager Salary Survey 2021 · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_void(base_family = f2m) +
  theme(
    legend.position = c(0.85, 1.05),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 40, 20, 40),
    plot.title = element_text(hjust = 0, family = f3b, size = 28, margin = margin(0, 0, 15, 0), color = "grey10"),
    plot.subtitle = element_text(hjust = 0, family = f4, size = 13, margin = margin(0, 0, 15, 0), color = "grey25"),
    plot.caption = element_text(hjust = 0.5, family = f4, margin = margin(10, 0, 0, 0), color = "grey45", size = 10)
  ) 

ggdraw(p) +
  draw_label("Age group", x = 0.5, y = 0.845, fontfamily = f1m) +
  draw_label("Participants", x = 0.5, y = 0.092, fontfamily = f1m) +
  ggsave(here::here("temp", paste0("survey-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 9)

# Plot as seen at
# https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2