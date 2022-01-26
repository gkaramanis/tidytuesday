library(tidyverse)
library(camcorder)
library(gghalves)
library(ggbeeswarm)
library(patchwork)
library(ggtext)
library(ggforce)
library(ggrepel)

gg_record(dir = "temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')

details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

rat_det <- ratings %>% 
  left_join(details, by = "id") %>% 
  select(id, name, minage, bayes_average, yearpublished) %>% 
  filter(bayes_average > 0) %>% 
  mutate(
    pub2000 = !is.na(yearpublished) & yearpublished >= 2000,
    facet_label = ifelse(pub2000, "Games published since 2000", "Games published until 1999")  )

f1 = "Graphik"

theme_set(theme_minimal(base_family = f1, base_size = 12))

theme_update(
      legend.position = "none",
      plot.background = element_rect(fill = "grey97", color = NA),
      axis.title = element_blank(),
      axis.text.x = element_markdown(),
      plot.title = element_text(face = "bold", size = 18)
)

p1 <- ggplot(rat_det, aes(x = minage, y = bayes_average, group = minage, color = pub2000)) +
  geom_half_point_panel(size = 0.05, transformation = position_quasirandom()) +
  # geom_half_violin(size = 0.2, scale = "count") +
  geom_half_point_panel(data = rat_det %>% mutate(minage = 28), aes(x = minage, y = bayes_average, group = minage), size = 0.05, transformation = position_quasirandom()) +
  # geom_half_violin(data = rat_det %>% mutate(minage = 28), aes(x = minage, y = bayes_average, group = minage), size = 0.2, scale = "count") +
  scale_x_continuous(limits = c(0, 29), breaks = c(seq(0, 25, 5), 28), labels = c(seq(0, 25, 5), "**All ages**")) +
  # Annotations
  geom_mark_circle(data = data.frame(minage = 25, bayes_average = 5.476, facet_label = "Games published since 2000"), aes(minage, bayes_average, label = "One game with\nminimum age 25 years"), label.fontsize = 8, color = "black", con.size = 0.25, size = 0.25, label.fontface = "plain") +
  geom_mark_circle(data = data.frame(minage = 21, bayes_average = 5.596, facet_label = "Games published until 1999"), aes(minage, bayes_average, label = "One game with\nminimum age 21 years"), label.fontsize = 8, color = "black", con.size = 0.25, size = 0.25, label.fontface = "plain", label.family = f1) +
  # Scales, etc
  scale_y_continuous(breaks = seq(4, 9, 1), limits = c(3.5, 8.5)) +
  scale_color_manual(values = c("#3E3A5D", "#EC5E2A")) +
  facet_wrap(vars(facet_label), ncol = 1) +
  labs(
    title = "Distribution of Bayesian average score by age rating (minimum age)",
    subtitle = "Most games on BoardGameGeek have a weighted average score around 5.5, but games published\nsince 2000 tend to get higher scores"
    ) +
  theme(plot.subtitle = element_text(lineheight = 1))

p2 <- ggplot(rat_det) +
  geom_histogram(aes(x = yearpublished, fill = pub2000), binwidth = 10) +
  geom_label_repel(data = rat_det %>% filter(yearpublished < 0), aes(x = yearpublished, y = 0, label = name), direction = "y", ylim = c(1000, 6000), family = f1, max.overlaps = Inf, size = 2.5, segment.size = 0.25, color = "grey20", label.padding = 0.2, label.size = NA) +
  scale_x_continuous(breaks = seq(-3500, 2000, 500), minor_breaks = seq(-3500, 2000, 100), labels = function(x) ifelse(x < 0, paste0(-x, " BC"), x)) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(values = c("#3E3A5D", "#EC5E2A")) +
  labs(subtitle = "Distribution of year of publication, all games") +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35"))
  

p3 <- ggplot(rat_det) +
  geom_histogram(aes(x = bayes_average, fill = pub2000), binwidth = 0.05) +
  scale_x_continuous(breaks = seq(3.5, 8.5, 0.5), minor_breaks = seq(3.5, 8.5, 0.1)) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(values = c("#3E3A5D", "#EC5E2A")) +
  labs(
    subtitle = "Distribution of Bayesian average score, all games",
    caption = "Source: BoardGameGeek · Graphic: Georgios Karamanis"
    ) +
  theme(
    plot.subtitle = element_text(face = "bold", color = "grey35"),
    plot.caption = element_text(color = "grey25", margin = margin(20, 0, 0, 0))
    )
  

p1 / p2 / p3 +
  plot_layout(heights = c(4, 1, 1)) &
  theme(plot.margin = margin(10, 10, 10, 10))

