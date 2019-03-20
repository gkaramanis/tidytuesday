library(ggplot2)
library(dplyr)

rawData <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

byStateAndRace <- rawData %>%
  group_by(location, state, driver_race) %>%
  summarize(meanRate = mean(citation_rate_speeding_stops))

ggplot(subset(byStateAndRace), aes(location, meanRate)) +
  geom_point(aes(colour = factor(driver_race)),
              size = 1, alpha = 0.5) +
  facet_grid(.~state, scales = "free") +
  coord_cartesian(ylim = c(0, 1), expand = TRUE) +
  scale_size(guide = 'none') +
  labs(title =  "Citation rates for speeding stops",
       subtitle = "as reported by counties or districts",
       y = "citation rate",
       caption = toupper("Source: arXiv:1706.05678")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme_dark() +
  theme(
    panel.background = element_rect(fill = "grey20"),
    plot.margin=unit(c(1,1.2,1,0.8),"cm"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.text = element_text(size = 11, color = "white"),
    legend.background = element_rect(fill = "grey20"),
    plot.caption = element_text(color = "gray50"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )
