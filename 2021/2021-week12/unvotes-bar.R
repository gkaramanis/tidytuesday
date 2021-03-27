library(tidyverse)
library(vhs)

issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

issues_count <- issues %>% 
  count(issue) %>% 
  mutate(issue = fct_rev(issue))

pal <- vhs("recoton")
  
ggplot(issues_count) +
  geom_col(aes(x = n - 240, y = issue, fill = issue)) +
  # Letters
  geom_text(aes(x = n, y = issue, label = str_sub(issue, 1, 1), color = issue),
            stat = "unique", hjust = 1,
            family = "Owners", size = 48, nudge_x = 18) +
  # Issue labels
  geom_text(aes(x = 50, y = issue, label = toupper(issue)), stat = "unique",
            hjust = 0, color = "grey97", nudge_y = -0.325, 
            family = "Proxima Nova Bold", size = 3.5) +
  # Issue Number
  geom_text(aes(x = 40, y = issue, label = n), stat = "unique",
            hjust = 0, color = "grey97", size = 28, 
            family = "Proxima Nova Bold", nudge_y = 0.04) +
  # Hide the ugly stuff
  geom_tile(aes(x = n/2, y = issue, width = n, height = 0.925),
            fill = NA, color = "grey25", size = 3) +
  # Scales, theme, etc.
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "UN roll call votes by issue",
    caption = "Source: Harvard's Dataverse · Graphic: Georgios Karamanis") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey25", color = NA),
    plot.title = element_text(margin = margin(0, 0, 10, 0), family = "Produkt Medium", color = "grey97", size = 30, hjust = 0.5),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 1, family = "Produkt", color = "grey70", size = 7),
    plot.margin = margin(20, 20, 10, 20)
  ) +
  # Save
  ggsave(here::here("temp", paste0("unvotes-bar-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 6, height = 9.5)
