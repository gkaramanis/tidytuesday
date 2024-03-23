library(tidyverse)
library(ggh4x)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8.5, units = "in", dpi = 320)

mutant_moneyball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv') %>% 
  janitor::clean_names()

mm_gt <- mutant_moneyball %>% 
  mutate(name = sub('([[:upper:]])', ' \\1', member)) %>% 
  mutate(name = gsub('(^[a-z])', '\\U\\1', name, perl = TRUE)) %>%
  select(name, 
         total_issues60s,
         total_value60s_ebay,
         total_issues70s,
         total_value70s_ebay,
         total_issues80s,
         total_value80s_ebay,
         total_issues90s,
         total_value90s_ebay) %>% 
  pivot_longer(2:last_col(), names_to = "metric", values_to = "value") %>% 
  mutate(decade = paste0(parse_number(metric), "s"))

label_rename <- function(x) {
  if_else(str_detect(x, "issue"), "Issues", "eBay value")
}

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(mm_gt, aes(value, name, label = value)) +
  # Alternate row color
  ggforestplot::geom_stripes(odd = "#D8DEE355") +
  # eBay bars
  geom_col(data = . %>% filter(str_detect(metric, "ebay")), aes(fill = value), width = 0.6) +
  # eBay vertical line at 0
  geom_tile(data = . %>% filter(str_detect(metric, "ebay")), aes(x = 0), width = 100) +
  # eBay labels
  geom_text(data = . %>% filter(str_detect(metric, "ebay") & between(value, 1, 10000)), aes(label = scales::dollar(value)), hjust = 0, nudge_x = 1000, family = f1) +
  # eBay labels
  geom_text(data = . %>% filter(str_detect(metric, "ebay") & value >= 10000), aes(label = scales::dollar(value)), hjust = 1, nudge_x = -1000, color = "white", family = f1) +
  # Issue labels
  geom_text(data = . %>% filter(str_detect(metric, "issue") & value > 0), aes(x = 14000), hjust = 1, family = f1) +
  # Scales and stuff
  MetBrewer::scale_fill_met_c("Hokusai1", direction = -1) +
  ggh4x::facet_nested_wrap(vars(decade, metric), nrow = 1, labeller = labeller(metric = label_rename)) +
  labs(
    # title = "X-Men Mutant Moneyball",
    caption = "Total number of appearances and total value of issues for each X-Men member per decade, as reflected by eBay sales tagged as 'very good' condition\nSource: Mutant Moneyball · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.y = element_text(hjust = 1, size = 12, margin = margin(0, 5, 0, 0), family = f1b, face = "bold"),
    ggh4x.facet.nestline = element_line(),
    panel.spacing.x = unit(0, "cm"),
    strip.text = element_text(size = 12, margin = margin(3, 0, 3, 0), face = "bold", family = f1b),
    plot.margin = margin(20, 20, 10, 20),
    plot.title.position = "plot"
  )
  
