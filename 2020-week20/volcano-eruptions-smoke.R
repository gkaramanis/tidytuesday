library(tidyverse)
library(lubridate)
library(ggforce)

eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

smoke <- eruptions %>% 
  filter(eruption_category == "Confirmed Eruption") %>% 
  filter(!is.na(end_year)) %>% 
  # filter(start_year > 2000) %>% 
  mutate(
    eruption_start = decimal_date(ymd(paste(start_year, start_month, start_day, sep = "-"))),
    eruption_end = decimal_date(ymd(paste(end_year, end_month, end_day, sep = "-"))),
    r = (eruption_end - eruption_start) / 2,
    x0 = eruption_start + r
  ) %>% 
  group_by(volcano_name) %>% 
  mutate(eruptions_n = n()) %>% 
  ungroup %>% 
  filter(eruptions_n > 30)

ggplot(smoke) +
  geom_segment(aes(y = eruption_start, yend = eruption_end, x = volcano_name, xend = volcano_name, size = r)) +
  # coord_fixed() +
  theme_minimal() +
  ggsave(here::here("2020-week20", "plots", "temp", paste0("volcano-erruptions-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
