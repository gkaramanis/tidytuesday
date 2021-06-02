library(tidyverse)
library(glue)

viewers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/viewers.csv')

viewers %>% 
  select(season, episode, title, share_18_49) %>% 
  slice_max(order_by = share_18_49, n = 10, with_ties = TRUE) %>% 
  mutate(
    pct = round(share_18_49 / 10),
    filled = str_dup("■", pct),
    empty = str_dup("□", 10 - pct),
    text = glue(
      "S{season}E{str_pad(episode, 2, pad = '0')} {title}
      {filled}{empty} {share_18_49}%"
    )
  ) %>% 
  pull(text)

spark_chr <- "▁▂▃▄▅▆▇█"

viewers %>% 
  select(season, episode, title, rating_18_49) %>%
  arrange(season, episode) %>% 
  mutate(
    pos = round(rating_18_49 * 8/10),
    chr = substring(spark_chr, pos, pos),
    chr = replace_na(chr, " ")
    ) %>% 
  group_by(season) %>% 
  summarise(sparkline = paste0(chr, collapse = "")) %>% 
  ungroup() %>% 
  mutate(text = glue("S{str_pad(season, 2, pad = '0')} {sparkline}")) %>% 
  pull(text)

# ggsave(here::here("temp", paste0("survivor-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

