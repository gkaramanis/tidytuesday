library(tidyverse)
library(ggsankey)

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

usa_all <- survey %>% 
  select(-timestamp) %>%  # remove duplicates
  distinct() %>%          # remove duplicates
  filter(startsWith(tolower(country), "u")) %>% # keep countries starting with "u"
  filter(str_detect(tolower(country), "kingdom|u.*k|uae|uganda|emirates|uruguay", negate = TRUE)) %>% # filter out other
  mutate(
    country = str_remove(country, "(-|,).+"),
    country = str_remove(country, "( gov| tomor).+"),
    lower_country = tolower(country)
  ) %>% 
  select(lower_country) %>% 
  rowwise() %>% 
  mutate(sub_chr = list(substring(lower_country, 1, 1:nchar(lower_country)))) %>%  # tokenize
  ungroup() %>% 
  unnest(c(sub_chr))

nodes <- usa_all %>% 
  # count(lower_country, sort = TRUE) %>% 
  distinct(lower_country) %>% 
  # mutate(l = str_length(lower_country)) %>%
  rowwise() %>% 
  mutate(sub_chr = list(substring(lower_country, 1, 1:nchar(lower_country)))) %>%  # tokenize
  ungroup() %>% 
  unnest(c(sub_chr)) %>% 
  mutate(l_chr = str_length(sub_chr)) %>% 
  ungroup() %>% 
  # add_count(sub_chr, l_chr) %>% 
  group_by(lower_country) %>% 
  # prepare for geom_sankey
  mutate(
    x = l_chr,
    next_x = lead(l_chr),
    node = sub_chr,
    next_node = lead(sub_chr)
  ) %>% 
  ungroup()

usa_nodes <- usa_all %>% 
  left_join(nodes)

ggplot(usa_nodes, aes(x = x, next_x = next_x, node = node, next_node = next_node, label = node)) +
  geom_sankey() +
  # geom_sankey_label() +
  theme_void() +
  theme(
    legend.position = "none"
  ) +
  ggsave("~/Desktop/missp.png", dpi = 320, width = 12, height = 7)
