library(tidyverse)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

issues_freq <- roll_calls %>% 
  left_join(issues) %>% 
  filter(!is.na(issue)) %>% 
  # filter(short_name == "hr") %>% 
  # left_join(unvotes) %>% 
  mutate(
    year = year(date),
    decade = year %/% 10 * 10
    ) %>% 
  group_by(decade, issue) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  group_by(issue) %>% 
  mutate(
    issue = toupper(issue),
    n = 1:n(),
    char = substr(issue, n, n),
    char_end = substr(issue, 9, nchar(issue)),
    char = if_else(n < 9 & char == " ", "~", char),
    y = 8 - cur_group_id()
  ) %>% 
  ungroup() %>% 
  group_by(decade) %>% 
  mutate(top_freq = if_else(freq == max(freq), TRUE, FALSE)) %>% 
  ungroup()


ggplot(issues_freq) +
  geom_segment(aes(x = 1935, xend = 2015,
                                y = y, yend = y),
               color = "pink", alpha = 0.5, stat = "unique") +
  geom_text(aes(decade, y + freq, label = char, fontface = if_else(top_freq, "bold", "plain")), family = "IBM Plex Mono", size = 7, vjust = 0) +
  geom_text(aes(2015, y, label = char_end), stat = "unique", hjust = 0, family = "IBM Plex Mono", size = 7, vjust = 0) +
  scale_x_continuous(limits = c(1935, 2160), breaks = c(seq(1940, 2010, by = 20))) +
  coord_cartesian(clip = "off") +
  labs(
    title = str_wrap("United Nations General Assembly Voting. Vertical shift of characters shows the relative frequency of issues voted on by decade. Bold indicates the most voted-on issue for each decade.", 84),
    # subtitle = "Vertical shift of characters shows the relative frequency of issues voted on by decade",
    caption = "Source: Harvard Dataverse · Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(family = "IBM Plex Mono", size = 8, color = "grey20"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 11, margin = margin(0, 0, 25, 0), family = "IBM Plex Mono"),
    plot.caption = element_text(family = "IBM Plex Mono", size = 8)
  ) 

ggsave(here::here("temp", paste0("unvotes-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 8, height = 7)

