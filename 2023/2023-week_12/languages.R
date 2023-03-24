library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

metrics <- languages %>% 
  select(pldb_id, title, number_of_users, wikipedia_daily_page_views)
  
r_rel <- languages %>% 
  select(pldb_id, title, wikipedia_related) %>%
  separate_rows(wikipedia_related, sep = " ") %>%
  filter(pldb_id == "r" | wikipedia_related == "r") %>%
  filter(wikipedia_related %in% languages$pldb_id) %>%
  left_join(metrics) %>%
  select(-wikipedia_related) %>% 
  distinct() %>% 
  mutate(label = fct_lump_n(title, number_of_users, n = 10)) %>% 
  group_by(label) %>% 
  summarise(
    pldb_id = list(pldb_id),
    title = paste(title, collapse = ", "),
    users = sum(number_of_users),
    views = sum(wikipedia_daily_page_views)
  ) %>% 
  ungroup() %>% 
  mutate(
    label = fct_reorder(label, -users),
    label = fct_relevel(label, "Other", after = Inf)
    ) %>% 
  arrange(label) %>% 
  mutate(
    y = views/2 + cumsum(lag(views, default = 0)),
    x = users/2,
    h = views,
    w = users
  ) 

f1 <- "Outfit"
f2 <- "Source Serif Pro"

# R and related languages
ggplot(r_rel) +
  geom_tile(aes(x = x, y = y, width = w, height = h, fill = label == "R"), color = "grey99", size = 0.5) +
  geom_text(data = r_rel %>% filter(users > 2e5), aes(x = x + w/2, y = y, label = title, size = users), family = f1, hjust = 1, nudge_x = -2e4, color = "white", fontface = "bold") +
  geom_text(data = r_rel %>% filter(users < 2e5), aes(x = x + w/2, y = y, label = str_wrap(title, 50), size = users), family = f1, hjust = 0, nudge_x = 2e4, color = "grey30", fontface = "bold") +
  annotate("text", x = 1.15e6, y = 4e3, label = "Bar heights are proportional\nto each language's\ndaily Wikipedia page views", family = f1, hjust = 0, lineheight = 1, color = "grey50") +
  scale_size_continuous(range = c(3, 6)) +
  scale_fill_manual(values = c("grey75", "#276DC2")) +
  scale_x_continuous(labels = scales::label_number_si()) +
  labs(
    x = "Users",
    title = "Number of users and Wikipedia page views for R and its related  (according to Wikipedia) languages",
    caption = "Source: Programming Language DataBase · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", margin = margin(0, 0, 20, 0), family = f2, size = 16),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  ) 
  
# Top 20 languages
top_20 <- languages %>% 
  select(pldb_id, title, rank = language_rank, users = number_of_users, views = wikipedia_daily_page_views) %>% 
  filter(rank < 20) %>% 
  arrange(-users) %>% 
  mutate(
    y = views/2 + cumsum(lag(views, default = 0)),
    x = users/2,
    h = views,
    w = users
  )

ggplot(top_20) +
  geom_tile(aes(x = x, y = y, width = w, height = h, fill = title == "R"), color = "grey99", size = 0.5) +
  geom_text(data = top_20 %>% filter(users > 2e5), aes(x = x + w/2, y = y, label = title, size = users), family = f1, hjust = 1, nudge_x = -3e4, color = "white", fontface = "bold") +
  geom_text(data = top_20 %>% filter(users < 2e5), aes(x = x + w/2, y = y, label = title, size = users), family = f1, hjust = 0, nudge_x = 3e4, color = "grey30", fontface = "bold") +
  annotate("text", x = 3e6, y = 3.4e4, label = "Bar heights are proportional\nto each language's\ndaily Wikipedia page views", family = f1, hjust = 0, lineheight = 1, color = "grey50") +
  scale_size_continuous(range = c(2, 6)) +
  scale_fill_manual(values = c("grey70", "#276DC2")) +
  scale_x_continuous(labels = scales::label_number_si()) +
  labs(
    x = "Users",
    title = "Number of users and Wikipedia page views for the top 20 programming languages",
    caption = "Source: Programming Language DataBase · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", margin = margin(0, 0, 20, 0), family = f2, size = 16),
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  ) 
