library(tidyverse)
library(emoji)
library(ggtext)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 9, units = "in", dpi = 320)

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')

language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv') %>% 
  group_by(language_iso_code) %>% 
  summarise(country = list(country)) %>% 
  ungroup()

afri_emoji <- afrisenti %>% 
  add_count(label, language_iso_code, name = "total_tweets") %>% 
  mutate(
    id = row_number(),
    emoji = str_extract_all(tweet, emoji_rx)
    ) %>% 
  filter(lengths(emoji) > 0) %>% 
  unnest(emoji) %>% 
  count(language_iso_code, label, emoji, total_tweets) %>% 
  mutate(emoji_per_tweet = n / total_tweets) %>% 
  group_by(language_iso_code, label) %>% 
  slice_max(order_by = emoji_per_tweet, n = 5, with_ties = FALSE) %>% 
  arrange(label, -n) %>% 
  mutate(
    rank = row_number(),
    x = case_when(
      label == "positive" ~ emoji_per_tweet,
      label == "negative" ~ -emoji_per_tweet,
      TRUE ~ NA
    )
    ) %>% 
  ungroup() %>% 
  left_join(languages) %>%  
  left_join(language_countries) %>% 
  rowwise() %>% 
  mutate(
    country_label = paste0("<span style='font-size:15px'>**", language, "**</span><br>", paste(country, collapse = ", ")),
    emoji = str_replace(emoji, "♂", "x"),
    emoji = str_replace(emoji, "♀", "-")
  ) %>% 
  filter(label != "neutral")

f1 <- "Outfit"
f2 <- "Apple Color Emoji"
f3 <- "Fauna One"

afri_emoji %>% 
  filter(!language_iso_code %in% c("amh", "yor")) %>% 
ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf), fill = alpha("#E06943", 0.55), stat = "unique") +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf), fill = alpha("#4B9EA8", 0.55), stat = "unique") +
  geom_text(data = . %>% filter(label == "negative"), aes(x = -0.85, y = 4.9, label = ifelse(language_iso_code == "arq", paste(total_tweets, "tweets"), total_tweets)), hjust = 0, color = "#E06943", family = f1, size = 3, stat = "unique") +
  geom_text(data = . %>% filter(label == "positive"), aes(x = 0.85, y = 4.9, label = ifelse(language_iso_code == "arq", paste(total_tweets, "tweets"), total_tweets)), hjust = 1, color = "#4B9EA8", family = f1, size = 3, stat = "unique") +
  geom_vline(xintercept = 0, color = "white") +
  geom_text(aes(x, rank, label = emoji), alpha = 0.5, family = f2) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.7, 0.7, 0.2), labels = round(abs(seq(-0.7, 0.7, 0.2)), 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(country_label)) +
  labs(
    title = "Emoji in AfriSenti",
    subtitle = "Number of times each emoji was used by total number of tweets. Showing the top 5 emoji for <span style='color:#E06943'>**negative**</span> and <span style='color:#4B9EA8'>**positive**</span> tweets.<br>Amharic and Yorùbá had a very low emoji usage and are not shown.",
    caption = "Source: AfriSenti: Sentiment Analysis dataset for 14 African languages · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    strip.text = element_markdown(lineheight = 1.2, color = "grey30"),
    axis.text.x = element_text(color = "grey60"),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold", family = f3),
    plot.subtitle = element_markdown(lineheight = 1.2, hjust = 0.5, color = "grey40"),
    plot.caption = element_text(color = "grey40", margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 10, 0, 10)
  )
  
