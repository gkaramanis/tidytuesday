library(tidyverse)
library(tidytext)
library(ggh4x)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 11, units = "in", dpi = 320)

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')

sauces <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')

nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

sentim_lex <- function(lexicon){
  sauce_sent <- sauces %>% 
    distinct(sauce_name, scoville) %>% 
    mutate(
      level = case_when(
        scoville < 2500 ~ "Mild (<2 500 SHU)",
        between(scoville, 2500, 30000) ~ "Medium (2 500-30 000 SHU)",
        between(scoville, 30000, 100000) ~ "Hot (30 000-100 000 SHU)",
        between(scoville, 100000, 300000) ~ "Extra Hot (100 000-300 000 SHU)",
        scoville > 300000 ~ "Extremely Hot (>300 000 SHU)",
      ),
      level = fct_inorder(level)
    ) %>% 
    unnest_tokens(word, sauce_name, drop = FALSE) %>% 
    inner_join(lexicon)
  
  sauce_f <- sauce_sent %>% 
    group_by(sentiment, level) %>% 
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)) %>% 
    ungroup()
  }

sauce_nrc <- sentim_lex(nrc) %>% 
  mutate(lexicon = "nrc")

sauce_bing <- sentim_lex(bing) %>% 
  mutate(
    lexicon = "bing",
    freq = -freq
    )

sauce_nrc_bing <- bind_rows(sauce_nrc, sauce_bing)

f1 <- "Outfit"

ggplot(sauce_nrc_bing) +
  geom_col(aes(freq, sentiment, fill = abs(freq))) +
  geom_text(data = . %>% filter(lexicon == "nrc"), aes(-0.005, sentiment, label = sentiment), hjust = 1, family = f1, size = 3.5) +
  geom_text(data = . %>% filter(lexicon == "bing"), aes(0.005, sentiment, label = sentiment), hjust = 0, family = f1, size = 3.5) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  scale_y_discrete(limits = rev) +
  facet_grid2(rows = vars(level), 
              cols = vars(toupper(lexicon)), 
              scales = "free_x",
              space = "free",
              strip = strip_split(c("top", "top"), text_x = elem_list_text(color = c("red3", "grey60")), by_layer_x = TRUE)
              ) +
  scale_x_facet(PANEL == 1, limits = c(-0.4, 0), labels = abs, expand = c(0.07, 0.05), breaks = seq(-0.4, 0, 0.1), minor_breaks = c(-0.35, -0.25, -0.15, -0.05)) +
  scale_x_facet(PANEL == 2, limits = c(0, 0.4), labels = abs, expand = c(0.07, 0.05), breaks = seq(0, 0.5, 0.1), minor_breaks = c(0.35, 0.25, 0.15, 0.05)) +
  labs(
    title = "Sentiment of the sauce names in Hot Ones",
    subtitle = "According to the Bing and NRC lexicons",
    caption = "Source: Wikipedia · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(10, 10, 10, 10)
  )
