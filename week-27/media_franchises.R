library(tidyverse)
library(ggimage)
library(here)
library(ggrepel)
library(cowplot)

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")
coin_img <- here("week-27", "img", "coin.png")

top_games <- media_franchises %>%
  filter(original_media == "Video game" &
           revenue_category == "Video Games/Games") %>%
  distinct() %>% 
  top_n(10, revenue) %>% 
  mutate(
    # franchise = str_replace_all(franchise, c("é" = "e", "&" = "AND")),
    franchise = str_replace_all(franchise, " (?=\\w)", "\n"),
    coins = map2(0, revenue, seq, by = 1),
    # year_created = case_when(
    #   franchise == "Final Fantasy" ~ 1987.7,
    #   T ~ year_created
    # )
    ) %>%
  arrange(year_created) %>%
  unnest(coins) 

# top_games_uniq <- top_games %>% 
#   distinct(franchise, revenue)

## original total revenue by all media types, not just the games themselves!
rev_plot <- ggplot(top_games, aes(coins,
              factor(fct_reorder(franchise, year_created, .desc = TRUE)))) +
  geom_image(aes(image = coin_img), size = 0.03, asp = 0.8) +
  scale_x_continuous(labels = c("0", "10", "20", "30", ""),
                     limits =  c(0, 40)) +
  labs(
  #   title = "Top 10 video games with the higher revenue",
  #   subtitle = "shown only revenue of games",
  #   caption = "Source: Wikipedia | Graphic: Georgios Karamanis",
    x = "Revenue in billion dollars"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#5099FF", color = "#5099FF"),
    text = element_text(size = 8, family = "Press Start 2P"),
    axis.text.x = element_text(color = "yellow"),
    axis.title.x = element_text(color = "yellow"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "yellow",
                                      size = 0.2),
    panel.grid.major.y = element_blank(),
  )

year_plot <- top_games %>% 
  distinct(franchise, year_created) %>% 
  mutate(franchise = str_replace_all(franchise, "\\n", " ")) %>% 
  ggplot(aes(1, year_created)) +
  geom_text_repel(aes(x = 1,
                      label = franchise),
                  hjust = 1, direction = "y", nudge_x = -0.5,
                  segment.alpha	= 0, color = "white",
                  size = 2, family = "Press Start 2P") +
  geom_text(aes(x = 2.5, label = year_created), check_overlap = TRUE,
            family = "Press Start 2P", size = 2, color = "yellow") +
  geom_point() +
  geom_path() +
  xlim(-8, 4) +
  scale_y_reverse() +
  labs(
    x = "Year created"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#5099FF", color = "#5099FF"),
    axis.title.x = element_text(size = 8, color = "yellow",
                                family = "Press Start 2P"),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank()
    )
  
p <- plot_grid(rev_plot, year_plot, rel_widths = c(1.5, 1)) +
  theme(
  plot.margin = margin(10, 30, 10, 30),
  plot.background = element_rect(fill = "#5099FF", color = "#5099FF")
  )


title <- ggdraw() +
  draw_label("Top 10 video games with the higher revenue",
             size = 10, fontfamily = "Press Start 2P") +
  theme(
    plot.background = element_rect(fill="#5099FF", color = "#5099FF")
  )

caption <- ggdraw() +
  draw_label("Source: Wikipedia | Graphic: Georgios Karamanis",
             size = 7, fontfamily = "Press Start 2P") +
  theme(
    plot.background = element_rect(fill="#5099FF", color = "#5099FF")
  )

plot_grid (title, p, caption, ncol = 1, rel_heights = c(0.1, 1, 0.1)) +
ggsave(here("week-27", "media_franchises.png"),
          dpi = 300, height = 6, width = 8)
  
