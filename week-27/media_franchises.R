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
  filter(revenue >= 10) %>% 
  mutate(
    franchise = str_replace_all(franchise, " (?=\\w)", "\n"),
    coins = map2(0, revenue, seq, by = 1)
    ) %>%
  arrange(year_created) %>%
  unnest(coins) 

rev_plot <- ggplot(top_games, aes(coins,
              factor(fct_reorder(franchise, revenue)))) +
  geom_image(aes(image = coin_img), size = 0.035, asp = 1.2) +
  scale_x_continuous(labels = c("0", "10", "20", "30"),
                     limits =  c(0, 32)) +
  labs(
    x = "Revenue in billion dollars"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "darkblue", color = "darkblue"),
    text = element_text(size = 8, family = "Press Start 2P"),
    axis.text.x = element_text(color = "magenta1"),
    axis.title.x = element_text(color = "blue",
                                margin = margin(20, 0, 0, 0)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "white", size = 7),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_line(color = "blue",
                                      size = 0.1),
    panel.grid.major.x = element_line(color = "blue",
                                      size = 0.2),
    panel.grid.major.y = element_blank(),
  )

year_plot <- top_games %>% 
  distinct(franchise, year_created) %>% 
  mutate(franchise = str_replace_all(franchise, "\\n", " ")) %>% 
  ggplot(aes(1, year_created)) +
  geom_text_repel(aes(x = 1,
                      label = franchise),
                  hjust = 1, direction = "y", nudge_x = -1,
                  segment.alpha	= 0, color = "white",
                  size = 2.5, family = "Press Start 2P") +
  geom_text(aes(x = 2.5, label = year_created), check_overlap = TRUE,
            family = "Press Start 2P", size = 2.5, color = "magenta1") +
  geom_path(size = 0.2, color = "blue") +
  geom_point(color = "yellow") +
  xlim(-8, 5) +
  scale_y_reverse() +
  labs(
    x = "Year of inception"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "darkblue", color = "darkblue"),
    axis.title.x = element_text(size = 8, color = "blue",
                                family = "Press Start 2P",
                                margin = margin(20, 0, 0, 0)),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.title.y = element_blank()
    )
  
p <- plot_grid(rev_plot, year_plot, rel_widths = c(1.5, 1)) +
  theme(
  plot.margin = margin(10, 30, 10, 30),
  plot.background = element_rect(fill = "darkblue", color = "darkblue")
  )


title <- ggdraw() +
  draw_label("Video game franchises with a revenue of $10 billion\nor more from sales of the actual video games ",
             size = 11, fontfamily = "Press Start 2P", colour = "white") +
  theme(
    plot.background = element_rect(fill="darkblue", color = "darkblue")
  )

caption <- ggdraw() +
  draw_label("Source: Wikipedia | Graphic: Georgios Karamanis",
             size = 6.5, fontfamily = "Press Start 2P", colour = "white") +
  theme(
    plot.background = element_rect(fill="darkblue", color = "darkblue")
  )

plot_grid(title, p, caption, ncol = 1, rel_heights = c(0.2, 1, 0.15)) 

ggsave(here("week-27", "media_franchises.png"),
          dpi = 300, height = 7, width = 10)
