library(tidyverse)
library(here)
library(ggimage)
library(cowplot)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

# elements to draw
draw_elements <- c("cloud", "mountain", "tree", "sun", "cabin",
                   "bushes", "lake", "river", "grass", "fence",
                   "waterfall", 
                   "cactus", "palm_trees", "lighthouse", "sea", "beach",
                   "moon", "boat", "rocks",
                   "night")

bob <- bob_ross %>% 
  janitor::clean_names() %>% 
  mutate(n = row_number()) %>% 
  separate(episode, into = c("season", "episode"), sep = "E", remove = F) %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer) %>%
  # remove frame elements and names (columns)
  select(-contains("FRAME"), -contains("STEVE"), -contains("DIANE")) %>%
  # remove episodes with guests (rows)
  filter(guest != 1) %>%
  # titlecase for episode titles
  mutate(title = str_to_title(title)) %>%
  # gather drawing elements
  gather("element", "exists", aurora_borealis:winter, na.rm = T) %>%
  filter(exists != 0) %>% 
  select(-exists) %>%
  # sort
  arrange(season, episode) %>%
  # rename elements
  mutate(
    element = case_when(
      element == "barn" ~ "cabin",
      element == "building" ~ "cabin",
      element == "farm" ~ "cabin",
      element == "clouds" ~ "cloud",
      element == "mountains" ~ "mountain",
      element == "hills" ~ "mountain",
      element == "trees" ~ "tree",
      element == "conifer" ~ "tree",
      element == "deciduous" ~ "tree",
      element == "cumulus" ~ "cloud",
      element == "cirrus" ~ "cloud",
      element == "snowy_mountain" ~ "mountain",
      element == "waves" ~ "sea",
      element == "ocean" ~ "sea",
      T ~ element
    )
  ) %>% 
  # remove duplicates after renaming
  distinct(season, episode, title, element, n) %>% 
	# images
	mutate(img_element = paste0(element, ".png")) %>%
  # keep elements that can be drawn
  filter(element %in% draw_elements)

# legend
label1 <- "cloud (cirrus, cumulus), moon, night, palm tree, sun, mountain (hills), cactus, lighthouse, rocks, tree (deciduous, conifer), beach, boat, cabin (barn, building, farm), sea (ocean, waves), bushes, fence, grass, waterfall, lake, river"
#label2 <- "moon, cactus, palm tree, lighthouse, rocks, beach, boat, sea (ocean, waves)"
# wrapper
wrapper <- function(x, ...){paste(strwrap(x, ...), collapse = "\n")}
# plot legend
p1 <- ggplot() +
  geom_image(aes(image = here("week-32", "elements", "legendx4.png"), 0, 0), size = 1) +
  geom_text(aes(label = wrapper(label1, width = 50), -9.5, -6),
            hjust = 0, vjust = 1,
            family = "Silkscreen", size = 4.5) +
  #geom_text(aes(label = wrapper(label2, width = 25), 0.5, -6),
  #          hjust = 0, vjust = 1,
  #          family = "Silkscreen", size = 4) +
  coord_fixed(xlim = c(-10, 10), ylim =  c(-10, 10)) +
  labs(
    # title = "Bob Ross:\npainting by the elements",
    subtitle = wrapper("Graphic representations of Bob Ross' paintings with elements identified in them by Walt Hickey (FiveThirtyEight). Each element represents one or more occurrences in the painting. Only the elements in the legend below are drawn. To the right there are 25 random paintings that Bob painted in 'The Joy of Painting', with the season and episode number.", width = 45),
    caption = "source: FiveThirtyEight | plot: Georgios Karamanis") +
  theme_void(base_family = "Silkscreen") +
	theme(
		plot.title = element_text(size = 20, family = "Silkscreen Bold"),
		plot.subtitle = element_text(size = 16),
		plot.caption = element_text(margin = margin(20, 0, 0, 0)),
		plot.margin = margin(0, 20, 20, 20)
	)
	
# paintings
p2 <- bob %>% 
  group_by(n) %>% nest() %>% sample_n(25) %>% unnest() %>%  ungroup() %>% 
  ggplot() +
  geom_image(aes(image = here("week-32", "elements", img_element), 0, 0), size = 1) +
  geom_text(aes(label = paste(season, episode, sep = "-"),
                x = -32, y = 32),
            family = "Silkscreen", size = 3, hjust = 0) +
  coord_fixed(xlim = c(-32, 32), ylim =  c(-32, 32)) +
  facet_wrap( ~ n, ncol = 5) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.border = element_rect(color = "grey90", fill = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) 

# title
title <- ggdraw() + draw_label("Bob Ross - painting by the elements",
                               size = 20, fontfamily = "Silkscreen Bold")
# p1+p2
p <- plot_grid(p1, p2, rel_widths = c(2, 3))
# title + (p1 + p2)
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) +
  ggsave(here("week-32", "img_plot", paste0("bob_ross", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         width = 16, height = 10, dpi = 320)

# all the paintings
bob %>% 
  ggplot() +
  geom_image(aes(image = here("week-32", "elements", img_element), 0, 0), size = 1) +
  geom_text(aes(label = paste(season, episode, sep = "-"),
                x = -32, y = 32),
            family = "Silkscreen", size = 2, hjust = 0) +
  coord_fixed(xlim = c(-32, 32), ylim =  c(-32, 32)) +
  facet_grid(season ~ episode) +
  labs(title = "Bob Ross - painting by the elements (The big picture)") +
  theme_void(base_family = "Silkscreen Bold") +
  theme(
    strip.text = element_blank(),
    plot.title = element_text(size = 20, margin = margin(0, 0, 40, 0)),
    panel.border = element_rect(color = "grey90", fill = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  ggsave(here("week-32", "img_plot", paste0("massive", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
         width = 12, height = 28, dpi = 320)

# theme_void(base_size, bas e_family)
# rmd
# https://medium.com/nightingale/the-process-of-familiarity-an-interview-with-nicholas-rougeux-c30f1a1b2f8?source=rss----356ca48206e6---4
# https://twitter.com/praveenpjose/status/1155724143184113665