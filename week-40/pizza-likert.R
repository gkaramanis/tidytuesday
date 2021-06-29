library(tidyverse)
library(here)
library(cowplot)

pizza_jared <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")


# keep some columns, relevel and number places ----------------------------

pizza_j <- pizza_jared %>% 
  # One place has "Fair" instead of "Never again"
  mutate(
    answer = case_when(
      answer == "Fair" ~ "Never Again",
      TRUE ~ answer
    )
  ) %>% 
  select(polla_qid, place, answer, percent) %>% 
  mutate(
    answer = fct_relevel(answer, c("Never Again", "Poor", "Average", "Good", "Excellent"))
    ) %>% 
  group_by(polla_qid) %>% 
  mutate(id = group_indices()) %>% 
  ungroup()


# common layers and theme -------------------------------------------------

gglist <- list(
  annotate("tile", x = 0, y = 1),
  annotate("tile", x = 6, y = 1),
  annotate("tile", x = 7, y = 1),
  geom_tile(aes(x = as.factor(answer), y = 1, height = 1.1),  fill = "black", stat = "identity"),
  geom_tile(aes(x = as.factor(answer), y = 1, fill = percent),  stat = "identity"),
  scale_x_discrete(expand = c(0, 0)),
  coord_polar(direction = -1, start = pi/7.5),
  scale_fill_gradient(limits = c(0, 1), labels = scales::percent, low = "burlywood1", high = "#cb4b12",
                      na.value = "grey30", guide = "colourbar"),
  facet_wrap(vars(id), ncol = 10),
  theme_void(base_family = "IBM Plex Serif"),
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10)
  )
)


# legend ------------------------------------------------------------------

p1 <- pizza_j %>%
  filter(id == 43) %>%
  ggplot() +
  gglist  +
  geom_text(aes(x = as.factor(answer), y = 1.05, label = answer),
            family = "IBM Plex Serif", size = 5) +
  guides(fill = guide_colorbar(
    title = "Slice color represents percentage of total votes",
    title.position = "top",
    label.position = "bottom",
    barwidth = 15,
    barheight = 0.5
  )
  ) +
  theme(
    legend.position = "top",
    legend.margin = margin(10, 0, -20, 0),
    strip.text = element_blank(),
    plot.margin = margin(10, 0, 0, 0)
    )

tb <- pizza_j %>%
  distinct(id, place) %>% 
  mutate(
    p_col = rep(1:3, each = 25),
    p_row = rep(1:25, 3),
    p_lab = paste0(id, ". ", place)
         ) %>% 
  ggplot() +
  geom_text(aes(x = p_col, y = p_row, label = p_lab), hjust = 0, family = "IBM Plex Serif", size = 2.5) +
  xlim(0.9, 4.1) +
  scale_y_reverse() +
  theme_void() +
  theme(
    plot.margin = margin(50, 0, 0, 0)
  )

# multiples ---------------------------------------------------------------

p2 <- pizza_j %>%
  ggplot() +
  gglist +
  theme(plot.margin = margin(0, 20, 0, 0))


# title, etc -------------------------------------------------------------------

t <- ggplot() +
  labs(
    title = "Pizza Poll Results",
    subtitle = "Results from the pizza polling done by Jared Lander at the New York R meetups"
  ) +
  theme(
    plot.title = element_text(size = 20, family = "IBM Plex Serif Medium", hjust = 0.5, margin = margin(20, 20, 10, 20)),
    plot.subtitle = element_text(size = 16, family = "IBM Plex Sans", hjust = 0.5),
    plot.margin = margin(20, 0, 10, 0)
  )

c <- ggplot() +
  labs(
    caption = "Source: @jaredlander | Graphic: Georgios Karamanis"
  ) +
  theme(
    plot.caption = element_text(size = 12, family = "IBM Plex Sans", hjust = 1, margin = margin(0, 0, 20, 0))
  )

# plot --------------------------------------------------------------------

 
col1 <- plot_grid(tb, p1, ncol = 1, rel_heights = c(1, 1))
row2 <- plot_grid(col1, p2, rel_widths = c(1.2, 2))

plot_grid(t, row2, c, ncol = 1, rel_heights = c(0.07, 0.9, 0.02)) 

ggsave(
    here::here("week-40", "figures", "temp", paste0("pizza-likert-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
    width = 14, height = 10, dpi = 320
    )
 
