library(tidyverse)
library(janitor)
library(jpeg)
library(cowplot)
library(ggbump)

dubois_2 <- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/challenge02/data.csv") %>% 
  pivot_longer(cols = 3:5, names_to = "status") %>% 
  clean_names() %>% 
  mutate(
    population = fct_rev(population),
    age = if_else(startsWith(age, "60"), str_wrap(age, 3), age)
    )

pal <- c("#4C6655", "#F2B937", "#D34150")

original <- readJPEG(here::here("2021", "2021-week7", "originals", "original-plate-10.jpg"))

f1 = "Nugo Sans Light"
f2 = "Jefferies"

p <- ggplot(dubois_2) +
  # bars and bar labels
  geom_bar(aes(fill = status, y = value, x = population), position = "fill", stat = "identity", width = 0.55, color = "grey50", size = 0.2) +
  geom_text(aes(y = value, x = population, label = paste0(value, "%")), size = 3.5, position = position_fill(vjust = 0.5), family = f1) +
  # population labels
  geom_text(aes(x = population, y = 0, label = toupper(population)), hjust = 1, nudge_y = -0.04, family = f1, size = 4) +
  # age labels
  geom_text(aes(x = 1.5, y = 0, label = toupper(age)), hjust = 0.5, nudge_y = -0.325, family = f1, size = 4, stat = "unique") +
  # brackets
  geom_sigmoid(aes(x = 2.25, xend = 1.5, y = -0.19, yend = -0.265), size = 0.15, direction = "y", smooth = 6) +
  geom_sigmoid(aes(x = 0.75, xend = 1.5, y = -0.19, yend = -0.265), size = 0.15, direction = "y", smooth = 6) +
  # scales, theme, etc.
  scale_fill_manual(values = pal) +
  coord_flip(clip = "off", expand = FALSE) +
  facet_wrap(vars(age), ncol = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#DFD6C7", color = NA),
    plot.margin = margin(0, 70, 150, 85),
    strip.text = element_blank(),
    panel.spacing.y = unit(7.5, "lines")
  ) 

status_labels <- c("Single", "Married", "Widowed and divorced")

l <- ggplot() +
  annotate("point", x = c(0, 0, 0.98), y = c(1, 0, 0.5), fill = rev(pal), size = 10, shape = 21, color = "grey50") +  
  annotate("text", x = c(0, 0, 1) + 0.1, y = c(1, 0, 0.5), label = toupper(status_labels), hjust = 0, family = f1, size = 3.5) + 
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(title = toupper("Conjugal condition .")) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, margin = margin(0, 0, 20, 0), family = f2),
    plot.background = element_rect(fill = "#DFD6C7", color = NA),
    plot.margin = margin(35, 180, 20, 180)
  )


plot_grid(l, p, ncol = 1, rel_heights = c(0.25, 1)) +
  draw_text("AGE", x = 0.16, y = 0.79, family = f1, size = 10.5) +
  draw_text(c("15-40", "45-64", "65 AND OVER"), x = 0.26, y = c(0.702, 0.456, 0.208), family = f1, size = 5) +
  draw_text("#DuBoisChallenge | Graphic: Georgios Karamanis", x = 0.5, y = 0.03, family = f1, size = 6) +
  ggsave(here::here("temp", paste0("dubois-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10.128, width = 8)


ggdraw() +
  draw_image(original) +
  # draw_plot(p) +
  ggsave(here::here("temp", paste0("dubois-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10.128, width = 8)
