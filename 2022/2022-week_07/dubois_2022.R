library(tidyverse)
library(camcorder)

gg_record(dir = "temp", device = "png", width = 8, height = 10, units = "in", dpi = 320)


h <- round(c(1146, 758, 194, 116, 78) / 1146, 2)
w <- round(1368 / 1146, 2)

triangles <- data.frame(
  i = 1:5,
  h = h,
  col = c("#654321", "#0D2773", "#E3BF0E", "#dc143c", "#0D2773")
  ) %>%
  rowwise() %>% 
  mutate(
    w = 0.75 * h * 1.5,
    x = list(c(0, -w/2, w/2)),
    y = list(c(0, -h, -h))
    ) %>% 
  ungroup() %>% 
  mutate(
    h2 = (h - lead(h, default = 0))/2 - h,
    l = c("Grand total\n153", "Weekly papers\ntotal\n136", 11, 3, 3)
    ) %>% 
  unnest(c(x, y))

f1 = "Charter"
f2 = "Jefferies"
f3 = "Nugo Sans"

ggplot(triangles) +
  # Triangles
  geom_polygon(aes(x, y, group = i, fill = col)) +
  # Text inside the triangles
  geom_text(aes(0, h2, label = l), color = "grey97", family = f2, size = 5.5, lineheight = 0.8) +
  # Text outside the triangles
  annotate("text", 0, 0.01, vjust = 0, label = toupper("Magazines"), family = f3, size = 5) +
  annotate("text", c(-0.06, 0.06), -0.08, hjust = c(1, 0), label = toupper(c("Daily", "Papers")), family = f3, size = 4.5) +
  annotate("text", c(-0.09, 0.09), -0.13, hjust = c(1, 0), label = toupper(c("School", "Papers")), family = f3, size = 4.5) +
  # Title and subtitle
  annotate("text", 0, c(0.55, 0.46, 0.37), label = c("American Negro newspapers and periodicals.", "Journaux et publications periodiques Nègres aux Etats Unis.", "Done by Atlanta University."), family = f1, size = c(7, 5, 5)) +
  # Lines between title and subtitle
  annotate("tile", 0, c(0.5, 0.41), width = 0.35, height = 0.001) +
  scale_fill_identity() +
  scale_y_continuous(limits = c(-1, 0.6)) +
  coord_fixed(expand = FALSE, clip = "off") +
  labs(
    caption = "Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#D6CFC4", color = NA),
    plot.caption = element_text(hjust = 0.5, family = f1, margin = margin(10, 0, 10, 0))
  )
