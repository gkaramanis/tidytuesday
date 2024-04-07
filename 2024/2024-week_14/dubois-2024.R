library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", height = 10, width = 8, units = "in", dpi = 320)

dubois_week10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-02/dubois_week10.csv')

pal <- c("#EA3323",
         "#000000",
         "#377E21")

pie_pal <- c("Teachers" = "#C93144",
             "Ministers" = "#7B81A4",
             "Government Service" = "#E8C9BC",
             "Business" = "#D2BFAB",
             "Other Professions" = "#8F8D7B",
             "House Wives" = "#EFC573"
             )

t1 <- "A SERIES OF STATISTICAL CHARTS, ILLUSTRATING THE CONDITION OF THE DESCENDANTS OF FORMER AFRICAN SLAVES NOW RESIDENT IN THE UNITED STATES OF AMERICA."

t2 <- "PREPARED AND EXECUTED BY NEGRO STUDENTS UNDER THE DIRECTION OF ATLANTA UNIVERSITY, ATLANTA, GA. UNITED STATES OF AMERICA."

t3 <- "THE UNIVERSITY WAS FOUNDED IN 1867. IT HAS INSTRUCTED 6000 NEGRO STUDENTS. IT HAS GRADUATED 830 NEGROES AMONG WHOM ARE:"

t4 <- "THE UNIVERSITY HAS 20 PROFESSORS AND INSTRUCTORS AND 250 STUDENTS AT PRESENT. IT HAS FIVE BUILDINGS, 60 ACRES OF CAMPUS, AND A LIBRARY OF 11,000 VOLUMES. IT AIMS TO RAISE AND CIVILIZE THE SONS OF THE FREEDMEN BY TRAINING THEIR MORE CAPABLE MEMBERS IN THE LIBERAL ARTS ACCORDING TO THE BEST STANDARDS OF THE DAY. THE PROPER ACCOMPLISHMENT OF THIS WORK DEMANDS AN ENDOWMENT FUND OF $500.000."

f1 <- "Jefferies"

dubois_week10 %>% 
  mutate(Occupation = fct_reorder(Occupation, Percentage)) %>% 
  ggplot(data = .,  aes(x = 1, y = Percentage, fill = Occupation, label = scales::percent(Percentage, scale = 1))) +
  geom_bar(position = "fill", stat = "identity", key_glyph = draw_key_dotplot) +
  geom_text(position = position_fill(vjust = 0.5), stat = "identity", family = f1, size = 6) +
  scale_fill_manual(values = pie_pal) +
  guides(fill = guide_legend(override.aes = list(size = 15))) +
  coord_radial(theta = "y", expand = FALSE, start = -pi/1.5, direction = -1) +
  labs(
    title = str_wrap(t1, 52),
    subtitle = paste0(str_wrap(t2, 52), "\n\n", str_wrap(t3, 54)),
    caption = paste0(str_wrap(t4, 80), "\n\nSource: 2024 Du Bois Visualization Challenge · Graphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "left",
    legend.title = element_blank(),
    legend.margin = margin(0, -20, 0, 0),
    legend.text = element_text(size = 15),
    legend.key.spacing.y = unit(-1.7, "line"),
    plot.background = element_rect(fill = "#E6DCCE", color = NA),
    plot.title.position = "plot",
    plot.title = element_text(size = 22, lineheight = 0.8, margin = margin(20, 20, 0, 0)),
    plot.subtitle = element_text(size = 22, lineheight = 0.8, margin = margin(20, 20, 0, 0)),
    plot.caption = element_text(hjust = 0.5, size = 14),
    plot.caption.position = "plot"
  )

