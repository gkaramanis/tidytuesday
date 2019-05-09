library(tidyverse)
library(wesanderson)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# keep just the countries
student_ratio %>% filter(nchar(country_code) < 4) %>% 
ggplot(aes(student_ratio, edulit_ind)) +
  geom_jitter(height = 0.05, alpha = 0.4,
              shape = 4, color = "midnightblue") +
  scale_x_continuous(limits = c(0, 180),
                     expand = c(0, 0)) +
  scale_y_discrete(labels = c("Pre-Primary", "Primary",
                              "Lower\nSecondary", "Secondary",
                              "Upper\nSecondary", "Post-Secondary\nNon-Tertiary",
                              "Tertiary")) +
  labs(title = "Student to Teacher Ratios",
       subtitle = "in 200 countries, by level of education",
       caption = "\nSource: UNESCO | Graphic: @geokaramanis",
       x = "Student to teacher ratio"
       ) +
  theme_minimal() +
  theme(
  legend.position = "none",
  plot.background = element_rect(fill = "cornsilk", colour = "cornsilk"),
  plot.margin = unit(c(0.2, 0.8, 0.2, 0.6), "cm"),
  panel.grid = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray85", size = 0.3),
  axis.text.y = element_text(size = 9),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  axis.ticks.x = element_line(color = "#212121", size = 0.3),
  axis.ticks.length = unit(0.2, "cm"),
  axis.line.x = element_line(size = 0.3, color = "#212121"),
  text = element_text(family = "IBM Plex Sans", size = 9),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(vjust = 2)
) +
  ggsave("./week-19/students.png", width = 6, height = 4)
