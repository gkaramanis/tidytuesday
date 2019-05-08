library(tidyverse)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

ggplot(student_ratio, aes(student_ratio, factor(indicator))) +
  geom_jitter(height = 0.1, alpha = 0.2) +
  scale_y_discrete(labels="edulit_ind") +
  theme_minimal()
