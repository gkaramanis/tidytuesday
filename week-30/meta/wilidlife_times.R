library(ggplot2)

wildlife_times <- read.csv("week-30/wildlife_times.csv") 

ggplot(wildlife_times) +
  geom_point(aes(date, time)) +
  scale_y_time()
