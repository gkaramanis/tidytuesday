library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(magick)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

bike_traffic$date <- strptime(bike_traffic$date, "%m/%d/%Y %I:%M:%S %p")
bike_traffic <- bike_traffic %>%
  separate(date, c("date", "time"), sep = 11)

bike_traffic <- bike_traffic %>%
  group_by(crossing, time) %>%
  summarise(Mean=mean(bike_count, na.rm = TRUE))

grTime$time <- substring(grTime$time, 1, 5)
grTime <- grTime[c(7:18, 19:24, 1:6),]

ggplot(grTime, aes(x = time, y = Mean)) +  
  geom_bar(stat = "identity") +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_discrete(limits = grTime$time) +
  facet_grid(rows = vars(drv))
