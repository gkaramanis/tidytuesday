library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(magick)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

# seTr <- subset(bike_traffic, crossing == "Sealth Trail")
seTr <- bike_traffic
seTr$date <- strptime(seTr$date, "%m/%d/%Y %I:%M:%S %p")
seTr <- seTr %>% separate(date, c("date", "time"), sep = 11)

grTime <- seTr %>%
  group_by(time) %>%
  summarise(Mean=mean(bike_count, na.rm=TRUE))

grTime$time <- substring(grTime$time, 1, 5)

day <- subset(grTime, time >= "06:00" & time <= "17:00")
night <- subset(grTime, time <= "05:00" | time >= "18:00")

t <- theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
    ) 


plot1 <- ggplot(day, aes(x = time, y = Mean)) +  
  geom_bar(stat = "identity") +
  ylim(-20, 33) +
  ylab(NULL) +
  xlab(NULL) +
  coord_polar(start = 2.9) +
  t


plot2 <- ggplot(night, aes(x = time, y = Mean)) +  
  geom_bar(stat = "identity") +
  ylim(-20, 33) +
  ylab(NULL) +
  xlab(NULL) +
  coord_polar(start = -0.25) +
  t +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) 


plot_grid(plot1, plot2) +
  draw_image("/Users/Georgios/Desktop/311808.png",
             x = 0.01, y = 0.13, scale = 0.78)

