library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")

seTr <- subset(bike_traffic, crossing == "Sealth Trail")
seTr$date <- strptime(seTr$date, "%m/%d/%Y %I:%M:%S %p")
seTr <- seTr %>% separate(date, c("date", "time"), sep = 11)

grTime <- seTr %>%
  group_by(time) %>%
  summarise(Mean=mean(bike_count, na.rm=TRUE))

grTime$time <- substring(grTime$time, 1, 5)

half1 <- grTime[1:12,]
half2 <- grTime[13:24,]

t <- theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.minor = element_blank()
    ) 


plot1 <- ggplot(half1, aes(x = time, y = Mean)) +  
  geom_bar(stat = "identity") +
  ylim(0, 1.5) +
  coord_polar() +
  scale_y_continuous(expand = c(1, 0.01)) +
  t

plot2 <- ggplot(half2, aes(x = time, y = Mean)) +  
  geom_bar(stat = "identity") +
  ylim(0, 1.5) +
  coord_polar() +
  scale_y_continuous(expand = c(1, 0.01)) +
  t

grid.arrange(plot1, plot2, ncol=2)

