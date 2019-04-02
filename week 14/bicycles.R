library(ggplot2)
library(tidyr)
library(lubridate)

bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
bike_traffic <- bike_traffic %>% separate(date, c("date", "time"), sep = 11)

# bike_traffic %>% group_by(crossing) %>% tally()
seTr <- subset(bike_traffic, crossing == "Sealth Trail")
seTr$time <- hms(seTr$time)
seTr$date <- mdy(seTr$date)

ggplot(head(seTr, 500), aes(x = date, y = bike_count)) +  
  geom_bar(stat = "identity") +
  coord_polar() +
  scale_y_continuous(expand=c(0.5, 0))
