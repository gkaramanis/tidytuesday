library(ggplot2)
library(dplyr)
library(tidyr)

# read data, sum by date
bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
bike_traffic$date <- strptime(bike_traffic$date, "%m/%d/%Y %I:%M:%S %p")
bike_traffic <- bike_traffic %>%
  separate(date, c("date", "time"), sep = 11)
bike_traffic <- bike_traffic %>%
  group_by(date) %>%
  summarise(value = sum(bike_count, na.rm = TRUE))
bike_traffic$date <- as.Date(bike_traffic$date)

# read rain data
rain <- read.csv("./week 14/precip.csv")
rain <- rain %>% 
  group_by(DATE) %>% 
  summarise(value = mean(PRCP, na.rm = TRUE))
rain$DATE <- as.Date(rain$DATE)
colnames(rain)[1] <- "date"

# plot
ggplot() +
  geom_path(data=rain, aes(date, value*5000), color = "blue", alpha = 0.6) +
  geom_point(data=bike_traffic, aes(date, value)) +
  scale_x_date() +
  scale_y_continuous(sec.axis = sec_axis(~./5000)) 
