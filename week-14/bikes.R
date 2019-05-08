library(ggplot2)
library(dplyr)
library(tidyr)
library(png)
library(grid)

# icons by Yannick Lung, https://www.iconfinder.com/yanlu
sunimg <- readPNG("./week 14/img/sun.png")
sun <- rasterGrob(sunimg, interpolate=TRUE)
moonimg <- readPNG("./week 14/img/moon.png")
moon <- rasterGrob(moonimg, interpolate=TRUE)

# read data
bike_traffic <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")
bike_traffic$date <- strptime(bike_traffic$date, "%m/%d/%Y %I:%M:%S %p")
bike_traffic <- bike_traffic %>%
  separate(date, c("date", "time"), sep = 11)
bike_traffic <- bike_traffic %>%
  group_by(crossing, time) %>%
  summarise(Mean=mean(bike_count, na.rm = TRUE))
bike_traffic$time <- substring(bike_traffic$time, 1, 5)

# change order in x axis
daytime = c("05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00",
            "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00",
            "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00")

# plot (messy, but it works!)
ggplot(bike_traffic, aes(factor(time, levels = daytime), Mean, color = crossing, group = crossing)) +
  geom_line(size = 2) +
  annotate("rect", xmin = "06:00", xmax = "18:00", ymin = -Inf, ymax = Inf, fill = "skyblue1", color = 0) +
  annotate("rect", xmin = "05:00", xmax = "06:00", ymin = -Inf, ymax = Inf, fill = "midnightblue", color = 0) + 
  annotate("rect", xmin = "18:00", xmax = "04:00", ymin = -Inf, ymax = Inf, fill = "midnightblue", color = 0) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_line(size = 1.2) +
  coord_cartesian(ylim = c(0, 80)) +
  annotation_custom(sun, xmin = 1, xmax = 6,
                    ymin = 65, ymax = 80) +
  annotation_custom(moon, xmin = 14, xmax = 17,
                    ymin = 59, ymax = 83) +
  scale_color_brewer(palette="Dark2") +
  # theme_dark() +
  labs(title =  "Mean number of bikes per hour in Seattle",
       subtitle = "as counted by bike counters in six crossings",
       y = "bikes",
       caption = toupper("Source: Seattle Department of Transportation")) +
  theme(
  text = element_text(size = 14),
  panel.background = element_rect(fill = "grey20"),
  plot.margin=unit(c(1,1.2,1,0.8),"cm"),
  legend.title = element_blank(),
  legend.position = "bottom",
  legend.key = element_blank(),
  legend.text = element_text(margin = margin(r = 20)),
  legend.spacing.x = unit(0.2, 'cm'),
  plot.caption = element_text(color = "gray50"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
  )
