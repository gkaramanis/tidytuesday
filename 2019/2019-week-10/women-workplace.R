library(ggplot2)

# read data
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv")

# get stats for all ages
age0 <- earnings_female[c(1:33),]

# plot!
ggplot() +
  geom_area(data=employed_gender, aes(year, full_time_female), fill = "bisque") +
  geom_area(data=employed_gender, aes(year, part_time_female), fill = "peachpuff2") +
  geom_line(data=age0, aes(Year, percent), color = "royalblue1", size=1) +
  scale_x_continuous(limits = c(1980,2010), expand = c(0,0)) +
  scale_y_continuous(breaks = c(25, 50, 75, 100), limits = c(0,100), expand = c(0,0)) +
  labs(title = "Women's earnings as a percentage of men's") +
  theme_bw() +
  theme(
    plot.title = element_text(colour = "royalblue2"),
    plot.margin = unit(c(1, 1, 0.5, 0.5),"cm"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(color = "gray10", size=rel(1.4), margin = margin(t = 15)),
    axis.text.y = element_text(color = "gray70", size=rel(1.2), margin = margin(r = 5))
        )
