library(tidyverse)
library(ggforce)
library(lubridate)

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

nuclear62 <- nuclear_explosions %>% 
  filter(year == 1962) %>% 
  mutate(month = month(ymd(date_long))) %>% 
  add_count(month) %>% 
  select(month, n) %>% 
  distinct()

# one petal
x0 = 0
y0 = 0
a = 0.2
b = a * 6
ar = 5
br = 2
petal <- data.frame(
  x = x0 + c(a/ar, -a/ar, a, -a/ar, a/ar, -a),
  y = y0 + c(b, b, b/br, 0, 0, b/br)
)

# flower with f
trans = linear_trans(rotate(a))
flower = NULL
for (i in 1:12){
  x <- trans$transform(petal$x, petal$y, a = i * pi / 6)["x"]
  y <- trans$transform(petal$x, petal$y, a = i * pi / 6)["y"]
  n<- nuclear62[i, 2]
  c <- data.frame(i, n, x, y)
  flower = rbind(flower, c)
}

# plot
ggplot(data = flower, aes(x = x, y = y, group = i, fill = n)) +
  geom_bspline_closed() +
  scale_fill_distiller(palette = "Reds") +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "grey20")
  )

ggsave("week-34/img_plot/n12.png")
