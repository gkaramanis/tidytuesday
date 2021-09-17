library(wesanderson)

pal <- wes_palette("Zissou1", n = 12, type = "continuous")

aifcis <- billboard %>% 
  filter(str_detect(song, "All I Want For Christmas Is You")) %>% 
  mutate(
    week_id = mdy(week_id),
    year = year(week_id),
    week = week(week_id),
    week = if_else(week < 10, week + 53, week),
    season = if_else(week < 3, year - 1, year)
  ) %>% 
  arrange(season, year, week)

ggplot(aifcis, aes(x = week, y = week_position, group = interaction(season, song_id), color = year)) +
  geom_vline(aes(xintercept = 52), size = 2, color = "grey50") +
  geom_line() +
  geom_point() +
  scale_x_continuous(limits = c(45, 55)) +
  scale_y_reverse(limits = c(100, 1), breaks = c(seq(100, 20, by = -10), 10, 5, 1)) +
  scale_size_continuous(range = c(0.25, 1.5)) +
  scale_color_stepsn(colors = pal, n.breaks = 12) 

