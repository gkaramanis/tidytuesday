marbles_times <- marbles %>% 
  filter(!is.na(points)) %>% 
  group_by(race) %>% 
  mutate(
    race_n = cur_group_id(),
    time_pct = 1 - time_s/min(time_s)
  )

ggplot(marbles_times) +
  geom_point(aes(time_pct, -race_n, colour = marble_name)) +
  geom_text(aes(time_pct, -race_n - 0.2, label = marble_name), angle = 45)
