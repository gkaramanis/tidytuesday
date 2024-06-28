library(ggdist)

iso_lang <- ISOcodes::ISO_639_3 %>%
  select(code = Part1, language = eng) %>% 
  filter(!is.na(code)) %>% 
  mutate(language = str_remove(language, " *\\(.+\\) *"))

lgbtq_lang <- lgbtq_movies %>% 
  filter(vote_count > 5) %>% 
  add_count(original_language) %>%
  filter(n > 10) %>% 
  group_by(original_language) %>% 
  mutate(vote_lang = mean(vote_average)) %>% 
  ungroup() %>% 
  mutate(original_language = fct_reorder(original_language, vote_lang)) %>% 
  left_join(iso_lang, by = c("original_language" = "code"))
  
f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Publico Headline"

ggplot(lgbtq_lang, aes(x = vote_average, y = paste(language, n), fill = original_language)) +
  geom_vline(xintercept = 5) +
  stat_interval() +
  # stat_slab() +
  scale_x_continuous(limits = c(3, 9), breaks = 1:10) +
  coord_radial(start = -pi/2, end = pi/2, inner.radius = 0.4, expand = FALSE) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 11)
  )

