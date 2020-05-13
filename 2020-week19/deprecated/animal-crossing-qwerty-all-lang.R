villagers <- readxl::read_xlsx(here::here("2020-week19", "data", "villagers_multilang_expanded.xlsx")) %>% 
  filter(str_detect(language, "NOA|NOE", negate = T)) %>% 
  select(language, name = translit)

villagers_chars <- villagers %>% 
  unnest_tokens(code, name, "characters") %>% 
  group_by(language, code) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  left_join(qwerty_grid)

ggplot(villagers_chars, aes(x = col, y = row)) +
  geom_tile(aes(alpha = freq), fill = "#E3C089") +
  geom_text(aes(label = toupper(code)), family = "JetBrains Mono Bold", colour = "white", size = 7) +
  coord_fixed() +
  facet_wrap(vars(language), nrow = 4) +
  theme_void(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "#61A554", colour = "#61A554"))
