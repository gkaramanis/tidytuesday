library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

game_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')

gf_fran <- game_films |> 
  filter(!is.na(worldwide_box_office)) |> 
  filter(category == "Theatrical releases") |> 
  filter(subcategory == "English") |> 
  mutate(
    franchise = case_when(
      str_detect(title, "Resident") ~ "Resident Evil",
      str_detect(title, "Tomb Raider") ~ "Tomb Raider",
      str_detect(title, "Sonic") ~ "Sonic the Hedgehog",
      str_detect(title, "Mortal Kombat") ~ "Mortal Kombat",
      str_detect(title, "Street Fighter") ~ "Street Fighter",
      str_detect(title, "Pokémon") ~ "Pokémon",
      str_detect(title, "Super Mario") ~ "Super Mario",
      str_detect(title, "Final Fantasy") ~ "Final Fantasy",
      str_detect(title, "Angry Birds") ~ "Angry Birds",
      str_detect(title, "Silent Hill") ~ "Silent Hill",
      TRUE ~ NA
    )
  ) |> 
  filter(!is.na(franchise)) |> 
  arrange(release_date) |>
  group_by(franchise) |> 
  mutate(
    release_year = year(release_date),
    cumm_bo = cumsum(worldwide_box_office)
  ) |> 
  ungroup()


ggplot(gf_fran, aes(x = release_date, y = cumm_bo)) +
  geom_line(aes(color = franchise), linewidth = 1) +
  geom_point(aes(color = franchise, size = worldwide_box_office), shape = 21, fill = "white") +
  scale_size_area(max_size = 20) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(franchise), scales = "free") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA)
  )

record_polaroid()


game_films |> 
  ggplot() +
  ggrepel::geom_text_repel(aes(x = metacritic, y = rotten_tomatoes, label = title))

record_polaroid()