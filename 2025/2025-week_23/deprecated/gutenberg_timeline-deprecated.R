gutenberg_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_authors.csv')

ggplot(gutenberg_authors, aes(x = birthdate)) +
  geom_histogram(binwidth = 10) +
  scale_y_log10()

ga_c <- gutenberg_authors |> 
  mutate(
    century = floor(birthdate / 100) * 100,
    cyear = birthdate %% 100
    ) |> 
  select(century, cyear) |> 
  count(century, cyear) 

timeline <- tidyr::expand_grid(
    century = seq(min(ga_c$century, na.rm = TRUE), max(ga_c$century, na.rm = TRUE), by = 100),
    cyear = 0:99
  ) |>
  arrange(century) |>
  mutate(century_rank = dense_rank(century)) |>
  mutate(cyear_for_sorting = if_else(century_rank %% 2 == 0, cyear, -cyear)) |>
  arrange(century_rank, cyear_for_sorting) |>
  select(century, cyear) |> 
   

ggplot() + 
  geom_path(data = timeline, aes(x = cyear, y = century, group = 1), color = "grey70", linewidth = 0.5) +
  # These points also should be alternating on x by decade, like the timeline path, in that case the x axis would not be labeled 
  geom_point(data = ga_c, aes(x = cyear, y = century, size = n), shape = 21, fill = "white") +
  scale_size_area(max_size = 15) +
  scale_shape_identity() +
  scale_y_reverse() +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA)
  )

