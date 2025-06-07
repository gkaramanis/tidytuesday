library(tidyverse)
library(eulerr)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

gutenberg_metadata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')

bookshelf_expanded <- gutenberg_metadata |> 
  separate_longer_delim(gutenberg_bookshelf, delim = "/") |>
  mutate(gutenberg_bookshelf = str_remove(gutenberg_bookshelf, "Browsing: ")) |>
  filter(!is.na(gutenberg_bookshelf), gutenberg_bookshelf != "")

gu_u <- bookshelf_expanded %>%
  distinct(gutenberg_id, gutenberg_bookshelf)

gu_c <- gu_u %>%
  inner_join(gu_u, by = "gutenberg_id") %>%
  filter(gutenberg_bookshelf.x < gutenberg_bookshelf.y) %>%  
  count(gutenberg_bookshelf.x, gutenberg_bookshelf.y, sort = TRUE) |> 
  rename(
    bookshelf_1 = gutenberg_bookshelf.x,
    bookshelf_2 = gutenberg_bookshelf.y
  ) 

bookshelf_counts <- gu_u %>%
  count(gutenberg_bookshelf, name = "bookshelf_count")

gu_c_with_counts <- gu_c %>%
  left_join(bookshelf_counts, by = c("bookshelf_1" = "gutenberg_bookshelf")) %>%
  rename(count_1 = bookshelf_count) %>%
  left_join(bookshelf_counts, by = c("bookshelf_2" = "gutenberg_bookshelf")) %>%
  rename(count_2 = bookshelf_count) |> 
  mutate(pct = round(n / ((count_1 + count_2) / 2) * 100))

compare_f <- function(shelf1, shelf2, max_size = 18) {
  b <- gu_c_with_counts |> 
    filter((bookshelf_1 == shelf1 & bookshelf_2 == shelf2) | (bookshelf_1 == shelf2 & bookshelf_2 == shelf1)) 
  
  p <- plot(euler(c("A" = b$count_1, "B" = b$count_2, "A&B" = b$n)), labels = c(b$bookshelf_1, b$bookshelf_2), quantities = TRUE)
  
  c_df <- data.frame(
    x = p$data$ellipses$h,
    y = p$data$ellipses$k,
    r = p$data$ellipses$a
  )
  
  l_df <- data.frame(
    lx = p$data$centers$x,
    ly = p$data$centers$y,
    ll = p$data$centers$labels,
    ln = p$data$centers$quantities
  ) |> 
    mutate(ll = replace_na(ll, ""))
  
  g <- ggplot() +
    ggforce::geom_circle(data = c_df, aes(x0 = x, y0 = y, r = r, color = factor(x)), linewidth = 2) +
    geom_text(data = l_df, aes(x = lx, y = ly, label = paste0(ll, "\n", scales::number(ln)), size = ln), family = "Borel") +
    coord_fixed(clip = "off") +
    scale_color_manual(values = c("#97b091", "#e09200")) +
    scale_size_continuous(range = c(8, max_size)) +
    labs(
      title = "Project Gutenberg",
      subtitle = "Distribution of books unique to, and shared by, two bookshelves",
      caption = "Source: Project Gutenberg · Graphic: Georgios Karamanis",
    ) +
    theme_void(base_family = "Porpora", base_size = 12) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#e5d1b5", color = NA),
      plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 15),
      plot.caption = element_text(hjust = 0.5, size = 12),
      plot.margin = margin(10, 0, 10, 0)
    )
  
  return(g)
}


unique(c(gu_c_with_counts$bookshelf_1, gu_c_with_counts$bookshelf_2))

compare_f("Society", "Fiction", 20) 
compare_f("Paranormal", "Psychiatry") 
compare_f("Architecture", "Art & Photography", 16) 
