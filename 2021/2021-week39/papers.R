library(tidyverse)
library(camcorder)
library(ggstream)
library(patchwork)
library(wesanderson)
library(tinter)

gg_record(dir = "temp", device = "png", width = 10, height = 12, units = "in", dpi = 320)

# Read in data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# Palette
pal <- wes_palette("Darjeeling1")[-3]

# Join data and add colors
pp <- programs %>%
  group_by(program_category) %>% 
  arrange(program_desc) %>% 
  mutate(
    program_category = replace_na(program_category, "Technical"),
    cat_id = cur_group_id(),
    # add main color for program categories 
    cat_col = pal[cat_id],
    prog_id = row_number(),
    prog_n = max(prog_id)
  ) %>% 
  ungroup() %>% 
  # add color tints for individual programs
  rowwise() %>% 
  mutate(prog_col = list(tinter(cat_col, steps = prog_n + 2, crop = 2, direction = "tints", adjust = -0.1))[[1]][prog_id]) %>% 
  ungroup() %>% 
  left_join(paper_programs) %>% 
  left_join(papers)

# Count papers per year, remove 2021
pp_date <- pp %>% 
  add_count(year, program_category, program_desc) %>% 
  filter(year < 2021) %>% 
  arrange(cat_id, prog_id) %>% 
  distinct(year, program_category, program_desc, cat_col, prog_col, n)

# Fonts
f1 = "Piazzolla SC"

# Common function for stream plot
stream <- function(type) {
  ggplot(pp_date, aes(x = year, y = n, fill = prog_col, color = colorspace::darken(cat_col, 0.2), label = program_desc)) +
    geom_stream(type = type, size = 0.15, sorting = "onset") +
    scale_color_identity() +
    scale_fill_identity() +
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_minimal(base_size = 14, base_family = f1) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  }

# Plot for number of papers
p1 <- stream("ridge") +
  scale_x_continuous(position = "top") +
  labs(
    title = "NBER Working Papers, 1975-2020",
    subtitle = "Number and proportion of papers distributed by the National Bureau of Economic\nResearch by program and program category",
    caption = "Source: NBER · Graphic: Georgios Karamanis",
    x = "Number of papers"
  ) +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(color = "grey30")
    )

# Annotations for p2
annot <- data.frame(
  x = c(2002.115, 1986.348, 2005.984, 2008.855),
  y = c(0.91, 0.68, 0.28, 0.03),
  program_category = c("Finance", "Macro/International", "Micro", "Technical")
  ) %>% 
  left_join(pp_date) %>% 
  distinct(x, y, program_category, cat_col)

# Proportional plot
p2 <- stream("proportional") +
  geom_stream_label(aes(color = colorspace::darken(prog_col, 0.4)), type = "proportional", sorting = "onset", alpha = 0.7, family = f1, size = 3, hjust = "inward") +
  geom_text(data = annot, aes(x = x, y = y, label = program_category, color = colorspace::darken(cat_col, 0.4)), inherit.aes = FALSE, family = f1, fontface = "bold", size = 5) +
  ylab("Proportion of papers")

# Legend for p1
p3 <- pp %>%
  distinct(program_category, cat_col, program_desc, prog_id, prog_n, prog_col) %>%
  # Break up categories to two columns (facets)
  mutate(
    program_category = fct_reorder(program_category, -prog_n),
    facet_col = if_else(program_category == "Micro", 1, 2)
  ) %>%
  group_by(facet_col) %>%
  arrange(program_category, program_desc) %>%
  mutate(facet_y = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = 0, y = facet_y, label = program_desc, fill = prog_col)) +
  geom_label(aes(color = colorspace::darken(cat_col, 0.2)), hjust = 0, family = f1, size = 2.5) +
  geom_text(nudge_x = 0.015, hjust = 0, family = f1, size = 2.5) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_reverse() +
  coord_cartesian(expand = FALSE, clip = "off") +
  facet_grid(~facet_col) +
  theme_void() +
  theme(
    strip.text = element_blank()
    )

# Combine all plots
p1 / p2 +
  inset_element(p3, 0.02, 1.26, 0.5, 1.98)

