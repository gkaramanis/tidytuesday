library(tidyverse)
library(camcorder)
library(poissoned)

gg_record(dir = "temp", device = "png", width = 10, height = 11.5, units = "in", dpi = 320)

production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

gr_prod <- production %>% 
  # Filter Greece
  filter(Code == "GRC") %>% 
  pivot_longer(4:last_col(), names_to = "fish", values_to = "tonnes") %>% 
  janitor::clean_names() %>%
  mutate(
    # Extract seafood names from column names
    fish = str_extract(fish, "(?<=lent - ).+(?= - 27)"),
    fish = str_remove(fish, ",.+"),
    # Create decade
    decade = paste0((year - 1) %/% 10 * 10, "s")
    ) %>% 
  group_by(decade, fish) %>% 
  # Calculate median
  summarise(tonnes_med = median(tonnes)) %>% 
  ungroup()

# Create points with {poissoned}
gr_prod_poissoned <- gr_prod %>% 
  rowwise() %>% 
  mutate(
    # Divide by 50 to create less points, otherwise too many
    t = sqrt(tonnes_med / 50),
    # Create list with all points for every row
    pnts = list(poisson_disc(ncols = t, nrows = t, cell_size = 1 / t))
    ) %>% 
  ungroup() %>% 
  unnest(pnts) %>% 
  rowwise() %>% 
  # Add random angle for each point
  mutate(angle = runif(1, 0, 360)) %>% 
  ungroup()

# Fonts and colors
f1 = "DIN Condensed"
f2 = "Apfer Grotezk"

col_sea = "aliceblue"
col_fish = "#4074BB"
col_bg = "#212147"
col_bg2 = "#FFF7F0"

# Plots
ggplot() +
  # Background for points
  geom_tile(data = gr_prod_poissoned, aes(0.5, 0.5, width = 1.07, height = 1.07), fill = col_sea, color = "grey97", size = 0.5, stat = "unique") +
  # Tonnes, rounded to the nearest tenth
  geom_text(data = gr_prod, aes(0.5, -0.23, label = format(round(tonnes_med, -1), big.mark = " ")), alpha = 1, family = f1, fontface = "bold", size = 5, vjust = 0, color = "grey97") +
  # Points
  geom_text(data = gr_prod_poissoned, aes(x, y, label = "➤", angle = angle), size = 2.5, color = col_fish, family = "SF Pro Text") +
  facet_grid(decade ~ fish) +
  coord_fixed(clip = "off") +
  labs(
    title = "Seafood production in Greece",
    subtitle = "Median production by decade (1961-2013) in tonnes rounded to the nearest tenth",
    caption = "Source: OurWorldinData.org · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_size = 15, base_family = f1) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = col_bg),
    strip.text = element_text(size = 12, family = f2, face = "bold"),
    strip.background = element_blank(),
    strip.text.x = element_text(color = col_bg),
    strip.text.y = element_text(color = col_bg, angle = -90),
    plot.background = element_rect(fill = col_bg2, color = NA),
    plot.title = element_text(size = 40, color = col_bg),
    plot.subtitle = element_text(size = 20, color = col_bg)
  )
  