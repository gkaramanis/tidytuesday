library(tidyverse)
library(ComplexUpset)
library(wesanderson)
library(ggimage)
library(patchwork)

parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv")

parks_amenities <- parks %>% 
  select(year,
         "Basketball hoops" = basketball_points, 
         "Dog parks" = dogpark_points,
         "Playgrounds" = playground_points,
         "Recreation and\nsenior centers" = rec_sr_points,
         "Restrooms" = restroom_points,
         "Splashgrounds and\nsplashpads" = splashground_points,
         total_pct)
  
amenities <- colnames(parks_amenities)[c(-1, -8)]

f1 = "Source Serif Pro"
f2 = "Fira Sans Compressed"

pal <- wes_palette("Zissou1", 9, type = "continuous")

theme_park <- function() {
  theme_minimal(base_family = f1) +
    theme(
      legend.position = c(0.5, 1.3),
      legend.text = element_text(size = 11),
      plot.background=element_rect(color = NA),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold")
    )
  }

# Upset plot
u <- upset(parks_amenities, amenities, name = "Amenities",
      width_ratio = 0.2, height_ratio = 0.6,
      stripes = c("white", "darkseagreen1"),
      set_sizes = upset_set_size(
        geom = geom_bar(fill = "darkgreen", width = 0.6)
      ) +
          theme(
          text = element_text(family = f2),
          axis.title.x = element_blank()
          ),
      themes = upset_modify_themes(
        list("intersections_matrix" = theme(
          text = element_text(family = f2, lineheight = 0.8),
          axis.title.x = element_blank()
          ))
        ),
      base_annotations = list(
        "Intersection size" = (
          intersection_size(
            mapping = aes(fill = factor(year)),
            text = list(family = f2)
          ) +
            labs(title = "Number of parks by combination of amenities") +
            scale_fill_manual(values = pal) +
            guides(fill = guide_legend(nrow = 1, override.aes = list(size = 1))) +
            theme_park() +
            theme(
              legend.title = element_blank(),
              plot.margin = margin(40, 0, 0, 0)
            )
        )
      ),
      annotations = list(
        "Total points" = (
          ggplot(mapping = aes(y = total_pct)) +
            geom_boxplot(width = 0.25, outlier.shape = NA, color = "grey50") +
            geom_point(aes(color = factor(year)), size = 0.5, alpha = 0.3,
                       position = position_jitter(width = 0.1)) +
            ylim(0, 100) +
            labs(title = "Total points by combination of amenities") +
            scale_color_manual(values = pal) +
            theme_park() +
            theme(
              legend.position = "none"
            )
          )
        )
      ) 

# Inset plot
img <- here::here("2021", "2021-week25", "img", "park-darkgreen.png")

i <- ggplot() +
  geom_image(aes(x = 0, y = 1.1, image = img), size = 1, asp = 0.33) +
  geom_text(aes(x = 0, y = 9, label = str_wrap("The ParkScore index is the most comprehensive rating system to measure how well the 100 largest U.S. cities are meeting the need for parks. To determine a city’s ParkScore rating, points for 14 measures across five categories are assigned: acreage, investment, amenities, access, and equity. Shown here is the number of parks  and the ParkScore index for the most common combinations of amenities from 2012 to 2020", 34)), vjust = 1, family = f2) +
  labs(
    title = "U.S. PARKS",
    caption = "Source: The Trust for Public Land\nGraphic: Georgios Karamanis"
  ) +
  coord_fixed(expand = FALSE, clip = "off") +
  xlim(-1.5, 1.5) +
  ylim(0, 9) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 30, margin = margin(0, 0, 10, 0), color = "darkgreen"),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 0, 0), lineheight = 1.01)
  )

# Combine plots
u +
  inset_element(i, -0.2, 1.1, 0.025, 4.4, align_to = "full") +
  plot_annotation(theme = theme(plot.margin = margin(20, 20, 20, 20)))

ggsave(here::here("temp", paste0("parks-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 10, height = 8)

