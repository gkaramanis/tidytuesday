library(tidyverse)
library(camcorder)
library(MetBrewer)
library(patchwork)

gg_record(dir = "tidytuesday-temp", device = "png", width = 19, height = 11, units = "in", dpi = 320)

death_source <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_source.csv')

deaths <- death_source %>% 
  janitor::clean_names() %>% 
  rename(death_rate = 4) %>% 
  mutate(entity = fct_reorder(entity, death_rate))

f1 <- "Input Mono Compressed"
f2 <- "Publico Headline"

# Plot
plot_f <- function(df) {
  
  # Angles for axis labels
  n <- length(unique(df$entity))
  a <- 360/(2*pi) * rev(pi/2 + seq(pi/n, 2*pi - pi/n, len = n)) - 180
  h = ifelse(abs(a) < 90, 0, 1)
  a2 <- ifelse(h == 0, a, a - 180)

  # Create coord_polar with hjust = h
  # https://stackoverflow.com/questions/55528120/how-to-align-the-text-in-labels-to-the-left-in-a-coord-polar-graph-in-ggplot2
  library(grid)
  CoordPolar2 <- ggproto("CoordPolar2",
                         CoordPolar,
                         render_fg = function (self, panel_params, theme) {
                           if (is.null(panel_params$theta.major)) {
                             return(element_render(theme, "panel.border"))
                           }
                           theta <- ggplot2:::theta_rescale(self, panel_params$theta.major, panel_params)
                           labels <- panel_params$theta.labels
                           theta <- theta[!is.na(theta)]
                           ends_apart <- (theta[length(theta)] - theta[1])%%(2 * pi)
                           if (length(theta) > 0 && ends_apart < 0.05) {
                             n <- length(labels)
                             if (is.expression(labels)) {
                               combined <- substitute(paste(a, "/", b), list(a = labels[[1]], 
                                                                             b = labels[[n]]))
                             }
                             else {
                               combined <- paste(labels[1], labels[n], sep = "/")
                             }
                             labels[[n]] <- combined
                             labels <- labels[-1]
                             theta <- theta[-1]
                           }
                           grid::grobTree(if (length(labels) > 0) 
                             ggplot2:::element_render(theme, 
                                                      "axis.text.x", 
                                                      labels,
                                                      unit(0.45 * sin(theta) + 0.5, "native"),
                                                      unit(0.45 * cos(theta) + 0.5, "native"), 
                                                      hjust = h, # hjust = 0.5,
                                                      vjust = 0.5), 
                             ggplot2:::element_render(theme, "panel.border"))
                         })
  
  coord_polar2 <- function (theta = "x", start = 0, direction = 1, clip = "on") {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") 
      "y"
    else "x"
    ggproto(NULL, 
            CoordPolar2, #CoordPolar,
            theta = theta, r = r, start = start, 
            direction = sign(direction), clip = clip)
  }
  
ggplot(df) +
  geom_line(aes(x = entity, y = death_rate, group = year, color = year), size = 0.3) +
  annotate("text", x = 1, y = seq(0, 300, 100), label = seq(0, 300, 100), family = f1, alpha = 0.2, color = "darkblue") +
  scale_color_met_c("Tam") +
  scale_y_continuous(limits = c(-100, 340)) +
  coord_polar2(clip = "off") +
  guides(color = guide_colorbar(direction = "horizontal", title = "Year", title.position = "top", title.hjust = 0.5)) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(-0.05, 0.1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = a2, size = 9, color = "#00000090")
  )
}

p1 <- deaths %>% 
  filter(!is.na(code)) %>%
  plot_f(.) +
  theme(legend.position = "none")

p2 <- deaths %>% 
  filter(is.na(code)) %>%
  plot_f(.)


p1 + p2 +
  plot_annotation(
    title = "Death rates from indoor air pollution are declining",
    subtitle = "Number of deaths per 100,000 people in a country (left) or region (right)\nCountries and regions ordered by mean death rate 1990-2019",
    caption = "Source: Our World in Data · Graphic: Georgios Karamanis"
    ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, family = f2, face = "bold", size = 25, margin = margin(15, 0, 7, 0)),
    plot.subtitle = element_text(hjust = 0.5, family = f2, size = 18, lineheight = 1.1),
    plot.caption = element_text(hjust = 0.5, family = f2, size = 15, margin = margin(0, 0, 10, 0))
  )
