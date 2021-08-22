library(tidyverse)
library(ggtext)

# data
dat <- tibble(
  country = toupper(c("United<br>States", "Italy", "Canada", "United<br>Kingdom", "Japan", "Germany", "France", "Russia")),
  # Get creative with the values, they don't need to match the labels
  count = c(3.2, 0.71, 0.5, 0.25, 0, 0.35, 0.2, 0),
  label = c(as.character(c(3.2, 0.71, 0.5, 0.1, 0, 0.2, 0.1, NA))),
  code = toupper(c("us", "it", "ca", "gb", "jp", "de", "fr", "ru"))
)

axis_labels <- c(paste0("<img src='week-50/png/", dat$code, ".png' width='85' align='top'><br>", dat$country))

#plot
dat %>%
  ggplot(aes(fct_relevel(dat$country, dat$country), count, label = label)) +
  geom_bar(stat = "identity", fill = "#b22222", width = 0.8) +
  
  # Bar labels
  geom_text(nudge_y = 0.19, color = "#b22222", size = 24, family = "Arial Bold") +
  annotate("text", x = 8, y = 0.15, label = "NO DATA", color = "black", size = 8, family = "Arial") +
  
  # Flags
  # geom_flag(aes(image = code), y = -0.25, size = 0.1, asp = 2) +
  scale_x_discrete(labels = axis_labels) +
   
  # Annotation
  annotate("text", x = 6.95, y = 3.85, label = toupper("Source: UNODC Homicide Statistics"), size = 8.3, family = "Arial Bold", color = "grey35") + 
  
  # Title
  annotate("rect", xmin = -2, xmax = 10, ymin = 4.2, ymax = Inf, fill = "black") +
  annotate("text", x = -0.25, y = 4.5, label = toupper("Homicides Per 100,000 in G-8 Countries"), color = "white", size = 18.3, hjust = 0, family = "Arial Black") +
  
  # Axis
  ylab(toupper("# of gun-related homicides\nper 100,000 people")) +
  scale_y_continuous(breaks = 0:4, limits = c(0, 4.58)) +
  # Get creative with the breaks, they don't need to align with the gridlines or the actual values
  annotate("text", x = 0.4, y = c(0.6, 1.7, 2.85, 4), label = c(1, 2, 3, 4), size = 9.5, family = "Arial Bold") +
  coord_cartesian(xlim = c(1.1, 8), clip = 'off') +
  
  # Theme
  ggthemes::theme_economist() +
  theme(
    plot.background = element_rect(fill = "grey87", color = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 26, color = "grey20", family = "Arial Bold", hjust = 0.45, margin = margin(0, 20, 0, 0)),
    # Use NA color to "hide" the axis labels but keep the spacing right
    axis.text.y = element_text(size = 26, color = NA, family = "Arial Bold"),
    axis.text.x = element_markdown(size = 25, family = "Arial Bold", vjust = 1, margin = margin(-12, 0, 0, 0)),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey60", size = 2),
    plot.margin = margin(0, 30, 15, 20)
  ) 

ggsave(
      here::here("week-50", "plots", "temp", paste0("replicate-homicides", ".png")), dpi = 320, width = 17.78, height = 10
      )
 
