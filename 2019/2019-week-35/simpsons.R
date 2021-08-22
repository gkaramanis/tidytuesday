library(tidyverse)
library(here)
library(ggtext)

simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

sim <- simpsons %>%
  # I want no movie
  filter(season != "Movie") %>% 
  # style for highlighted guest stars
  mutate(
    guest_star = case_when(
      str_detect(tolower(role), "himself|herself|themselves") ~ paste0("<span style='color:black'>", guest_star, "</span>"),
      # style for others
      TRUE ~ paste0("<span style='color:#D1B271'>", guest_star, "</span>")
    )
  ) %>%
  # keep unique combinations
  distinct(guest_star, role)

# concatenate all the names
all_guests <- paste(sim$guest_star, collapse = " • ")

# plot
ggplot() +
  # set color to NA to hide the box (it hides unstyled text, too!)
  geom_textbox(aes(label = all_guests, 1, 2), 
               color = NA, fill = NA,
               height = 0.9, width = 0.845,
               box.padding = unit(c(0, 0, 0, 0), "pt"),
               hjust = 0.5,
               family = "Akura Popo") +
    labs(
      title = "The Simpsons Guest Stars",
      subtitle = "Unique guest-role combinations, in order of first appearance. Highlighted are the guest voices that played themselves.",
      caption = "Source: Wikipedia via Andrew Collier | Graphic: Georgios Karamanis"
    ) +
    lims(x = c(0, 2), y = c(0, 2)) +
    theme_void(base_family = "Akura Popo") +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "#FED90F", color = "#FED90F"),
      plot.title = element_text(size = 32, hjust = 0.5, margin = margin(40, 0, 0, 0)),
      plot.subtitle = element_text(size = 24, hjust = 0.5),
      plot.caption = element_text(size = 12, hjust = 0.9,
                                  margin = margin(30, 30, 40, 0))
    ) 

ggsave(
  here::here("week-35", "figures", "simpsons.png"),
  height = 13, width = 18, dpi = 320
)
