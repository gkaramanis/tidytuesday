library(tidyverse)
library(here)

r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

rfds <- r4ds_members %>%
  mutate(
    yearmonth = format(as.Date(date), "%Y-%m"),
    day = format(as.Date(date), "%d"),
    month = format(as.Date(date), "%m"),
    year = format(as.Date(date), "%Y"),
    ) %>%
  select(date, yearmonth, day, month, year,
         messages_in_public_channels)

# Top day with most messages
annot <- rfds %>%
  group_by(yearmonth) %>%
  top_n(n = 1)

# Office hours
office_hours <- read_tsv(here("week-29", "office_hours.tsv"))
office_hours <- office_hours %>%
  mutate(
    yearmonth = format(as.Date(date), "%Y-%m"),
    day = format(as.Date(date), "%d"),
    month = format(as.Date(date), "%m"),
    year = format(as.Date(date), "%Y"),
    ) %>%
    left_join(., rfds)

# Facet labels
m_labels <- c("01" = "January", "02" = "February",
              "03" = "March", "04" = "April",
              "05" = "May", "06" = "June",
              "07" = "July", "08" = "August",
              "09" = "September", "10" = "October", 
              "11" = "November", "12" = "December",
              "2017" = "2017", "2018" = "2018", "2019" = "2019") 

ggplot(rfds) +

  # Office hours
  geom_segment(data = office_hours,
               aes(x = day, xend = day, y = 200, yend = messages_in_public_channels),
               size = 0.3, color = "#36C5F0", alpha = 0.25) +
  # geom_point(data = office_hours, aes(day, messages_in_public_channels),
  #           alpha = 0.7, color = "#36C5F0", size = 1.2) +
  
  # Messages
  geom_line(aes(day, messages_in_public_channels,
                group = fct_rev(yearmonth)),
            color = "white", size = 0.3) +
            
  # Top days
  geom_point(data = annot, aes(day, messages_in_public_channels),
   color = "#ECB22E", size = 0.7) +
  # geom_segment(data = annot, aes(x = day, y = messages_in_public_channels, xend = day, yend = 0),
  #  color = "#ECB22E", size = 0.7) +
  labs(
    title = "R for Data Science Online Learning Community: Messages in public channels on Slack",
    subtitle = "The yellow points indicate the day of the month with the most messages and the blue lines the office hours",
    caption = "source: R4DS Slack | graphic: Georgios Karamanis"
  ) +
  facet_grid(month ~ year, labeller = as_labeller(m_labels)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#4A154B",
                                   color = "#4A154B"),
    plot.margin = margin(20, 30, 20, 30),
    strip.text = element_text(family = "IBM Plex Sans Light",
                              color = "grey90"),
    plot.title = element_text(family = "IBM Plex Sans Light",
                              color = "white", hjust = 1),
    plot.subtitle = element_text(family = "IBM Plex Sans Light",
                                 color = "white", hjust = 1,
                                 margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(family = "IBM Plex Sans Thin",
                                color = "grey90",
                                hjust = 0, margin = margin(25, 0, 0, 0))                      
  ) 

ggsave(here("week-29", "r4ds.png"),
         width = 9, height = 9, dpi = 300)
  
# Slack palette
# ECB22E yellow
# 36C5F0 blue
# 2EB67D green
# E01E5A red
# 4A154B aubergine
