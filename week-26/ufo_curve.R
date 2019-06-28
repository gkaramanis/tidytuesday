library(here)
library(tidyverse)
library(tidytext)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

hoax <- ufo_sightings %>%
  mutate(hoax = ifelse(grepl("hoax", tolower(description)), 1, 0),
        n = 1:n()) %>% 
  # filter(grepl("hoax", tolower(description))) %>% 
  mutate(date_documented = as.Date(date_documented, "%m/%d/%Y"),
         date_time = as.Date(date_time, "%m/%d/%Y")) %>% 
  select(date_time, date_documented) %>%
  mutate(
    ym1 = format(date_time, "%Y%m"),
    ym2 = format(date_documented, "%Y%m")
    ) %>%
    group_by(ym1, ym2) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(
      ym1 = as.Date(paste(ym1, "01", sep = ""), "%Y%m%d"),
      ym2 = as.Date(paste(ym2, "01", sep = ""), "%Y%m%d")
    )

hoax %>% 
  # distinct(date_time, date_documented) %>% 
  # sample_n(1000) %>% 

ggplot() +
  geom_curve(aes(x = as.Date("1990-01-01"), y = ym1, 
                  xend = ym2, yend = as.Date("1900-01-01"),
                  size = n),
                  curvature = -0.4, color = "white",
            alpha = 0.1) +
  scale_size_continuous(range = c(0.1, 0.6)) +        
  scale_x_date(breaks = as.Date(c("1998-01-01", "2002-01-01",
                                  "2006-01-01", "2010-01-01",
                                  "2014-01-01")),
               date_labels = "%Y", expand = c(0,0)) +
  scale_y_date(breaks = as.Date(c("1920-01-01", "1940-01-01",
                                  "1960-01-01", "1980-01-01",
                                  "2000-01-01", "2014-01-01")),
               date_labels = "%Y", expand = c(0,0)) +
  labs(
    title = "UFO sightings reported to NUFORC",
    subtitle = "Date of sighting vs date documented",
    caption = "Source: NUFORC | Graphics: Georgios Karamanis",
    x = "Date documented",
    y = "Date of sighting"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    plot.margin = unit(c(1, 1, 0.6, 0.8),"cm"),
    plot.background = element_rect(fill = "#454c92"),
    plot.title = element_text(color = "#9fee98",
                              family = "IBM Plex Sans Bold"), 
    plot.subtitle = element_text(color = "grey90", 
                                 margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(family = "IBM Plex Sans ExtraLight",
                                color = "grey90",
                                margin = margin(30, 0, 0, 0)),
    axis.text = element_text(color = "#9fee98",
                             family = "IBM Plex Mono ExtraLight"),
    axis.title.x  = element_text(margin = margin(20, 0, 0, 0)),
    axis.title.y  = element_text(margin = margin(0, 20, 0, 0)),
    text = element_text(family = "IBM Plex Sans",
                        color = "white",
                        size = 12)
  ) +
   
  ggsave(here("week-26", "ufo_curve.png"), height = 9, width = 7)

