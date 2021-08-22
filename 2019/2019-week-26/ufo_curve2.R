library(here)
library(tidyverse)
# library(tidytext)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

hoax <- ufo_sightings %>%
  # mutate(hoax = ifelse(grepl("hoax", tolower(description)), 1, 0),
  #      n = 1:n()) %>% 
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

ggplot(hoax) +
  geom_curve(aes(x = ym1, y = 5, 
                  xend = ym2, yend = 0,
                  size = n),
                  curvature = 0.1, color = "white",
            alpha = 0.1) +
  scale_size_continuous(range = c(0.1, 0.6)) +        
  scale_x_date(breaks = as.Date(c("1920-01-01", "1940-01-01", "1960-01-01",
                                  "1980-01-01", "2000-01-01", "2020-01-01")),
               date_labels = "%Y", expand = c(0,0),
               sec.axis = sec_axis(~ .)) +
  labs(
    title = "UFO sightings reported to NUFORC",
    subtitle = "Date the event took place vs date it was documented",
    caption = "Source: NUFORC | Graphics: Georgios Karamanis",
    x = "Date documented",
    y = "Date occurred"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "None",
    panel.grid = element_blank(),
    plot.margin = unit(c(1, 1, 0.6, 0.8),"cm"),
    plot.background = element_rect(fill = "#454c92", color = "#454c92"),
    plot.title = element_text(color = "#9fee98",
                              family = "IBM Plex Sans"), 
    plot.subtitle = element_text(color = "grey90", 
                                 margin = margin(0, 0, 30, 0)),
    plot.caption = element_text(family = "IBM Plex Sans ExtraLight",
                                color = "grey90",
                                margin = margin(30, 0, 0, 0)),
    axis.text = element_text(color = "#9fee98",
                             family = "IBM Plex Mono Light"),
    axis.title.x  = element_text(margin = margin(20, 0, 0, 0)),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "IBM Plex Sans",
                        color = "white",
                        size = 12)
  ) 

ggsave(here("week-26", "ufo_curve2.png"), height = 9, width = 5)

