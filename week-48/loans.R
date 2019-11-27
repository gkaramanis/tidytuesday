library(tidyverse)
library(viridis)
library(here)

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

outstanding_loans <- loans %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    agency_name = case_when(
      agency_name == "FMS" ~ "FMS Investment Corp",
      agency_name == "ACT" ~ "Account Control Technology, Inc.",
      agency_name == "GC Services" ~ "GC Services LP",
      agency_name == "Immediate Credit Recovery" ~ "Immediate Credit Recovery, Inc.",
      agency_name == "Pioneer" ~ "Pioneer Credit Recovery, Inc",
      agency_name == "Windham" ~ "Windham Professionals, Inc.",
      TRUE ~ agency_name
    ),
    agency_name = str_remove(agency_name, "\\*"),
    agency_name = str_replace(agency_name, "(?<!,) Inc$", ", Inc."),
    year_q = as.numeric(paste0(year, ".", quarter)),
    outstanding = starting + added - total
         )

ggplot(outstanding_loans) +
  geom_area(aes(year_q, outstanding, group = 1), fill = "black") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = c(15, 16, 17, 18), labels = c(2015, 2016, 2017, 2018)) +
  scale_y_continuous(labels = paste0(c(0, 5, 10, 15), "B")) +
  facet_wrap(vars(agency_name), strip.position = "bottom") +
  labs(
    title = "Outstanding student loan debt by collection agency, in billion dollars",
    subtitle = "",
    caption = "Source: Department of Education | Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "IBM Plex Mono") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", size = 6),
    axis.text = element_text(family = "IBM Plex Mono Bold"),
    axis.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "IBM Plex Mono Bold", size = 20),
    plot.subtitle = element_text(),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  ) +

ggsave(
      here::here("week-48", "plots", "temp", paste0("loans-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 10, width = 16
      )
 
