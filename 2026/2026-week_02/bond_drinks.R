library(tidyverse)
library(waffle)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 6, units = "in", dpi = 320)

# Get Table 2 data from article at https://www.bmj.com/content/347/bmj.f7255
library(httr2)
library(rvest)

url <- "https://www.bmj.com/highwire/markup/750335/expansion?width=1000&height=500&iframe=true&postprocessors=highwire_figures%2Chighwire_math"

resp <- request(url) |> 
  req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36") |> 
  req_headers(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.5",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = "https://www.bmj.com/",
    "Cache-Control" = "no-cache"
  )  |> 
  req_options(
    followlocation = TRUE
  ) |> 
  req_perform()

table2_raw <- resp |> 
  resp_body_string(encoding = "UTF-8") |> 
  read_html() |> 
  html_element("table") |> 
  html_table()

table2 <- table2_raw |> 
  select(book = 1, alcohol_units = 4, weekly_incl_unable = 5, weekly_excl_unable = 6) |> 
  filter(!book %in% c("", "Total")) |> 
  mutate(
    across(c(-book, weekly_excl_unable), as.numeric),
    year = str_extract(book, "\\d{4}"),
    .after = 1,
    book = fct_reorder(book, year)
  )

f1 <- "Publico Headline"  
n_rows <- 3

ggplot(table2) +
  geom_waffle(aes(fill = I("#D4AF37"), values = weekly_excl_unable), n_rows = n_rows, size = 0.5, color = "#001D3E") +
  geom_text(aes(y = 2, x = weekly_excl_unable / n_rows, label = round(weekly_excl_unable)), color = "white", size = 6, fontface = "bold", nudge_x = 1.5, hjust = 0) +
  scale_x_continuous(limits = c(0, 150 / n_rows)) +
  coord_fixed(expand = FALSE, clip = "off") +
  facet_wrap(vars(book), ncol = 2, dir = "v") +
  labs(
    title = "James Bond's Dangerous Drinking Habit",
    subtitle = str_wrap("Bond averaged 92 units of alcohol per week across the 12 analysed novels—more than four times the 21 units per week recommended by UK health guidelines in 2013, when the study was conducted. This rate is calculated only for days when he was able to drink.", 130),
    caption = "Source: Were James Bond’s drinks shaken because of alcohol induced tremor? BMJ 2013;347:f7255 · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#001D3E", color = NA),
    strip.text = element_text(face = "bold", size = 12, hjust = 0, margin = margin(5, 0, 3, 0), color = "grey97"),
    plot.margin = margin(20, 20, 20, 20),
    panel.spacing.x = unit(2, "lines"),
    plot.title = element_text(color = "white", size = 20, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 12, margin = margin(t = 5, b = 30)),
    plot.caption = element_text(color = "white", size = 10, margin = margin(t = 20))
  )
  
record_polaroid()