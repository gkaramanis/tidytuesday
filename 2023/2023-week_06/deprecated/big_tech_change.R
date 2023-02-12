library(tidyverse)
library(bdscale)
library(ggpath)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')

big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

stock <- big_tech_stock_prices %>% 
  left_join(big_tech_companies) %>% 
  group_by(date) %>% 
  ungroup() %>% 
  filter(between(date, as.Date("2017-01-01"), as.Date("2022-12-29"))) %>% 
  mutate(
    simple_name = tolower(stringi::stri_extract_first_words(company, 1)),
    simple_name = case_when(
      str_detect(simple_name, "international") ~ "ibm",
      str_detect(simple_name, "amazon") ~ "amazon",
      TRUE ~ simple_name
    ),
    path = here::here("2023/2023-week_06/img", paste0(simple_name, ".png"))
  )

pal <- MetBrewer::met.brewer("Tam", direction = -1)[1:8]
f1 <- "Outfit"

annot_y = "/Users/georgios/SynologyDrive/R/tidytuesday/2023/2023-week_06/img/alphabet.png"

stock_index <- stock %>% 
  mutate(path = fct_rev(path)) %>% 
  group_by(company) %>% 
  mutate(
    start_price = adj_close[1L],
    price_change = adj_close / start_price
  ) %>% 
  ungroup()

ggplot(stock_index) +
  geom_line(aes(date, price_change, color = company))


ggplot(stock_index) +
  geom_raster(aes(x = date, y = path, fill = price_change)) +
  # March 2020 stock market crash
  # Arrow annotation
  annotate("segment", xend = as.Date("2020-04-20"), x = as.Date("2020-09-18"), y = annot_y, yend = annot_y, color = "white", arrow = arrow(length = unit(0.3, "cm"))) +
  # Text annotation
  annotate("text", x = as.Date("2020-10-19"), y = annot_y, color = "white", label = "March 2020\nstock market crash", family = f1, hjust = 0, size = 3.5, lineheight = 0.9) +
  scale_x_bd(business.dates = stock$date, max.major.breaks = 10, labels = scales::date_format("%b %d, %Y"), expand = c(0.01, 0)) +
  scale_fill_gradientn(colors = pal,  guide = guide_colorbar(direction = "horizontal"), labels = scales::percent, breaks = seq(0, 30, 10), limits = c(0, 30)) +
  labs(
    title = "Big Tech Stock Prices, 2017–2022",
    caption = "*Closing price after adjustments for all applicable splits and dividend distributions · Showing only business days · Source: Yahoo Finance · Graphic: Georgios Karamanis",
    fill = "Change in adjusted* closing price\ncompared to price on January 03, 2007"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.715, 1.04),
    legend.key.width = unit(2.8, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.title = element_text(vjust = 1, hjust = 1, margin = margin(0, 10, 0, 0), lineheight = 1),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(margin = margin(5, 0, 0, 0)),
    axis.text.y = element_path(size = 0.9),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0.2, "line"),
    plot.margin = margin(20, 20, 10, 20),
    plot.title = element_text(size = 22, face = "bold", margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  )
