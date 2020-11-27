library(tidyverse)

hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

trails <- hike_data %>% 
  mutate(name = str_trim(name)) %>% 
  separate(length, into = c("length", "roundtrip"), sep = ", ") %>% 
  mutate(
    length = parse_number(length) * 1609.34,  # miles to metres
    gain = parse_number(gain) * 0.3048,  # feet to metres
    highpoint = parse_number(highpoint) * 0.3048, # feet to meres
    rating = as.numeric(rating),
    location = str_remove(location, " -- .+"),
    bin_length = as.numeric(cut(length, 5)), # bin length
    bin_highpoint = as.numeric(cut(highpoint, 5)) # bin highpoint
  )

length_plot <- trails %>% 
  count(bin_length) %>% 
  mutate(
    freq_bin = bin_length / 5,
    freq_n = n / sum(n),
    freq_n_cumsum = cumsum(freq_n),
    freq_n_cumsum_lag = lag(cumsum(freq_n), default = 0),
  ) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(freq_bin - 0.2, freq_bin - 0.2, freq_bin, freq_bin,
               freq_n_cumsum, freq_n_cumsum, freq_n_cumsum_lag, freq_n_cumsum_lag)),
    y = list(c(0.7, 0.8, 0.8, 0.7,
               -0.1, -0.2, -0.2, -0.1))
    ) %>% 
  unnest(c(x, y))

highpoint_plot <- trails %>% 
  count(bin_highpoint) %>% 
  mutate(
    freq_bin = bin_highpoint / 5,
    freq_n = n / sum(n),
    freq_n_cumsum = cumsum(freq_n),
    freq_n_cumsum_lag = lag(cumsum(freq_n), default = 0),
  ) %>% 
  rowwise() %>% 
  mutate(
    x = list(1.25 + c(-0.1, -0.2, -0.2, -0.1,
                     0.7, 0.8, 0.8, 0.7)),
    y = list(0.9 + c(freq_bin - 0.2, freq_bin - 0.2, freq_bin, freq_bin,
               freq_n_cumsum, freq_n_cumsum, freq_n_cumsum_lag, freq_n_cumsum_lag)),
  ) %>% 
  unnest(c(x, y))

wa_palette <- c("#008457", "#000000", "#FFD520", "#34C2DE", "#FDD6C6")

ggplot() +
  geom_polygon(data = length_plot, aes(x, y, group = bin_length, fill = as.factor(bin_length)), color = "#102F29", size = 0.25) +
  geom_polygon(data = highpoint_plot, aes(x, y, group = bin_highpoint, fill = as.factor(bin_highpoint)), color = "#102F29", size = 0.25) +
  annotate("text", x = 0.5, y = 0.835, label = "TRAIL LENGTH (km)", family = "Graphik Compact", color = "#91A94A") +
  annotate("text", x = c(0, 1), y = 0.835, label = c(round(min(trails$length)/1000), round(max(trails$length)/1000)), family = "Graphik Compact", color = "#91A94A", hjust = c(0, 1)) +
  annotate("text", x = 1.01, y = 1.35, label = "TRAIL HIGHPOINT (m)", angle = 90, family = "Graphik Compact", color = "#91A94A") +
  annotate("text", x = 1.01, y = c(0.9, 1.9), label = c(round(min(trails$highpoint)), round(max(trails$highpoint))), family = "Graphik Compact", color = "#91A94A", hjust = c(0, 1), angle = 90) +
  annotate("text", x = 0, y = 1.4, label = "WAS\nHING\nTON", family = "Futura Bold", size = 30, lineheight = 0.8, hjust = 0, color = "#91A94A") +
  annotate("text", x = 2.1, y = 0.32, label = "TRA\nILS", family = "Futura Bold", size = 48, lineheight = 0.8, hjust = 1, color = "#91A94A") +
  annotate("text", x = 0.5, y = -0.14, label = "98% of the trails are shorter than 48 km", family = "Futura Bold", color = "grey95", size = 4) +
  annotate("text", x = 2.01, y = 0.99, label = "The highpoint of 34%\nof the trails is under 750 m", family = "Futura Bold", color = "grey95", hjust = 1, size = 4) +
  annotate("text", x = 2.01, y = 1.62, label = "The highpoint of\n35% of the trails is\nbetween 1500 and 2250 m", family = "Futura Bold", color = "grey20", hjust = 1, size = 4) +
  annotate("text", x = 1.6, y = -0.188, label = "Source: Washington Trails Association | Graphic: Georgios Karamanis", color = "#889B4D", size = 3, family = "Graphik Compact") +
  scale_fill_manual(values = wa_palette) +
  coord_fixed() +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#102F29", color = NA)
  ) +
  ggsave(here::here("temp", paste0("washington-hiking-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 9)

