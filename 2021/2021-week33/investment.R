library(tidyverse)
library(camcorder)
library(ggforce)
library(scales)

# Datawrapper dispatch!!!

chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')

digital_inv <- chain_investment %>% 
  filter(str_detect(meta_cat, "Digital")) %>% 
  mutate(gross_inv_chain = gross_inv_chain * 1e6) # Convert values to million

gg_record(dir = "temp", device = "png", width = 9, height = 11, units = "in", dpi = 320)

options(scipen = 5) 

ggplot(digital_inv) +
  geom_line(aes(year, gross_inv_chain, group = category, color = category), size = 1) +
  scale_color_manual(values = c("#4363d8", "#f58231", "#3cb44b", "#800000")) +
  scale_y_continuous(breaks = c(0, 10e9, 20e9, seq(50e9, 150e9, by = 25e9)), sec.axis = dup_axis(), labels = label_dollar(rescale_large = rescale_short_scale()), minor_breaks = seq(0, 125e9, by = 2e9)) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, title = NULL)) +
  facet_zoom(x = year < 1990, ylim = c(0, 21e9), horizontal = FALSE) +
  labs(
    title = "Real Digital Infrastructure Investment",
    subtitle = "In chained (2012) dollars",
    caption = "Source: Bureau of Economic Analysis · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "Source Serif Pro") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.background = element_rect(fill = "grey95", color = NA),
    strip.background = element_rect(fill = "grey90", color = "grey70", size = 1),
    panel.background = element_rect(fill = "grey90", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 23, family = "Fira Sans", face = "bold", color = "#105585"),
    plot.subtitle = element_text(hjust = 0.5, size = 16, family = "Fira Sans", face = "bold", color = "#105585"),
    plot.caption = element_text(family = "Fira Sans", color = "grey40"),
    panel.grid.major.y = element_line(color = "grey50", size = 0.125),
    panel.grid.minor.y = element_line(color = "grey50", size = 0.05),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text = element_text(size = 12)
  )

# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # ffmpeg -i 2021_08_01_14_53_40.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" olympics_makingof.mp4

# ggsave(here::here("temp", paste0("investment-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
