library(tidyverse)
library(here)
library(wesanderson)
library(colorspace)
library(lemon)

cpu <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")

# calculate density for all CPUs
cpu <- cpu %>%
  mutate(density = transistor_count/area)

# subset SoC
soc <- cpu %>%
  filter(str_detect(processor, "SoC|Medfield|Freedom"))

# remove SoC from big data set
cpu_nosoc <- anti_join(cpu, soc)

# list of SoC designers and date of last SoC, used for annotations 
soc_annot <- soc %>%
  group_by(designer) %>%
  top_n(1, date_of_introduction) %>%
  distinct(designer, date_of_introduction)

# color palette
cols <- c("#e41a1c","#377eb8","#4daf4a","#f781bf","#ff7f00","#ffff33","#a65628","#f781bf", "#cab2d6")

# plot
ggplot() +
  # plot all CPUs (no SoC) in grey
  geom_tile(data = cpu_nosoc, aes(x = date_of_introduction,
                y = designer,
                width = sqrt(area)/10,
                height = sqrt(area)/10,
                alpha = density),
                color = "grey30", fill = "grey40") +
  # plot SoC in color
  geom_tile(data = soc, aes(x = date_of_introduction,
                y = designer,
                width = sqrt(area)/10,
                height = sqrt(area)/10,
                alpha = density, fill = designer, color = designer)) +
  # annotations for SoC      
  geom_text(data = soc_annot,
            aes(label = designer,
                x = date_of_introduction + 6,
                y = designer, color = designer),
            hjust = 0, family = "IBM Plex Mono Medium", size = 8) +
  geom_segment(data = soc_annot,
               aes(x = date_of_introduction + 2,
                   xend = date_of_introduction  + 5,
                   y = designer,
                   yend = designer,
                   color = designer)) +
  # annotations for all CPUs
  # geom_text(data = cpu, aes(label = designer,
  #                           x = 1977,
  #                           y = designer),
  #           hjust = 0, family = "IBM Plex Mono Light", size = 6, color = "grey50") +
  # axes
  scale_x_continuous(limits = c(1967, 2030), breaks = c(seq(1970, 2010, 10), 2019), labels = c(seq(1970, 2010, 10), 2019)) +
  coord_capped_cart(bottom = "both") +
  scale_y_discrete(expand = expand_scale(add = c(2, 0))) +
  # scales
  scale_alpha(range = c(0.1, 0.75)) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = colorspace::darken(cols, 0.3)) +
  # title, etc
  labs(
    title = "What will the future of SoC be after the upcoming end of Moore's law?",
    subtitle = "Because of the drive of Moore's law, SoC (system-on-chip) has been very popular in the past 10+ years. With the upcoming end\nof Moore's law it is becoming increasingly difficult and costly to reduce the feature size, in order to manufacture the SoC. The\nplot shows SoCs (color) against CPUs (grey) by various designers, by year of introduction. The area of the square denotes the\narea of the CPU/SoC and transparency indicates transistor density",
    caption = "Source: Wikipedia via r/dataisbeautiful | Graphic: Georgios Karamanis"
  ) +
  # theme
  theme_minimal(base_family = "IBM Plex Sans", base_size = 20) +
    theme(
      legend.position = "none",
      plot.margin = margin(30, 20, 60, 0),
      plot.background = element_rect(fill = "grey15", color = NA),
      panel.grid = element_line(color = "grey10"),
      axis.text.x = element_text(size = 20, family = "IBM Plex Mono", color = "grey70"),
      axis.text.y  = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(family = "IBM Plex Serif Medium", color = "grey85", size = 32),
      plot.subtitle = element_text(color = "grey80", margin = margin(0, 0, 40, 0)),
      plot.caption = element_text(family = "IBM Plex Serif Light", color = "grey75", margin = margin(40, 0, 0, 0))
    ) +
  # save image
  ggsave(
  here::here("week-36", "figures", "temp", paste0("moore_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  height = 16, width = 22, dpi = 320
)
