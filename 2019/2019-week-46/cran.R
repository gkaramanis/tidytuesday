library(tidyverse)
library(here)
library(viridis)
library(ggtern)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv") %>% 
  mutate(
    lang_id = as.numeric(factor(language)),
    pkg_id = as.numeric(factor(pkg_name)),
    major_v = as.numeric(str_match(version, ".|-(?=\\.)"))
    )

top10_lang <- cran_code %>% group_by(language) %>% summarise(tot = sum(code)) %>% top_n(10, tot)

ggtern(cran_code, aes(blank, code, comment)) +
  geom_point(aes(color = language, size = file), alpha = 0.5) +
  scale_size(range = c(1, 25), guide = "none") +
  scale_color_viridis_d(breaks = c("R", "C/C++ Header", "C", "HTML", "C++", "SVG", "JSON", "JavaScript", "Bourne Shell", "Fortran 77"), name = "top 10\nlanguages") +
  labs(
    title = "Distribution of code, blank and comment lines in CRAN packages, by major release number",
    subtitle = "Point color represents the language and point size the number of files for that language in a specific package",
    caption = "Source: CRAN via Philippe Massicotte | Graphic: Georgios Karamanis"
  ) +
  facet_wrap(vars(major_v), ncol = 5) +
  theme_linedraw(base_family = "IBM Plex Mono") +
  theme(
    axis.text = element_blank(),
    axis.title = element_text(size = 6),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(family = "IBM Plex Mono Bold"), 
    plot.subtitle = element_text(margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(20, 0, 0, 0))
  ) 

ggsave(
    here::here("week-46", "plots", "temp", paste0("cran-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 7, width = 17
  )
