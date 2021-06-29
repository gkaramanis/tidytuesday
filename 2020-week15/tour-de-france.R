library(tidyverse)
library(lubridate)
library(countrycode)
library(ggtext)
library(glue)
library(here)
#library(skimr)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

tdf_table <- tdf_winners %>% 
  mutate(
    wins_consecutive = with(rle(winner_name), rep(lengths, times = lengths)), # count consecutive wins for bold names
    year = year(start_date),
    year_labels = ifelse(year %% 10 == 0, glue("**{year}**"), year), # make year labels, every year ending in 0 is bold
    year_group = case_when(  # make 3 year groups, before/after World Wars
    year < 1915 ~ 1,
    year > 1915 & year < 1940 ~ 2,
    TRUE ~ 3),
    avg_speed = distance / time_overall,
    country_code = countrycode(nationality, origin = "country.name", destination = "iso3c"), # 3-letter country codes
    winner_annot = ifelse(wins_consecutive > 2, glue("**{winner_name} ({country_code})**"), glue("{winner_name} ({country_code})")) # make name labels, bold for winners with >2 consecutive wins
    ) %>%
  group_by(year_group) %>% 
  mutate(
    n_annot = row_number(),
    annot = ifelse((n_annot - 2) %% 3 == 0, TRUE, FALSE) # this is to annotate every third year in the plots with points and values, starting from the second year in every group
    ) %>% 
  ungroup() %>% 
  add_row(year = c(1915, 1916, 1917, 1918, 1940, 1941, 1942, 1943)) %>% # add dummy rows between the year groups
  arrange(year) %>% 
  mutate(n = row_number())

ggplot(tdf_table) +
# dotted gridlines ---------------------------------------------------
  geom_segment(data = subset(tdf_table, !is.na(year_labels)), aes(x = 0, xend = 24000, y = n, yend = n), linetype = "dotted", size = 0.2) +
# year plot ----------------------------------------------------------------
  geom_richtext(aes(x = -1000, y = n, label = year_labels), fill = "#F3F2EE", label.color = NA, label.padding = unit(0.1, "lines"), family = "JetBrains Mono", size = 2.5) +
  geom_richtext(aes(x = 25000, y = n, label = year_labels), fill = "#F3F2EE", label.color = NA, label.padding = unit(0.1, "lines"), family = "JetBrains Mono", size = 2.5) +
# distance plot -------------------------------------------------------------
  geom_area(aes(x = distance * 0.625, y = n, group = year_group), fill = "#7DDDB6", alpha = 0.6, orientation = "y", position = "identity") +
  geom_point(data = subset(tdf_table, annot), aes(x = distance * 0.625, y = n), size = 0.5) +
  geom_label(data = subset(tdf_table, annot), aes(x = distance * 0.625 + 100, y = n, label = distance), fill = "#F3F2EE", label.size = 0, label.padding = unit(0.1, "lines"), hjust = 0, family = "JetBrains Mono", size = 2.5) +
# names list -----------------------------------------------------------------
  geom_richtext(aes(x = 5300, y = n, label = winner_annot, .na = NULL), fill = "#F3F2EE", label.size = 0, label.padding = unit(0.1, "lines"), hjust = 0, family = "JetBrains Mono", size = 2.5) +
# teams list -----------------------------------------------------------------
  geom_label(aes(x = 10300, y = n, label = glue("{winner_team}", .na = NULL)), fill = "#F3F2EE", label.size = 0, label.padding = unit(0.1, "lines"), hjust = 0, family = "JetBrains Mono", size = 2.5) +
# average speed plot --------------------------------------------------------
  geom_segment(aes(x = 16000, xend = 16000 + avg_speed * 66.67, y = n, yend = n), size = 2, colour = "#7DDDB6", alpha = 0.6) +
  geom_label(data = subset(tdf_table, annot), aes(x = 16000 + avg_speed * 66.67 + 100, y = n, label = round(avg_speed, 1)), fill = "#F3F2EE", label.size = 0, label.padding = unit(0.1, "lines"), hjust = 0, family = "JetBrains Mono", size = 2.5) +
# total time plot ---------------------------------------------------------
  geom_ribbon(aes(xmin = 20000, xmax = 20000 + time_overall * 10, y = n, group = year_group), fill = "#FCDF33", alpha = 0.6, orientation = "y", position = "identity") +
  geom_point(data = subset(tdf_table, annot), aes(x = 20000 + time_overall * 10, y = n), size = 0.5) +
  geom_label(data = subset(tdf_table, annot), aes(x = 20000 + time_overall * 10 + 100, y = n, label = round(time_overall, 1)), fill = "#F3F2EE", label.size = 0, label.padding = unit(0.1, "lines"), hjust = 0, family = "JetBrains Mono", size = 2.5) +
# custom annotations, lines and boxes --------------------------------------
  annotate("segment", x = c(-2000, 0, 5000, 10000, 16000, 20000, 24000, 26000), xend = c(-2000, 0, 5000, 10000, 16000, 20000, 24000, 26000), y = -4, yend = 115, size = 0.3) +
  annotate("segment", x = -2000, xend = 26000, y = c(-4, -1, 115), yend = c(-4, -1, 115), size = 0.3) +
  annotate("text", x = c(-1000, 2500, 7500, 13000, 18000, 22000, 25000), y = -2.5, label = toupper(c("year", "distance", "winner", "team", "average speed", "total time", "year")), hjust = 0.5, family = "IBM Plex Sans Bold", size = 3.5) +
  annotate("rect", xmin = -2000, ymin = c(13, 38), xmax = 26000, ymax = c(16, 41), fill = "#F3F2EE", colour = "black", size = 0.3) +
  annotate("richtext", x = 13000, y = c(14.5, 39.5), label = c("**1915-1918** Tour suspended because of Word War I", "**1940-1946** Tour suspended because of Word War II"), label.color = NA, fill = "#F3F2EE", hjust = 0.5, family = "IBM Plex Sans", size = 3.5) +
  annotate("text", x = c(100, 4900), y = 0, label = c("0", "8000 km"), hjust = c(0, 1), family = "IBM Plex Mono Light", size = 3) +
  annotate("text", x = c(16100, 19900), y = 0, label = c("0", "60 km/h"), hjust = c(0, 1), family = "IBM Plex Mono Light", size = 3) +
  annotate("text", x = c(20100, 23900), y = 0, label = c("0", "300 h"), hjust = c(0, 1), family = "IBM Plex Mono Light", size = 3) +
  annotate("text", x = 26000, y = -6, label = "Source: alastairrushworth/tdf & kaggle.com/jaminliu | Graphic: Georgios Karamanis", hjust = 1, family = "IBM Plex Mono Light", size = 3) +
# scales and theme ----------------------------------------------------------
  coord_cartesian(clip = 'off') +
  scale_x_continuous(limits = c(-2300, 26300), expand = expansion(add = 1)) +
  scale_y_reverse(expand = expansion(add = 0)) +
  labs(
    title = "Tour de France Winners"
    ) +
  theme_void(base_family = "JetBrains Mono") +
  theme(
    plot.background = element_rect(fill = "#F3F2EE", colour = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.01, size = 28, family = "IBM Plex Sans Bold", margin = margin(0, 0, -8, 0))
    ) 

ggsave(here::here("2020-week15", "plots", "temp", paste0("tour-de-france", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 11, height = 15)
 