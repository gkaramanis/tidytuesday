library(tidyverse)
library(waffle)
library(cowplot)
library(ggtext)

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')

peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

deaths <- members %>% 
  filter(died) %>% 
  mutate(
    expedition_role = if_else(expedition_role == "H-A Worker", "guides", "climbers"),
    death_cause = case_when(
      death_cause == "Unknown" ~ "Other and unknown",
      death_cause == "Other" ~ "Other and unknown",
      death_cause == "Falling rock / ice" ~ "Falling rock or ice",
      death_cause == "Crevasse" ~ "Fall into a crevasse",
      death_cause == "Disappearance (unexplained)" ~ "Disappearance",
      death_cause == "Exposure / frostbite" ~ "Exposure",
      death_cause == "Illness (non-AMS)" ~ "Illness",
      death_cause == "AMS" ~ "Acute mountain sickness",
      TRUE ~ death_cause
    )
    ) %>% 
  count(death_cause, expedition_role) %>% 
  group_by(death_cause) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  mutate(death_cause = fct_reorder(death_cause, total))

deaths_1 <- deaths %>% 
  filter(total < 50)

deaths_2 <- deaths %>% 
  filter(total > 50 & total < 200)

deaths_3 <- deaths %>% 
  filter(total > 200)

by_peak <- members %>% 
  group_by(peak_name) %>% 
  mutate(climbers = n()) %>% 
  add_count(died) %>% 
  filter(died) %>%
  mutate(rate = n / (climbers + n) * 100) %>% 
  ungroup() %>% 
  distinct(peak_name, n, rate) %>% 
  slice_max(rate, n = 8) %>% 
  left_join(peaks) %>% 
  mutate(peak_name = fct_reorder(peak_name, rate))

f1 = "Proxima Nova"

waf_plot <- function(data, rows){
  ggplot(data) +
    geom_waffle(aes(values = n, fill = expedition_role), n_rows = rows, color = "grey97") +
    geom_richtext(data = subset(data, expedition_role == "climbers"), aes(x = 1, y = Inf, label = paste0("**", n, "**", " climbers<br>")), hjust = 0,  vjust = 0, lineheight = 0.9, size = 3.5, family = f1, color = "grey10", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
    geom_richtext(data = subset(data, expedition_role == "guides"), aes(x = 51, y = Inf, label = paste0("**", n, "**", " guides<br>")), hjust = 1, vjust = 0, lineheight = 0.9, size = 3.5, family = f1, color = "#4CACEA", fill = NA, label.color = NA, label.padding = grid::unit(rep(0, 4), "pt")) +
    scale_fill_manual(
      name = NULL,
      values = c("grey10", "#4CACEA"),
      labels = c("climbers", "guides")
    ) +
    xlim(0, 52) +
    coord_fixed(clip = "off") +
    facet_wrap(vars(death_cause), ncol = 1, labeller = labeller(death_cause = toupper)) +
    theme_void() +
    theme(
      legend.position = "none",
      strip.text = element_text(margin = margin(10, 0, 8, 0), family = f1, size = 11),
      plot.background = element_rect(fill = "grey97", color = NA)
      )
}

plot_1 <- waf_plot(data = deaths_1, rows = 1)
plot_2 <- waf_plot(data = deaths_2, rows = 2)
plot_3 <- waf_plot(data = deaths_3, rows = 8)
plot_3b <- add_sub(plot_3, "Source: The Himalayan Database | Graphic: Georgios Karamanis", size = 8, fontfamily = f1, color = "grey20")


plot_peaks <- ggplot(by_peak) +
  geom_col(aes(peak_name, rate), width = 0.3, fill = "grey10") +
  geom_text(aes(peak_name, rate + 3, label = paste(round(rate, 1), "%")), family = f1) +
  geom_text(aes(peak_name, 2.5, label = n), family = f1, color = "grey97") +
  geom_text(aes(peak_name, -4, label = paste0(peak_name, "\n", height_metres, " m")), family = f1, lineheight = 0.9, size = 3) +
	annotate("text", x = 0.7, y = 30, label = toupper("Death rates and\ntotal deaths by peak\n1905-2019"), family = f1, hjust = 0, lineheight = 0.9) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(15, 0, 0, 0)
    )

title <- ggdraw() +
  draw_label(x = 0.045, "Deaths of climbers compared to guides", hjust = 0, fontfamily = f1, fontface = "bold", size = 20) +
	theme(plot.margin = margin(15, 0, 0, 5))

plot_grid(plot_peaks, title, plot_1, plot_2, plot_3b, ncol = 1, rel_heights = c(1.2, 0.3, 2.2, 0.9, 1.7)) +
  theme(plot.background = element_rect(fill = "grey97", color = NA)) 

ggsave(here::here("temp", paste0("himalayan-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, height = 12, width = 7)

