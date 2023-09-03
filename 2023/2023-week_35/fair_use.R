library(tidyverse)
library(ggparliament)
library(patchwork)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

fair_use_cases <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv')

f1 <- "Exetegue"
f2 <- "Outfit"

fu <- fair_use_cases %>% 
  count(fair_use_found) %>% 
  mutate(
    fair_use_found = if_else(fair_use_found == TRUE, "Fair", "Use"),
    fair_use_found = fct_rev(fair_use_found),
    pct = round(n / sum(n) * 100, 1),
    sum_pct = pct + lag(pct, default = 0),
    xmax = scales::rescale(sum_pct, from = c(0, 100), to = c(-1.5, 1.5))
    ) 

fu_parl <- parliament_data(election_data = fu,
                           type = "semicircle",
                           parl_rows = 8,
                           party_seats = fu$n)


p <- ggplot(fu_parl) +
  geom_parliament_seats(aes(x = x, y = y, colour = fair_use_found), size = 7) +
  scale_color_manual(values = c("orange", "purple3")) +
  scale_fill_manual(values = c("orange", "purple3")) +
  coord_fixed(ylim = c(0, 3)) +
  labs(
    title = "U.S. Fair Use Trials",
    subtitle = paste0("Out of the ", fu[1,2] + fu[2,2],  " cases tried from 1841 to 2022, ", fu[2,2], " (", fu[2,3], "%) were found to be Fair Use"),
    caption = "Source: U.S. Copyright Office Fair Use Index · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
    plot.subtitle = element_text( size = 14, hjust = 0.5),
    plot.caption = element_text(face = "bold", color = "#101820", hjust = 0.5),
    plot.margin = margin(0, 15, 0, 15)
  )

b <- ggplot(fu, aes(x = n, y = "Fair Use", fill = fair_use_found, color = after_scale(colorspace::darken(fill, 0.6)), label = n)) +
  geom_col() +
  shadowtext::geom_shadowtext(position = position_stack(0.5), family = f1, size = 40, bg.r = 0.02, bg.color = "grey99") +
  geom_text(aes(label = c("NOT FAIR USE", "FAIR USE")), position = position_stack(0.5), family = f1, size = 10, vjust = 3.5) +
  scale_x_reverse() +
  scale_fill_manual(values = c("orange", "purple3")) +
  coord_fixed(ratio = 15, clip = "off", expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none"
  )

p +
  inset_element(b, left = 0.1, right = 0.9, top = 0.9, bottom = 0.8)

# ggplot(fu, aes(x = n, y = "Fair Use", fill = fair_use_found, label = toupper(fair_use_found))) +
#   geom_col(color = "white", linewidth = 1.5) +
#   geom_text(aes(color = fair_use_found), position = position_stack(0.5), family = f1, size = 80) +
#   scale_fill_manual(values = c("orange", "purple3")) +
#   scale_color_manual(values = c("#101820", "#FEE715")) +
#   coord_fixed(ratio = 90, clip = "off", expand = FALSE) +
#   labs(
#     title = paste0("Out of the ", fu[1,2] + fu[2,2],  " cases tried,\n", fu[1,2], " (", fu[2,3], "%) were found to be Fair Use"),
#     caption = "Source: U.S. Copyright Office Fair Use Index · Graphic: Georgios Karamanis"
#   ) +
#   # theme_void(base_family = f2) +
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(fill = "grey99", color = NA),
#     plot.title = element_text(face = "bold", color = "purple3", size = 14),
#     plot.caption = element_text(face = "bold", color = "#101820"),
#     plot.margin = margin(0, 15, 0, 15)
#   )
#   
