library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9.7, height = 11, units = "in", dpi = 320)

prizes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv')

prizes |> 
  count(person_role, ethnicity_macro) |> 
  group_by(person_role) |> 
  mutate(pct = n / sum(n) * 100) |> 
  filter(ethnicity_macro %in% c("White British", "Non-UK White")) 

prizes |> 
  count(person_role, gender) |> 
  group_by(person_role) |> 
  mutate(pct = n / sum(n) * 100) |> 
  filter(gender %in% c("man", "woman")) 

f1 <- "Outfit"
f2 <- "Publico Text"

prizes |> 
  count(gender, ethnicity_macro, person_role) |>
  ggplot(aes(x = str_to_sentence(gender), y = str_wrap(ethnicity_macro, 10), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "white", size = 5) +
  MetBrewer::scale_fill_met_c("Tam") +
  facet_wrap(vars(str_to_sentence(person_role)), ncol = 2) +
  labs(
    title = "UK literary prizes favor White British authors and men",
    subtitle = "White British authors account for 61% of winners vs. 46% shortlisted. Men win 56% vs. 46% shortlisted.",
    caption = "Data: Post45 Data Collective · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 15) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 13),
    strip.text = element_text(size = 14, face = "bold", family = f2),
    panel.grid = element_line(linetype = "dotted", color = "grey70"),
    plot.title = element_text(size = 22, face = "bold", family = f2, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title.position = "plot",
    plot.margin = margin(20, 20, 20, 20),
    plot.caption = element_text(margin = margin(t = 15), hjust = 0.5)
  )

record_polaroid()
