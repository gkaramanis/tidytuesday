library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# https://www.fjc.gov/history/judges/biographical-directory-article-iii-federal-judges-export
judges_app <- read_csv(here::here(
  "2025/2025-week_24/data/federal-judicial-service.csv"
))

judges_dem <- read_csv(here::here(
  "2025/2025-week_24/data/judges_demographics.csv"
))

term_length <- 4

judges_appdem <- judges_app |>
  left_join(judges_dem) |>
  janitor::clean_names() |>
  group_by(appointing_president) |>
  mutate(
    first_year = min(year(nomination_date)),
    last_year = max(year(nomination_date)),
    terms = ceiling((last_year - first_year) / term_length)
    ) |>
  ungroup() |>
  filter(last_year > 1980)

judges_court <- judges_appdem |> 
  count(appointing_president, party_of_appointing_president, first_year, last_year, terms, court_type) |> 
  filter(str_detect(appointing_president, "None", negate = TRUE)) |> 
  filter(court_type != "Other") |> 
  mutate(
    appointing_president = case_when(
      terms == 2 ~ paste0(appointing_president, "*"),
      TRUE ~ appointing_president
    ),
    appointing_president = fct_reorder(appointing_president, last_year)
    ) |> 
  mutate(total = sum(n), .by = appointing_president)

f1 <- "Apple SD Gothic Neo"
f2 <- "JetBrains Mono"

# https://www.pewresearch.org/short-reads/2025/01/09/how-biden-compares-with-other-recent-presidents-in-appointing-federal-judges/

# By court type
ggplot(judges_court, aes(x = appointing_president, y = n, alpha = court_type, label = n, fill = party_of_appointing_president)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE), width = 0.75) +
  geom_text(data = judges_court |> filter(n > 20), stat = "identity", position = position_stack(reverse = TRUE, vjust = 0.5), family = f2, fontface = "bold", size = 7, color = "white", alpha = 1) +
  # Totals
  annotate("text", x = 8.75, y = Inf, label = "Total", size = 7, family = f1, fontface = "bold") +
  geom_text(aes(y = Inf, label = total), stat = "unique", family = f2, fontface = "bold", size = 7) +
  # Annotations
  annotate("text", x = 8.8, y = 0, label = "Supreme Court\njustices", lineheight = 0.85, hjust = 1) +
  annotate("text", x = 8.8, y = 33, label = "Appeals court\njudges", lineheight = 0.85) +
  annotate("text", x = 8.8, y = 130, label = "District court\njudges", lineheight = 0.85) +
  scale_fill_manual(values = c("Democratic" = "#013364", "Republican" = "#d30b0d"), name = "Party of Appointing President", guide = guide_legend(title = NULL)) +
  scale_alpha_ordinal(range = c(1, 0.6), guide = "none") +
  coord_flip(clip = "off") +
  scale_x_discrete(expand = expansion(add = c(0, 1))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.06))) +
  labs(
    title = paste0("Federal judges appointed by each president (", min(judges_court$first_year), "-", max(judges_court$last_year), ")"),
    caption = "* Served two terms.\nSource: Federal Judicial Center · Graphic: Georgios Karamanis",
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = "top",
    legend.justification = -0.25,
    legend.margin = margin(0, 0, 20, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.y = element_text(hjust = 1, face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 20, 0)),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, size = 11, margin = margin(20, 0, 10, 0)),
    plot.margin = margin(10, 40, 10, 15)
  )
  

# By gender
judges_gender <- judges_appdem |> 
  filter(court_type != "Other") |> 
  count(appointing_president, party_of_appointing_president, first_year, last_year, terms, gender) |> 
  filter(str_detect(appointing_president, "None", negate = TRUE)) |> 
  mutate(
    appointing_president = case_when(
      terms == 2 ~ paste0(appointing_president, "*"),
      TRUE ~ appointing_president
    ),
    appointing_president = fct_reorder(appointing_president, last_year)
  ) |> 
  mutate(
    total = sum(n), .by = appointing_president,
    fem_pct = round(n / total * 100)
    ) |> 
  filter(gender == "Female")


ggplot(judges_gender, aes(y = appointing_president, x = fem_pct, label = ifelse(fem_pct == max(fem_pct), paste0(fem_pct, "%"), fem_pct), fill = party_of_appointing_president)) +
  geom_col(width = 0.75, alpha = 0.8) +
  geom_text(nudge_x = 1, hjust = 0, family = f2, fontface = "bold", size = 7) +
  geom_text(aes(x = -16, label = n), hjust = 0, family = f2, fontface = "bold", size = 6) +
  # "Column" names
  annotate("text", x = c(-16, 0), y = 8.7, label = c("NUMBER\nOF FEMALE\nJUDGES\nAPPOINTED", "SHARE OF ALL\nJUDGES APPOINTED\nBY THAT PRESIDENT"), lineheight = 0.85, hjust = 0, vjust = 0, family = f1, fontface = "bold", size = 5, color = "grey60") +
  scale_fill_manual(values = c("Democratic" = "#013364", "Republican" = "#d30b0d"), name = "Party of Appointing President", guide = guide_legend(title = NULL)) +
  scale_x_continuous(limits = c(-20, 70)) +
  scale_y_discrete(expand = expansion(add = c(0, 1.5))) +
  coord_cartesian(clip = "off") +
  labs(
    title = paste0("Female federal judges appointed by each president (", min(judges_court$first_year), "-", max(judges_court$last_year), ")"),
    caption = "* Served two terms.\nSource: Federal Judicial Center · Graphic: Georgios Karamanis",
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = "top",
    legend.justification = -0.25,
    legend.margin = margin(0, 0, 20, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.y = element_text(hjust = 1, face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 20, 0)),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, size = 11, margin = margin(20, 0, 10, 0)),
    plot.margin = margin(10, 15, 10, 15)
  )
  