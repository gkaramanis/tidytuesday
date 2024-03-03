library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 11, height = 8, units = "in", dpi = 320)

# events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/events.csv')
births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')

births_deaths <- births %>% 
  rename(description_birth = description) %>% 
  left_join(deaths %>% rename(description_death = description)) %>% 
  mutate(
    person = fct_reorder(person, year_birth),
    i = row_number(),
    leap_birth = leap_year(year_birth),
    leap_death = leap_year(year_death),
    both_leap = leap_birth & leap_death
    )

both_leap <- births_deaths %>% 
  filter(both_leap) %>% 
  mutate(ii = row_number())

leap_lives <- both_leap %>% 
  rowwise() %>% 
  mutate(
    year_range = list(year_birth:year_death),
    age = year_death - year_birth
  ) %>% 
  ungroup() %>% 
  unnest(year_range) %>% 
  mutate(is_leap = leap_year(year_range)) %>% 
  group_by(person) %>% 
  mutate(leap_years_lived = sum(is_leap)) %>% # Not really lived but whatever
  ungroup()

f1 <- "Outfit"
f2 <- "Domine"

ggplot(leap_lives) +
  geom_segment(aes(x = year_birth, xend = year_death, y = person, color = if_else(!is.na(description_death), "coral2", "purple4")), stat = "unique", size = 3, lineend = "round") +
  geom_point(data = . %>% filter(is_leap), aes(x = year_range, y = person), size = 1.1, color = "white") +
  # Name and info
  ggtext::geom_textbox(aes(x = year_birth, y = person, label = paste0("**", person, "** (", year_birth, "-", year_death, ")<br>", description_birth)), hjust = 1, halign = 1, nudge_x = -5, family = f1, stat = "unique", size = 3.5, lineheight = 0.97, fill = NA, box.size = 0, width = unit(3.4, "inches")) +
  # Leap years lived
  ggtext::geom_richtext(aes(x = year_death, y = person, label = paste0("Lived ", age, " years,<br> or **", leap_years_lived, "** leap years")), hjust = 0, nudge_x = 5, family = f1, stat = "unique", size = 3.5, fill = NA, label.size = 0, lineheight = 1.1) +
  # Title, subtitle, caption
  annotate("text", x = 1428, y = 5.7, label = "Born on a leap day,\ndead on a leap year", family = f2, hjust = 0, fontface = "bold", size = 9, lineheight = 0.9, color = "purple4") +
  annotate("text", x = 1428, y = 3.1, label = paste0(str_wrap("Out of the 121 people listed on Wikipedia as being born on a leap day, 14 died during a leap year. James Milne Wilson is the only person on the list who was both born and died on a leap day.", 42), "\n\nSource: Wikipedia · Graphic: Georgios Karamanis"), family = f2, hjust = 0, size = 4.5, lineheight = 1) +
  annotate("text", x = 1428, y = 1.5, label = "*Ages and leap years lived are approximate, calculated from birth and death year", family = f2, hjust = 0, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)), breaks = seq(1500, 2000, 100)) +
  scale_y_discrete(limits = rev) +
  scale_color_identity() +
  theme_minimal(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 0, 15, 0)
  )

