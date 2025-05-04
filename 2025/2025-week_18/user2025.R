library(tidyverse)
library(scales)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 14, height = 8, units = "in", dpi = 320)

user2025 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-29/user2025.csv')

sched <- user2025 %>% 
  separate(time, into = c("time_start", "time_end"), sep = "–", remove = FALSE) %>% 
  mutate(
    time_start = as_datetime(hm(time_start)),
    time_end = as_datetime(hm(time_end)),
    x = time_start + (time_end - time_start) / 2,
    date_label = strftime(date, format = "%A %d %B %Y"),
    title_label = case_when(
      session == "Tutorial" ~ paste0(str_sub(title, 1, 40), "…"),
      TRUE ~ paste0(str_sub(title, 1, 28), "…")
    ),
    title_wrap = case_when(
      session == "Tutorial" ~ str_wrap(title_label, 60),
      TRUE ~ str_wrap(title_label, 20)
    )
  ) %>% 
  filter(time != "TBD") %>% 
  group_by(date, time) %>% 
  mutate(slot_id = row_number()) %>% 
  ungroup() %>% 
  group_by(session) %>% 
  mutate(
    session_id = as.numeric(factor(time)),
    session_x = mean(x),
    session_y = mean(slot_id)
    ) %>% 
  ungroup() 

f1 <- "Radio Canada"
f2 <- "Sofia Sans Extra Condensed"

pal <- c(MetBrewer::met.brewer("Austria"), MetBrewer::met.brewer("Java"), MetBrewer::met.brewer("Lakota"), MetBrewer::met.brewer("Moreau"))

annot <- tribble(
  ~date_label, ~x, ~y, ~label,
  "Friday 08 August 2025", as_datetime("1970-01-01 08:1:00"), 22, "useR! 2025",
  "Friday 08 August 2025", as_datetime("1970-01-01 08:1:00"), 20.5, "In-person conference program\n8-10 August 2025\nPenn Pavilion, Duke University\nDurham, North Carolina",
  "Friday 08 August 2025", as_datetime("1970-01-01 08:1:00"), 17, "Source: useR! 2025 · Graphic: Georgios Karamanis"
)

col1 <- "#3563B0"
col2 <- "#F1F3F6"
col3 <- "#3A456F"

ggplot() +
  # Session labels
  geom_text(data = sched %>% filter(time != "14:30–15:40"), aes(time_start - 900, session_y, label = session, color = session), stat = "unique", angle = 90, family = f2, fontface = "bold") +
  geom_text(data = sched %>% filter(time == "14:30–15:40"), aes(time_end + 900, session_y, label = session, color = session), stat = "unique", angle = -90, family = f2, fontface = "bold") +
  # Titles
  geom_rect(data = sched, aes(xmin = time_start, xmax = time_end, ymin = slot_id - 0.8, ymax = slot_id, fill = session, color = after_scale(col_darker(fill))), linewidth = 0.2) +
  geom_text(data = sched, aes(x = x, y = slot_id - 0.4, label = title_wrap), size = 2.8, color = "white", family = f2, lineheight = 0.8, fontface = "bold") +
  # Title, subtitle, caption
  geom_text(data = annot, aes(x = x, y = y, label = label), hjust = 0, size = c(9, 6, 4), family = f1, fontface = c("bold", "plain", "plain"), vjust = 1, lineheight = 0.9, color = col1) +
  # Scales, theme, etc
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_x_datetime(breaks = "1 hours", labels = time_format(format = "%H:%M")) +
  scale_y_continuous(expand = c(0.01, 0)) + 
  coord_cartesian(clip = "off") +
  facet_grid(cols = vars(date_label), scales = "free_x", space = "free_x") +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = col2, color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(color = c( "grey80", col3)),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing.x = unit(2, "lines"),
    panel.background = element_rect(fill = "grey99"),
    panel.border = element_rect(fill = NA, color = col3),
    strip.text = element_text(size = 12, face = "bold", color = col1),
    plot.margin = margin(10, 10, 10, 10)
  )
  

