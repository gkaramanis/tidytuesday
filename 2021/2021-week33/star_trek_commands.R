library(tidyverse)
library(camcorder)
library(shadowtext)
library(colorspace)

gg_record(dir = "temp", device = "png", width = 9, height = 10, units = "in", dpi = 320)

computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

# Code adapted from https://www.r-bloggers.com/2018/12/rrrrs-in-r-letter-frequency-in-r-package-names/

char_frequencies <- computer %>% 
  select(-(type:sub_domain)) %>% # keep unique lines
  distinct() %>% 
  mutate(
    interaction = str_remove_all(interaction, "\\(.+?\\)"), # remove text within parentheses (is not dialog)
    interaction = str_remove_all(tolower(interaction), "computer") # remove wake word
    ) %>%
  group_by(char_type) %>% 
  summarise(split_char = unlist(str_split(tolower(interaction), ""))) %>%
  filter(split_char %in% letters) %>% 
  count(split_char) %>% 
  mutate(freq = 100 * prop.table(n))

# http://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
eng_freq <- read_csv(here::here("2021", "2021-week33", "data", "eng_letter_freq.csv")) %>% 
  rename(avg_freq = frequency)

freq_plot <- char_frequencies %>% 
  left_join(eng_freq, by = c("split_char" = "letter")) %>% 
  mutate(
    pct_diff = avg_freq - freq,
    split_char = fct_rev(split_char),
    annot = if_else(split_char == "e" & char_type == "Computer", "average frequency  ➙", "")
    )


f1 = "Piazzolla"
f2 = "Aquarius"
pos_col = "darkorange"
neg_col = "purple"
bg_col = "#E9ECEF"

ggplot(freq_plot) +
  # freq
  geom_point(aes(x = freq, y = split_char,
                 fill = if_else(freq > avg_freq, pos_col, neg_col),
                 color = after_scale(lighten(fill)),
                 size = abs(pct_diff),
                 stroke = abs(pct_diff)),
             shape = 21) +
  geom_segment(aes(x = avg_freq, xend = freq,
                   y = split_char, yend = split_char,
                   color = if_else(freq > avg_freq, pos_col, neg_col)),
               size = 1) +
  # avg_freq
  geom_point(aes(x = avg_freq, y = split_char),
             fill = bg_col, shape = 21, size = 2,
             stroke = 1, color = "grey70") +
  # letters
  geom_shadowtext(aes(x = freq, y = split_char, label = split_char),
                  size = 5, family = f1, fontface = "bold",
                  color = bg_col, vjust = 0.35, bg.colour = "grey30") +
  # annotations
  geom_text(aes(x = avg_freq, y = split_char, label = annot), hjust = 1, nudge_x = -0.5, family = f1, color = "grey25") +
  scale_color_identity() +
  scale_fill_identity() +
  scale_size_continuous(range = c(3, 14)) +
  scale_x_continuous(limits = c(0, 15), minor_breaks = 0:15, expand = c(0, 0)) +
  coord_fixed(clip = "off") +
  facet_wrap(vars(char_type)) +
  labs(
    title = "Speech interactions in Star Trek: The Next Generation",
    subtitle = str_wrap("Percentage frequency of letters in lines talked by computer and people (with the word 'computer' removed) compared to the English language average as calculated by the Cornell Math Explorers Club", 90),
    caption = "Source: SpeechInteraction.org, http://pi.math.cornell.edu/~mec\nGraphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", linetype = "dotted"),
    panel.grid.major.x = element_line(color = "grey50", size = 0.125),
    panel.grid.minor.x = element_line(color = "grey60", size = 0.05),
    panel.spacing.x = unit(2, "lines"),
    plot.title = element_text(size = 13.5, hjust = 0.5, family = f2, face = "bold"),
    plot.subtitle = element_text(size = 15, hjust = 0, margin = margin(0, 0, 30, 0), lineheight = 0.95),
    plot.caption = element_text(size = 10, margin = margin(30, 0, 0, 0)),
    plot.margin = margin(0, 30, 0, 30),
    strip.text = element_text(size = 12, margin = margin(0, 0, 15, 0), family = f2)
  )

# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # ffmpeg -i animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" makingof.mp4

# ggsave(here::here("temp", paste0("star_trek_commands-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

