library(tidyverse)
library(cowplot)
library(ggtext)

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

# The Claremont Project has a Twitter handle - please reference them when using this data: @ClaremontRun

emotions <- characters %>% 
  distinct(issue, explicitly_states_i_love_you_to_whom, kiss_with_which_character, visible_tears_number_of_intances) %>% 
  mutate(
    love = if_else(!is.na(explicitly_states_i_love_you_to_whom), 1, 0),
    kiss = if_else(!is.na(kiss_with_which_character), 1, 0),
    tears = if_else(visible_tears_number_of_intances > 0, 1, 0)
  ) %>% 
  pivot_longer(cols = love:tears, names_to = "emotion", values_to = "n") %>% 
  group_by(issue, emotion) %>% 
  summarise(n = if_else(sum(n) > 0, 1, 0)) %>% 
  ungroup() 

logo <- here::here("2020-week27", "img", "x-men-logo.png")

arcs <- tribble(
  ~story, ~x, ~xend, ~hjust,
	"Dark Phoenix Saga", 129, 138, 0,
	"Days of Future Past", 141, 142, 0,
	"E is for Extinction", 114, 116, 0,
	"The Brood Saga", 154, 167, 0,
	"From the Ashes", 168, 176, 0,
  "Wounded Wolf", 205, 205, 1,
  "Mutant Massacre", 210, 214, 1,
  "Broodfall", 232, 234, 1,
	"Inferno", 239, 243, 1
)

main_plot <- ggplot(emotions) +
  # Points for all the issues
  geom_point(aes(x = issue, y = 0.5), colour = "grey40", size = 0.3) +
  # Issue tick marks inside the circle
  annotate("tile", x = c(97, seq(100, 280, 20)), y = 0, height = 0.5, width = 0.15, fill = "grey65", colour = "grey70") +
  # Issue numbers
  annotate("text", x = c(97, seq(100, 260, 20)), y = -0.55, label = c(97, seq(100, 260, 20)), family = "DIN Condensed Bold", size = 4, colour = "grey40") +
  annotate("text", x = 280, y = -0.55, label = "Issue 280", family = "DIN Condensed Bold", size = 4, colour = "grey40", hjust = 0.9) +
  # Love, tears and kisses
  geom_bar(aes(x = issue, y = n, fill = emotion), position = "fill", stat = "identity") +
  # Story arcs outside the circle
	geom_segment(data = arcs, aes(x = x - 0.15, xend = xend + 0.15, y = 1.25, yend = 1.25), colour = "grey65") +
  geom_text(data = arcs, aes(x = x + (xend - x) / 2, y = 1.6, label = story, hjust = hjust), family = "DIN Condensed Bold", size = 4, colour = "grey35") +
  # Title
  annotate("richtext", x = 190, y = -4.7,
           label = "<span style='color:#DC4431;display:inline-block'>LOVE,</span><span style='color:#377AB1;display:inline-block'> TEARS </span> and <span style='color:#FCED1F;display:inline-block'>KISSES</span>",
           size = 8, family = "IBM Plex Sans Bold", fill = NA, label.color = NA) +
  # Subtitle
  annotate("text", x = 190, y = -3.2, label = "Issues during the Claremont run in which\ncharacters say \"I love you\", cry, or\nkiss another character", family = "DIN Condensed Bold", size = 7.5, lineheight = 1, colour = "grey30") +
  # Caption
  annotate("text", x = 190, y = -1.8, label = "Graphic: Georgios Karamanis\nSource: The Claremont Run", family = "IBM Plex Sans Condensed Light", size = 3, lineheight = 1, colour = "grey45") +
  # Scales, theme, etc
  scale_x_continuous(limits = c(95, 285)) +
  ylim(-5, 2) +
  coord_polar(clip = "off") +
  scale_fill_manual(values = c("#FCED1F", "#DC4431", "#377AB1")) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#BFBFBF", colour = NA)
    # plot.margin = margin(20, 20, 20, 20)
  )

ggdraw() +
  draw_plot(main_plot) +
  draw_image(logo, x = 0.015, y = 0.1, scale = 0.24) 

ggsave(here::here("2020-week27", "plots", "temp", paste0("xmen-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 9, height = 9)

