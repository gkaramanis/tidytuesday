library(tidyverse)
library(gender)
library(genderdata)
library(schrute)
library(here)
library(cowplot)

# Pull unique characters from the dataset and identify gender
names_gender <- gender(unique(theoffice$character)) %>% 
  select(character = name, everything())

# Join with the original dataset with the script
theoffice_gender <- full_join(theoffice, names_gender) %>% 
  select(index, season, episode, character, gender, proportion_male, proportion_female, everything())

theoffice_toplot <- theoffice_gender %>% 
  mutate(
    season = as.numeric(season),
    episode = as.numeric(episode),
    character = str_replace_all(character, "\"", ""), # remove quotes
    # Identify gender manually
    gender = case_when(
      str_detect(tolower(character), "woman|mom|girl|waitress|actress|hostess|mrs\\.|mary-beth|diane|cindy|nurse|bar manager") ~ "female",
      str_detect(tolower(character), "jack|goldenface|aaron|ben |a\\.j|billy|^bob|david|rob$|grotti|tod|guy|mr\\.|businessman|policeman|waiter|actor|host|\\bman\\b|grandfather|robert|ryan|salesman|senator|father|son$") ~ "male",
      TRUE ~ gender
  )) %>% 
  # Calculate total lines by character and filter
  group_by(character) %>% 
  add_count(name = "character_lines") %>% 
  filter(character_lines > 20) %>% 
  ungroup() %>% 
  
  filter(!is.na(gender)) %>% # remove NA gender
  
  # Calculate frequencies for every episode
  group_by(season, episode, gender) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  mutate(
    episode_max = max(episode), # max nr of episodes in every season (for the plot)
    season_text = paste0("Season ", season) # add "Season" to the facet label
    )  %>% 
  ungroup()

main <- ggplot(theoffice_toplot) +
  geom_bar(aes(x = freq, y = episode, fill = gender), position = position_fill(reverse = TRUE), stat = "identity") +
  geom_text(aes(x = 0, y = episode, label = episode), size = 1.8, hjust = 1, nudge_x = -0.01, family = "IBM Plex Mono Bold", colour = "#696926") +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0.2, yend = episode_max + 0.8), colour = "grey90", size = 0.3) +
  scale_y_reverse() +
  scale_fill_manual(values = c("#691A69", "#EBBC55"), guide = guide_legend(direction = "horizontal", keyheight = 0.1, keywidth = 2, title = "", title.position = "top")) +
  facet_wrap(vars(season_text), nrow = 3) +
  labs(
    title = "A Man's Office",
    subtitle = str_wrap("Female characters in 'The Office' (US) have only 25% of the total dialogue lines across 9 seasons and 186 episodes (median value of the percentage of lines spoken by female characters in every episode). The gender of the characters was identified by the gender R package and manually, for characters with more than 20 lines.", width = 110),
    caption = "Source: schrute R package | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "JetBrains Mono") +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 20, 0),
    legend.text = element_text(margin = margin(0, 20, 0, 0)),
    strip.text = element_text(family = "American Typewriter", size = 15, colour = "#371a69"),
    plot.title = element_text(family = "American Typewriter Bold", size = 28, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(20, 0, 20, 0), hjust = 0.5, size = 15),
    plot.background = element_rect(fill = "grey90", colour = NA),
    plot.margin = margin(20, 20, 20, 20)
  )
  

inset <- theoffice_toplot %>%
  ggplot() +
  geom_dotplot(aes(x = freq, fill = gender), width = 0.1, colour = NA, binwidth = 0.01, stackdir = "down") +
  coord_fixed(expand = FALSE) +
  scale_x_continuous(position = "top", limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
  scale_y_continuous(limits = c(-0.15, 0)) +
  scale_fill_manual(values = c("#691A69", "#EBBC55")) +
  labs(title = "Distribution of episodes by gender and dialogue percentage") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "American Typewriter Semibold", size = 11, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    axis.line.x = element_line(size = 0.25, colour = "grey40"),
    axis.text.x = element_text(family = "JetBrains Mono", size = 9, margin = margin(0, 0, 5, 0))
  )

ggdraw(main) +
  draw_plot(inset, x = 0.031, y = 0.445, width = 0.29, height = 0.4) +
  ggsave(here::here("2020-week12", "plots", "the-office.png"), dpi = 320, width = 16, height = 12)
    
