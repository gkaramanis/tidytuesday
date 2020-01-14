library(tidyverse)
library(here)
library(tidytext)
library(grid)
library(tidylog)
# library(conflicted)

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>% 
  na.omit()

# Calculate character frequency within categories
passwords_freq <- passwords %>% 
  add_count(category, name = "n_passwords") %>% 
  mutate(category = paste0(category, "(", n_passwords, "):")) %>% 
  unnest_tokens(characters, password, token = "characters", drop = FALSE) %>% 
  group_by(category, characters) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ungroup()

# Make a ggplot
p <- ggplot(passwords_freq) +
  geom_text(aes(x = characters, y = fct_rev(toupper(category)), label = toupper(characters), alpha = freq), family = "Press Start 2P", color = "green3") +
  scale_alpha(range = c(0.1, 1)) +
  labs(
    title = "Frequency of characters by password category\n(number of passwords)",
    caption = "Source: Information is Beautiful | Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = "Press Start 2P") +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill = "black", color = NA),
    axis.text.y = element_text(color = "green3", family = "Press Start 2P", size = 11),
    plot.title = element_text(color = "green3", family = "Press Start 2P", margin = margin(0, 0, 10, 0), hjust = 1),
    plot.caption = element_text(color = "green3", family = "Press Start 2P"),
    plot.margin = margin(30, 40, 30, 40)
  )

# Round corners
# https://stackoverflow.com/questions/48199791/rounded-corners-in-ggplot2
g <- ggplotGrob(p)
bg <- g$grobs[[1]]
round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
                          r=unit(0.05, "snpc"),
                          just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
g$grobs[[1]] <- round_bg

# Plot and ggsave
plot(g)
ggsave(plot = g,
  here::here("2020-week03", "plots", "temp", paste0("passwords-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 12, height = 9
)


