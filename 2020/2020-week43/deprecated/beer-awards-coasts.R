library(tidyverse)
library(urbnmapr)

# medals inflation by year
# combine heatmap and table

beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

# west_coast = data.frame(state_abbv = c("CA", "OR", "WA", "AK"), coast = "West Coast")
# east_coast = data.frame(state_abbv = c("ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC", "SC", "GA", "FL"), coast = "East Coast")
# coasts <- rbind(west_coast, east_coast)

beer_medals <- beer_awards %>% 
  mutate(state_abbv = toupper(state)) %>%
  # left_join(coasts) %>% 
  filter(str_detect(category, "India Pale Ale")) %>%
  add_count(state, medal)
  # filter(!is.na(coast))

states_sf <- get_urbn_map("states", sf = TRUE)

beer_states <- beer_medals %>%
  left_join(states_sf)

ggplot(beer_states) +
  geom_sf(aes(geometry = geometry, fill = n), color = "#ffffff") +
  scale_fill_steps2() +
  facet_wrap(vars(year)) +
  theme_void() +
  theme(
    legend.position = "none"
  ) 

ggsave(here::here("temp", paste0("beer-awards-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

