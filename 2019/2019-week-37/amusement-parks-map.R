library(tidyverse)
library(here)
library(urbnmapr)
library(ggsci)
library(ggsflabel)

# read in data
safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

# rename things, keep only two columns
acc_state <- safer_parks %>%
  rename(state_abbv = acc_state) %>%
  group_by(state_abbv) %>%
  top_n(n = 1, wt = device_category) %>% 
  distinct(state_abbv, device_category) %>% 
  mutate(device_category = str_replace_all(device_category,
                                      c("other.+" = "other",
                                      " " = "\n")))

# read map and merge with data
states_sf <- get_urbn_map("states", sf = TRUE)
spatial_data <- left_join(states_sf, acc_state, by = "state_abbv")

# custom legend order: get types from data, then move "other" to end of list
x <- unique(sort(acc_state$device_category))
legend_items <- c(x[-2], x[2])

# plot
ggplot() +
  geom_sf(data = spatial_data, aes(fill = device_category),
          color = "grey30", size = 0.25) +
  # labels
  geom_sf_text_repel(data = spatial_data, aes(label = device_category),
               size = 6, family = "IBM Plex Sans Medium", hjust = 0,
               color = "grey15", point.padding = NA, lineheight = 0.9) +
  # customize legend (label.position puts labels under keys!)
  guides(fill = guide_legend(title.position = "top",
                             title = "Amusement park injuries:\nThe most common device category reported by state", label.hjust = 0,
                             title.hjust = 0, nrow = 1,
                             label.position = "bottom",
                             override.aes = list(color = "grey50"))) +
  # palette, make NA grey80
  scale_fill_simpsons( na.value = "grey80",
                limits = legend_items) +
  # caption
  labs(caption = "Source: data.world & Safer Parks | Graphic by Georgios Karamanis") +
  # theme
  theme_void(base_family = "IBM Plex Sans", base_size = 20) +
  theme(
    plot.margin = margin(20, 30, 20, 30),
    plot.background = element_rect(fill = "grey90"),
    legend.position = "top",
    legend.title = element_text(size = 32, color = "grey10", family = "IBM Plex Sans Medium"),
    legend.text = element_text(color = "grey30", hjust = 0, lineheight = 0.8),
    legend.key = element_rect(fill = 'white', color = NA),
    legend.key.width = unit(35, "points"),
    legend.key.height = unit(7, "points")
  ) +
  # save image
  ggsave(
  here::here("week-37", "figures", "temp", paste0("amusement_parks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
  height = 12, width = 16, dpi = 320
  )
