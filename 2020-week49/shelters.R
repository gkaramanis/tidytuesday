library(tidyverse)
library(lubridate)
library(ggtext)

shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

windows <- shelters %>% 
  mutate(
    month = month(occupancy_date),
    year = year(occupancy_date)
    ) %>% 
  group_by(year, month, organization_name) %>% 
  summarise(occupancy = sum(occupancy, na.rm = TRUE), capacity = sum(capacity, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    occupancy_status = case_when(
      occupancy >= 0.99 * capacity & occupancy > 0 ~ "#EFC242",
      occupancy == 0 ~ "grey20",
      TRUE ~ "#A13E05"
    ),
    org_n = as.numeric(as.factor(organization_name)),
    org_y = (org_n + 4) %/% 5,
    org_x = (org_n - 1) %% 5 - 2,
    org_x = if_else(org_y == max(org_y), org_x + 1, org_x)
  )

houses <- data.frame(x = 8 * 1:12, y = 0) %>% 
  rowwise() %>% 
  mutate(
    h_x = list(x + c(-3, 3, 3, 0, -3)),
    h_y = list(y + c(0, 0, 8, 9.5, 8))
  ) %>% 
  unnest(c(h_x, h_y))
  
ggplot(windows) +
  geom_polygon(data = houses, aes(h_x, h_y, group = x), color = "grey5", fill = "grey15", size = 1) +
  geom_tile(aes(month * 8 + org_x, org_y, fill = occupancy_status, height = 0.75, width = 0.75)) +
  scale_fill_identity() +
  scale_x_continuous(breaks = 8 * 1:12, labels = toupper(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
) +
  # coord_fixed() +
  facet_wrap(vars(year), ncol = 1) +
	labs(
		title = "Occupancy Rates of Toronto Shelters",
		subtitle = "Each window represents one of the 33 organisations that<br>provide shelters, highlighted are occupancy rates <span style = 'color:#EFC242;'>over 99%</span>.",
		caption = "Source: open.toronto.ca via {opendatatoronto} | Graphic: Georgios Karamanis"
	) +
  theme_void(base_family = "Atkinson Hyperlegible") +
  theme(
    plot.background = element_rect(fill = "#0F527A", color = NA),
    axis.text.x = element_text(family = "DIN Condensed Bold", size = 8, color = "grey90", margin = margin(2, 0, 0, 0)),
    strip.text = element_text(family = "DIN Condensed Bold",  color = "grey90", size = 28, hjust = 0.95, margin = margin(30, 0, 10, 0)),
    plot.margin = margin(20, 20, 15, 20),
		plot.title = element_text(family = "Atkinson Hyperlegible Bold", color = "grey90", size = 15.5),
		plot.subtitle = element_markdown(color = "grey90", lineheight = 1),
		plot.caption = element_text(hjust = 0.5, margin = margin(30, 0, 0, 0), size = 8, color = "#EFC242")
  ) +
  ggsave(here::here("temp", paste0("shelters-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 6.3, height = 6.4)
