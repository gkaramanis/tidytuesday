library(tidyverse)
library(magick)

charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv') %>% 
  mutate(chart_position = as.numeric(chart_position))

chart_swift <- charts %>% 
  filter(artist == "Taylor Swift") %>% 
  mutate(b = 100 - chart_position)

img_swift <- image_read(here::here("2020-week40", "img", "swift.jpg")) %>%
  image_convert(colorspace = "gray")

img_w_swift <- image_info(img_swift)$width
img_h_swift <- image_info(img_swift)$height

if (img_w_swift >= img_h_swift) {
  img_swift <- image_resize(img_swift, "80")
} else {
  img_swift <- image_resize(img_swift, ("x80"))
}

img_array_swift <- drop(as.integer(img_swift[[1]]))
rownames(img_array_swift) <- 1:nrow(img_array_swift)
colnames(img_array_swift) <- 1:ncol(img_array_swift)

# Create data frame from array and rename columns
img_df_swift <- as.data.frame.table(img_array_swift) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    # convert b (0-255) to bf (1-0), so that "brighter" values become smaller bars
    bf = 1 - b / 255
  )

img_df_swift %>% 
  left_join(chart_swift) %>%
  ggplot() +
  geom_rect(aes(xmin = x, xmax = x + bf * 0.9, ymin = y, ymax = y + 0.85, fill = chart), color = NA) +
  scale_y_reverse() +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none", plot.background = element_rect(fill = "grey90", color = NA)) +
  ggsave(here::here("temp", paste0("beyonce-swift-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)
