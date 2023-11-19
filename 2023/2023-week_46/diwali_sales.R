library(tidyverse)
library(geofacet)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 8, units = "in", dpi = 320)

diwali_sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-14/diwali_sales_data.csv') %>% 
  janitor::clean_names()

ds <- diwali_sales %>% 
  group_by(gender, product_category) %>% 
  summarise(
    med_amount = median(amount, na.rm = TRUE),
    med_orders = median(orders, na.rm = TRUE)
    ) %>% 
  mutate(
    med_amount = if_else(gender == "F", med_amount, -med_amount),
    product_category = fct_reorder(product_category, abs(med_amount)),
    gender = if_else(gender == "F", "Women", "Men")
    ) %>% 
  ungroup()

f1 <- "Outfit"
f2 <- "Caladea"

ggplot(ds, aes(y = product_category, x = med_amount, fill = gender, width = med_orders/4.5)) +
  geom_col(aes(color = after_scale(colorspace::darken(fill, 0.5))), size = 0.4) +
  geom_vline(xintercept = 0) +
  shadowtext::geom_shadowtext(aes(x = 0, label = product_category), family = f1) +
  scale_fill_manual(values = c("#BC8F8F", "#8FBC8F")) +
  scale_x_continuous(labels = function(x) scales::number(abs(x))) +
  labs(
    title = "Diwali Sales",
    subtitle = str_wrap("Sales by gender and product category in rupees. The width of the bar represents the median number of sales in each category.", 90),
    caption = "Source: Saad Haroon · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(margin = margin(15, 0, 0, 0)),
    plot.margin = margin(20, 20, 10, 20)
  )
  
