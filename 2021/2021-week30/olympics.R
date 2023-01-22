library(tidyverse)
library(camcorder)
library(ggrepel)
library(colorspace)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

gg_record(dir = "temp", device = "png", width = 10, height = 11, units = "in", dpi = 320)

bmi <- olympics %>% 
  filter(age > 19) %>%
  filter(year == 2016 | year == 1960) %>%
  filter(season == "Summer") %>% 
  mutate(
    bmi = weight / (height/100)^2,
    sex = if_else(sex == "F", "Women", "Men")
    ) %>%
  group_by(sport, sex, year) %>% 
  summarise(
    # sport = sport,
    bmi_m = mean(bmi, na.rm = TRUE),
    weight_m = mean(weight, na.rm = TRUE),
    height_m = mean(height, na.rm = TRUE),
    bmi_sd = sd(bmi, na.rm = TRUE),
    weight_sd = sd(weight, na.rm = TRUE),
    height_sd = sd(height, na.rm = TRUE)
    ) %>% 
  ungroup() %>% 
  distinct()

bmi_chart <- expand.grid(
  weight = 45:105,
  height = 155:205,
  sex = c("Men", "Women")
  ) %>% 
  mutate(
    bmi = weight / (height/100)^2,
    bmi_label = case_when(
      bmi > 29.9 ~ "Obese",
      bmi > 24.9 ~ "Overweight",
      bmi < 18.5 ~ "Underweight",
      TRUE ~ "Normal"
    )
    )

bmi_cols <- c(
  "Underweight" = "cadetblue2",
  "Normal" = "darkolivegreen2",
  "Overweight" = "lightgoldenrod1",
  "Obese" = "lightcoral"
  )

annot <- tribble(
  ~weight, ~height, ~sex, ~year, ~label,
  50, 202, "Women", 1960, "Underweight",
  80, 202, "Women", 1960, "Normal",
  90, 188, "Women", 1960, "Overweight",
  97, 172, "Women", 1960, "Obese"
  )

f1 = "Piazzolla SC"
f2 = "Graphik"

ggplot(bmi, aes(x = weight_m, y = height_m)) +
  geom_tile(data = bmi_chart, aes(x = weight, y = height, fill = bmi_label), color = NA) +
  geom_point(size = 0.2) +
  geom_text_repel(aes(x = weight_m, y = height_m, label = sport),
                  family = f1,
                  size = 3.5,
                  segment.size = 0.1,
                  segment.color = "grey30",
                  color = "grey10",
                  stat = "unique",
                  seed = 42,
                  max.overlaps = 15) +
  geom_text(data = annot, aes(x = weight, y = height, label = label), hjust = 0, family = f2, color = "grey20", alpha = 0.6) +
  scale_fill_manual(values = bmi_cols) +
  scale_x_continuous(breaks = seq(50, 100, 10)) +
  facet_grid(vars(sex), vars(year)) +
  coord_cartesian(expand = FALSE) +
  labs(
    title = toupper("Olympic athletes are getting bigger"),
    subtitle = "Mean weight and height by sport at the 1960 and 2016 Summer Olympics. Values calculated\nfor athletes 20 years and older, compared to the BMI classification for the general population.",
    caption = "Source: Kaggle · Graphic: Georgios Karamanis",
    x = "Weight (kg)",
    y = "Height (cm)"
  ) +
  theme_minimal(base_family = f2, base_size = 13) +
  theme(
    plot.background = element_rect(fill = "grey97"),
    legend.position = "none",
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0), lineheight = 1),
    axis.title = element_text(family = f1),
    axis.text = element_text(family = f1),
    plot.margin = margin(20, 20, 20, 20)
  )


# export gif
# gg_playback(frame_duration = 0.15, image_resize = 1080)
# convert to mp4 in terminal
# ffmpeg -i 2021_08_01_14_53_40.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" olympics_makingof.mp4