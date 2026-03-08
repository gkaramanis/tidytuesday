library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

clutch_size_cleaned <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-03/clutch_size_cleaned.csv')

tortoise_body_condition_cleaned <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-03/tortoise_body_condition_cleaned.csv')

tortoise_body_condition_cleaned |> 
  ggplot() +
  geom_boxplot(aes(x = year, y = body_condition_index, group = individual)) +
  facet_grid(vars(sex), vars(locality))

bci_by_year <- tortoise_body_condition_cleaned |>
  filter(locality == "Plateau", sex == "f") |>
  summarise(mean_bci = mean(body_condition_index, na.rm = TRUE), .by = year)


ratio_by_year <- tortoise_body_condition_cleaned |>
  filter(locality == "Plateau") |>
  distinct(individual, year, sex) |>
  count(year, sex) |>
  pivot_wider(names_from = sex, values_from = n) |>
  mutate(ratio = m / f)

annotations <- tribble(
  ~year, ~label,
  2008,  "Study begins. 52 males per female in BCI surveys.",
  2009,  "Most females captured in a single year. 30 females, 19 males per female.",
  2013,  "Only 2 females captured. 93 males per female was the study peak. Female BCI at its lowest.",
  2020,  "Female BCI reached its highest recorded value amid large year to year variation in the study.",
  2023,  "Last survey year. Demographic models predict the last island female will die in 2083."
)

tort <- bci_by_year |> 
  inner_join(ratio_by_year) |>
  left_join(annotations) |> 
  arrange(year)

f1 <- "Karst Light"
f2 <- "Karst ExtraBold"
pal <- MetBrewer::met.brewer("Hokusai2")[-1]

ggplot(tort, aes(x = ratio, y = mean_bci)) +
  geom_path(aes(color = year), linewidth = 1.25) +
  geom_point(aes(size = f, fill = year %in% c(2008, 2023)), shape = 21, color = "#f7f9f4", stroke = 1) +
  geom_mark_circle(data = . %>% filter(!is.na(label)), 
                   aes(label = paste0(year,"\n", label),
                     group = interaction(year, mean_bci),
                     x0 = case_when(
                       year == 2023 ~ ratio + 65,
                       year == 2020 ~ ratio + 20,
                       TRUE ~ ratio
                     ),
                     y0 = case_when(
                       year == 2009 ~ mean_bci - 0.5,
                       year == 2013 ~ mean_bci + 0.9,
                       TRUE ~ mean_bci
                     )), color = NA, expand = unit(2, "mm"), label.buffer = unit(5, "mm"), con.colour = "gray40", con.size = 0.35, label.fill = alpha("white", 0.7), label.colour = "gray20", label.family = f2, label.fontsize = 10.5, label.width = unit(55, "mm"), con.linetype = "dotted") +
  scale_size_area(max_size = 8) +
  scale_color_gradientn(colors = pal) +
  scale_fill_manual(values = c("gray30", "#c8963c")) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "A male-biased sex ratio is driving a tortoise population to extinction",
    subtitle = str_wrap("Each point is a year of field surveys from 2008 to 2023 on the Golem Grad Island plateau in North Macedonia. Moving right means more males per female among individuals captured that year. Moving down means lower average female body condition, which measures body mass relative to body size. Point size shows how many females were captured. Male harassment drives up female mortality, which pushes the sex ratio further out of balance, which increases harassment in turn. The paper calls this an extinction vortex and predicts the last island female will die in 2083.", 115),
    caption = '**Graphic:** Georgios Karamanis · **Source:** Arsovski, D., X. Bonnet, A. Golubović, and L. Tomović. 2026. “ Sex Ratio Bias Triggers Demographic Suicide in a Dense Tortoise Population.” Ecology Letters 29, no. 1: e70296. https://doi.org/10.1111/ele.70296.',
    x = "Males per female ratio from capture surveys",
    y = "Mean female BCI"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#f7f9f4", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "darkgreen", linewidth = 0.05),
    plot.title = element_text(family = f2),
    plot.caption = marquee::element_marquee(hjust = 0, margin = margin(10, 0, 0, 0), width = 0.85),
    plot.margin = margin(10, 15, 10, 15)
  )  

record_polaroid()
