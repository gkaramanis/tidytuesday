library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 9, height = 8, units = "in", dpi = 320)

patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

prp_long <- patient_risk_profiles %>% 
  pivot_longer(`age group:  10 -  14`:`age group:  90 -  94`, names_to = "age_group", values_to = "age_group_value") %>% 
  pivot_longer(`Sex = FEMALE`:`Sex = MALE`, names_to = "sex", values_to = "sex_value") %>% 
  pivot_longer(`Acetaminophen exposures in prior year`:`Antibiotics Tetracyclines in prior year`, names_to = "exposure", values_to = "exposure_value") %>% 
  pivot_longer(`predicted risk of Pulmonary Embolism`:`predicted risk of Parkinson's disease, inpatient or with 2nd diagnosis`, names_to = "predicted_risk", values_to = "predicted_risk_value") 

horm_conc <- prp_long %>% 
  filter(exposure == "HORMONAL CONTRACEPTIVES in prior year") %>% 
  filter(exposure_value == 1 & age_group_value == 1 & sex_value == 1) %>% 
  mutate(
    predicted_risk = str_remove(predicted_risk, "predicted risk of "),
    age_group = str_remove(age_group, "age group: "),
    age_group = str_remove_all(age_group, " "),
    sex = str_remove(sex, "Sex = "),
    sex = str_to_lower(sex)
    ) %>% 
  group_by(age_group, predicted_risk) %>% 
  mutate(pred_risk_med = median(predicted_risk_value)) %>% 
  ungroup()
  
f1 <- "Bricolage Grotesque 12pt Condensed"

ggplot(horm_conc) +
  geom_bar(aes(sex, 1, fill = predicted_risk_value), position = "fill", stat = "identity", size = 1, color = "grey99") +
  geom_text(aes(sex, 1, label = personId), position = "fill", stat = "identity", hjust = 1.2, family = f1, color = "grey99", size = 3.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(position = "right") +
  MetBrewer::scale_fill_met_c("VanGogh3") +
  facet_grid(rows = vars(str_wrap(predicted_risk, 25)), cols = vars(age_group)) +
  labs(
    title = "Risk of use of hormonal contraceptives in prior year",
    subtitle = "Predicted 1-year risk of 14 outcomes by sex and age group for 19 simulated patients. Each patient is shown as a rectangle with an ID number.",
    caption = "Source: Jenna Reps · Graphic: Georgios Karamanis",
    y = "Age group",
    fill = "Risk"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.5, "line"),
    legend.key.width = unit(2, "lines"),
    legend.title = element_text(vjust = 1),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = c("purple3", "orange3"), size = 10),
    panel.grid = element_blank(),
    strip.clip = "off",
    strip.text.x = element_text(face = "bold", size = 10),
    strip.text.y = element_text(angle = 0, hjust = 0, face = "bold", size = 8),
    strip.placement = "inside",
    panel.margin = unit(0.1, "lines"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )
