library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

pal <- RColorBrewer::brewer.pal(name = "RdPu", n = 4)

scurvy_long <- scurvy %>% 
  mutate(
    across(gum_rot_d6:fit_for_duty_d6, parse_number)
  ) %>% 
  pivot_longer(gum_rot_d6:fit_for_duty_d6, names_to = "symptom", values_to = "severity") %>% 
  mutate(
    treatment = str_to_sentence(treatment),
    treatment = str_replace_all(treatment, "_", " "),
    treatment = fct_rev(treatment),
    symptom = str_to_sentence(symptom),
    symptom = str_remove(symptom, "_d6"),
    symptom = str_replace_all(symptom, "_", " "),
    symptom = fct_inorder(symptom),
    symptom_col = case_when(
      symptom != "Fit for duty" ~ pal[severity + 1],
      symptom == "Fit for duty" & severity == 0 ~ "grey35",
      symptom == "Fit for duty" & severity == 1 ~ "grey85"
    )
  ) %>% 
  mutate(treat_case = 2 - study_id %% 2) %>% 
  rowwise() %>% 
  mutate(
    x = case_when(
      treat_case == 1 ~ list(c(as.numeric(symptom), as.numeric(symptom) + 1, as.numeric(symptom))),
      treat_case == 2 ~ list(c(as.numeric(symptom), as.numeric(symptom) + 1, as.numeric(symptom) + 1))
    ),
    y = case_when(
      treat_case == 1 ~ list(c(as.numeric(treatment) + 1, as.numeric(treatment) + 1, as.numeric(treatment))),
      treat_case == 2 ~ list(c(as.numeric(treatment), as.numeric(treatment) + 1, as.numeric(treatment)))
    ),
  ) %>% 
  ungroup() %>% 
  mutate(i = row_number())

rct <- expand.grid(x = 1:5 + 0.5, y = 1:6 + 0.5)
  
sev <- scurvy %>% 
  distinct(gum_rot_d6) %>% 
  separate(gum_rot_d6, into = c("points", "grade")) %>% 
  arrange(points) %>% 
  mutate(txt = paste0("<span style='color:", pal[as.numeric(points) + 1], "'>⯀</span> ", grade))

subt1 <- paste("**Severity of symptom** ", paste0(sev$txt, collapse = " "))

subt2 <- paste0("**Fit for duty** <span style='color:grey85'>⯀</span> Yes <span style='color:grey35'>⯀</span> No")

subt <- paste("During the trial, conducted in 1747, 12 seamen suffering from scurvy were treated. They were divided into six treatment groups,<br>with each group receiving a different treatment. At the 6th day, five outcomes were evaluated, namely the severity of four scurvy<br>symptoms and being fit for duty or not. The group receiving citrus fruits showed the most remarkable improvement.<br><br>", subt1, "-", subt2)

f1 <- "Outfit"
f2 <- "Publico Headline"

ggplot(scurvy_long %>% unnest(c(x, y))) +
  geom_polygon(aes(x = x, y = y, fill = symptom_col, color = symptom_col, group = i)) +
  geom_text(aes(x = as.numeric(symptom) + 0.5, y = 0.8, label = symptom), family = f1, stat = "unique") +
  geom_text(aes(x = 0.8, y = as.numeric(treatment) + 0.5, label = str_wrap(treatment, 15)), family = f1, stat = "unique", hjust = 1) +
  geom_tile(data = rct, aes(x, y), fill = NA, color = "grey99", linewidth = 0.5) +
  # annotate("text", x = c(1.25, 1.75), y = c(6.75, 6.25), label = c("Seaman 1", "Seaman 2")) +
  scale_x_continuous(limits = c(0.3, 6)) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(
    title = "James Lind's scurvy trial",
    subtitle = subt,
    caption = "Source: medicaldata R package · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(face = "bold", size = 18, family = f2),
    plot.subtitle = ggtext::element_markdown(hjust = 0, lineheight = 1.1, size = 12),
    plot.caption = element_text(family = f2, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )
  
