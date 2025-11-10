library(tidyverse)
library(ggdist)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv')

flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv')

flint <- flint_mdeq |> 
  rename(lead2 = lead, lead1 = lead2) |> 
  full_join(flint_vt |> rename(lead3 = lead), by = "sample") |> 
  pivot_longer(cols = c(-sample, -notes), names_to = "lead", values_to = "value") |> 
  group_by(lead) |> 
  mutate(p90 = quantile(value, 0.9, na.rm = TRUE), percentile_group = ifelse(value >= p90, "Above 90th", "Below 90th")) |> 
  ungroup() |> 
  arrange(sample, lead) |> 
  mutate(lead = case_match(
    lead,
    "lead1" ~ "Official data (with exclusions)",
    "lead2" ~ "Official data (complete)",
    "lead3" ~ "Citizen science data"
  ),
  lead = fct_inorder(lead)
  )

f1 <- "Inclusive Sans"
f2 <- "Publico Headline"

# Color palette
col_danger <- "#FF5252"      # Bright pulsating red for points above 90th
col_safe <- "#78909C"        # Lead-grey for below 90th percentile  
col_threshold <- "#B0BEC5"   # Lead-grey for 15 ppb threshold area
col_p90_line <- "#8B0000"    # Dark blood red for 90th percentile line   

ggplot(flint) +
  geom_rect(data = NULL, aes(xmin = Inf, xmax = 15, ymin = -Inf, ymax = Inf),  fill = col_threshold, alpha = 0.3) +
  geom_vline(xintercept = 15, linetype = "dotted", color = col_threshold, linewidth = 0.8, stat = "unique") +
  geom_vline(aes(xintercept = p90), linetype = "dashed", color = col_p90_line, linewidth = 0.8) +
  geom_swarm(aes(value, fill = value < p90), color = NA, dotsize = 0.8) +
  scale_fill_manual(values = c("TRUE" = col_safe, "FALSE" = col_danger)) +
  facet_wrap(vars(lead), ncol = 1) +
  labs(
    title = "Below the action level",
    subtitle = str_wrap("In 2015, Michigan officials removed two 'outlier' water samples from their dataset, keeping Flint's lead levels just below the federal action threshold. Meanwhile, citizen scientists were collecting their own data and finding a very different story. Each dot represents a water sample; the dashed red line marks the 90th percentile, while the shaded area shows the 15 ppb federal limit.", 140),
    x = "Lead concentration (parts per billion)",
    caption = "Source: Using Flint, Michigan, lead data in introductory statistics (2018, Travis Loux, Andrew K. Gibson) · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(family = f2, size = 20, face = "bold", hjust = 0, margin = margin(b = 8)),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )

record_polaroid()