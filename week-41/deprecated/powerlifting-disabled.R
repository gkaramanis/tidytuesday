library(tidyverse)
library(here)
library(gghighlight)

ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

ipf_d <- ipf_lifts %>% 
  mutate(
    disability = case_when(
     str_detect(meet_name, "Disabled") ~ "disabled",
     TRUE ~ "non-disabled"
    ),
    age_class = fct_explicit_na(age_class),
    age_class = fct_relevel(age_class, "5-12"),
    age_class = fct_rev(age_class),
    # age_class = fct_relevel(age_class, NA, Inf),
    age_class = fct_recode(age_class, "80-" = "80-999")
    )


# timeline of tournaments!
#
#

ggplot(ipf_d) +
  geom_tile(aes(x = best3bench_kg, y = age_class, width = 3, height = 0.7, fill = bodyweight_kg), alpha = 0.5) +
  gghighlight(disability == "disabled") +
  xlim(0, 450) +
  scale_fill_viridis_c(option = "inferno") +
  facet_wrap(vars(sex)) +
  theme_minimal(base_family = "IBM Plex Sans") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    axis.title = element_blank()
  ) +
  ggsave(
    here::here("week-41", "figures", "temp", paste0("powerlifting-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")),
    width = 16, height = 10, dpi = 320
    )

