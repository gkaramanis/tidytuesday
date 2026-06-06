library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

eplp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv') |> 
  mutate(across(where(is.numeric), ~replace_values(.x, -98 ~ NA))) 

# Inspiration for chart
# European Parliamentary Research Service (EPRS)
# https://epthinktank.eu/2014/12/19/maternity-and-paternity-leave-in-the-eu/

before_after <- eplp |> 
  group_by(country) |> 
  filter(year == max(year)) |> 
  ungroup() |> 
  select(country, year, mand_before = mat_m_ld_bb, mand_after = mat_m_ld_ab, vol_before = mat_v_ld_bb, vol_after = mat_v_ld_ab) |> 
  mutate(
    mat_bb = mand_before + vol_before,
    mat_ab = mand_after + vol_after,
    total = mat_bb + mat_ab,
    country_name = countrycode::countrycode(country, "iso2c", "country.name"),
    country_name = if_else(country == "UK", "United Kingdom", country_name),
    country_name = fct_reorder(country_name, total)
    ) 

f1 <- "Poppins"

col1 <- "#6A8FA0"
col2 <- "#C4956A"
col3 <- "white"

ggplot(before_after, aes(x = country_name, 10)) +
  geom_col(aes(y = -mat_bb), color = NA, fill = "white", linewidth = 0.5, width = 0.7) +
  geom_col(aes(y = mat_ab), color = NA, fill = "white", linewidth = 0.5, width = 0.7) +
  ggpattern::geom_col_pattern(aes(y = -mand_before), fill = col1, color = col1, pattern = "crosshatch", pattern_color = "white", pattern_density = 0.02, pattern_spacing = 0.01, width = 0.69) +
  ggpattern::geom_col_pattern(aes(y = mand_after), fill = col2, color = col2, pattern = "crosshatch", pattern_color = "white", pattern_density = 0.02, pattern_spacing = 0.01, width = 0.69) +
  geom_col(aes(y = -mat_bb), color = col1, fill = NA, linewidth = 0.5, width = 0.7) +
  geom_col(aes(y = mat_ab), color = col2, fill = NA, linewidth = 0.5, width = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 0, color = col3, linewidth = 0.25) +
  scale_y_continuous(minor_breaks = -10:40, breaks = seq(-10, 40, 5), labels = abs(seq(-10, 40, 5)), expand = c(0.01, 0.05)) +
  labs(
    title = "Maternity leave in the EU, 2024",
    subtitle = "Mandatory▩ and voluntary□ maternity leave {.#6A8FA0 **← before**} and {.#C4956A **after →**} childbirth. Countries sorted by total leave.  
    Voluntary figures show the maximum entitlement, actual take-up may be lower. Where no dedicated maternity leave exists, non-transferable parental leave reserved for birth mothers counts as maternity leave for comparability. Covers employed mothers at first birth under 2024 legislation. Parental leave shared between parents is not included.",
    caption = "Source: S. Spitzer et al., “The European Parenting Leave Policies (EPLP) Dataset”. Zenodo, Nov. 19, 2025. doi: 10.5281/zenodo.17648712 · Graphic: Georgios Karamanis",
    y = "Number of weeks"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(hjust = 0.15),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = marquee::element_marquee(),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 3)),
    plot.title.position = "plot",
    plot.subtitle = marquee::element_marquee(margin = margin(t = 0, b = 15), width = 0.99),
    plot.caption = element_text(margin = margin(t = 15)),
    plot.margin = margin(10, 10, 10, 10)
  )
  
record_polaroid()