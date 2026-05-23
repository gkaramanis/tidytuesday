library(tidyverse)
library(gt)
library(countrycode)

metadata <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-05-19/metadata_coverage_stats_by_country.csv"
)

region_labels <- c(
  EAS = "East Asia & Pacific",
  ECS = "Europe & Central Asia",
  LCN = "Latin America & Caribbean",
  MEA = "Middle East & North Africa",
  NAC = "North America",
  SAS = "South Asia",
  SSF = "Sub-Saharan Africa"
)

# Latest date, journal articles, top 20 countries by DOI volume
base_counts <- metadata |>
  filter(
    current_up_to == max(current_up_to),
    document_type == "journal-article"
  ) |>
  group_by(iso3_code, region_id) |>
  summarise(
    n_dois         = sum(n_dois, na.rm = TRUE),
    with_ref       = sum(with_ref, na.rm = TRUE),
    with_abstract  = sum(with_abstract, na.rm = TRUE),
    with_orcid     = sum(with_orcid, na.rm = TRUE),
    with_funding   = sum(acknowledges_funding, na.rm = TRUE),
    with_funder_id = sum(with_funder_id, na.rm = TRUE),
    with_license   = sum(with_license, na.rm = TRUE),
    .groups = "drop"
  ) |>
  slice_max(n_dois, n = 20) |>
  mutate(
    country_name = coalesce(countrycode(iso3_code, "iso3c", "country.name"), iso3_code),
    region_label = coalesce(region_labels[region_id], region_id)
  )

add_pcts <- function(df) {
  mutate(df,
    pct_ref       = with_ref / n_dois,
    pct_abstract  = with_abstract / n_dois,
    pct_orcid     = with_orcid / n_dois,
    pct_funding   = with_funding / n_dois,
    pct_funder_id = with_funder_id / n_dois,
    pct_license   = with_license / n_dois
  )
}

tbl_data <- base_counts |>
  add_pcts() |>
  select(region_label, country_name, iso3_code, n_dois, starts_with("pct_")) |>
  arrange(region_label, desc(n_dois))

f1 <- "Inclusive Sans"
f2 <- "Input Mono Condensed"

# ---- Build gt table ----
tbl_data |>
  group_by(region_label) |>
  mutate(n_dois = n_dois / 1e6) |>
  gt(rowname_col = c("country_name")) |>
  fmt_flag(columns = iso3_code) |>
  cols_merge(columns = c(country_name, iso3_code), pattern = "{2} {1}") |>
  cols_width(starts_with("pct_") ~ px(70)) |>
  fmt_number(columns = n_dois, decimals = 2, pattern = "{x}M") |>
  fmt_percent(columns = starts_with("pct_"), decimals = 0) |>
  data_color(
    columns = starts_with("pct_"),
    method  = "numeric",
    palette = "PuBu"
  ) |>
  cols_label(
    n_dois        = "DOIs",
    pct_ref       = "Refs",
    pct_abstract  = "Abstract",
    pct_orcid     = "ORCID",
    pct_funding   = "Funding",
    pct_funder_id = "Funder ID",
    pct_license   = "License"
  ) |>
  tab_header(
    title    = md("**Research Nexus Readiness**"),
    subtitle = md("The table shows selected Crossref metrics for the top 20 countries by journal article DOI volume in<br>April 2026: **Refs** works with references &nbsp;·&nbsp; **Abstract** with abstract &nbsp;·&nbsp; **ORCID** with an author ORCID ID &nbsp;·&nbsp;<br>**Funding** acknowledging any funding &nbsp;·&nbsp; **Funder ID** with a funder ID &nbsp;·&nbsp; **License** with license metadata")
  ) |>
  tab_source_note(
    source_note = "Source: Crossref · Table: Georgios Karamanis"
  ) |>
  tab_style(
    style     = cell_text(font = f2),
    locations = cells_body(columns = c(n_dois, starts_with("pct_")))
  ) |>
  tab_style(
    style     = cell_text(align = "center"),
    locations = cells_column_labels()
  ) |>
  opt_table_font(font = f1) |>
  tab_options(
    table.font.size                    = 12,
    column_labels.font.weight          = "bold",
    row_group.font.weight              = "bold",
    heading.title.font.size            = 18,
    heading.subtitle.font.size         = 12,
    table.background.color             = "grey99",
    column_labels.background.color     = "grey99",
    stub.background.color              = "grey99",
    stub.border.style                  = "none",
    table_body.hlines.color            = "grey88",
    table_body.vlines.style            = "solid",
    table_body.vlines.color            = "grey99",
    column_labels.border.top.color     = "grey30",
    column_labels.border.bottom.color  = "grey30",
    table.border.top.color             = "transparent",
    table.border.bottom.color          = "grey30",
    heading.border.bottom.color        = "transparent",
    source_notes.font.size             = 11,
    source_notes.border.lr.color       = "transparent",
    heading.align                      = "left"
  ) |> 
  gtsave("2026/2026-week_21/plots/crossref.png", expand = 15)

