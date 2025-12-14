library(tidyverse)
library(reactable)
library(htmltools)

qatarcars <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv')

qc <- qatarcars %>%
  mutate(
    width = if_else(model == "GLB 200", 1.834, width),
    model = str_trim(str_remove(model, make)),
    iso3166 = case_match(
      origin,
      "Germany" ~ "de",
      "Italy" ~ "it",
      "Japan" ~ "jp",
      "PR China" ~ "cn",
      "South Korea" ~ "kr",
      "Sweden" ~ "se",
      "UK" ~ "gb",
      "USA" ~ "us"
    )
  )

# Constants
FONT_FAMILY <- "Montserrat, Arial, sans-serif"
CELL_HEIGHT <- 40
BAR_HEIGHT <- 8
COLOR_BETTER_HIGHER <- "#f59e0b"
COLOR_BETTER_LOWER <- "#8b5cf6"

# Max values as named vector for efficient lookup
max_vals <- c(
  length = max(qc$length, na.rm = TRUE),
  width = max(qc$width, na.rm = TRUE),
  height = max(qc$height, na.rm = TRUE),
  horsepower = max(qc$horsepower, na.rm = TRUE),
  trunk = max(qc$trunk, na.rm = TRUE),
  economy = max(qc$economy, na.rm = TRUE),
  performance = max(qc$performance, na.rm = TRUE),
  mass = max(qc$mass, na.rm = TRUE)
)

bar_style <- function(width = 1, color = "#fc5185") {
  fill_pct <- width * 100
  gradient <- sprintf("linear-gradient(90deg, %s %.2f%%, #e1e1e1 %.2f%%)", color, fill_pct, fill_pct)
  list(
    background = gradient
  )
}

# Helper function for cell HTML structure
create_cell_html <- function(attr_name, value_html, height = CELL_HEIGHT, font_family = FONT_FAMILY, bar_html = NULL) {
  bar_section <- if (!is.null(bar_html)) bar_html else ""
  sprintf('<div style="display: flex; flex-direction: column; align-items: center; height: %dpx; justify-content: %s; font-family: %s;">
    <div style="font-size: 11px; letter-spacing: 1px; font-weight: 700; color: #222;">%s</div>
    %s
    <div style="font-size: 15px; font-weight: 400; color: #444;">%s</div>
  </div>', height, if (is.null(bar_html)) "center" else "space-between", font_family, attr_name, bar_section, value_html)
}

qc_l <- qc |> 
  # slice_sample(n = 3) |> 
  group_by(make, model) |>
  mutate(
    flag_html = sprintf('<img src="https://flagcdn.com/w20/%s.png" style="vertical-align: middle; margin-right: 5px;" /> %s', tolower(iso3166), origin),
    logo_src = knitr::image_uri(sprintf("2025/2025-week_50/img/logos/%s.png", tolower(make)))
  ) |> 
  reframe(
    header_text = sprintf('%s<br>%s', cur_group()$make, cur_group()$model),
    logo_src = unique(logo_src),
    attr_l = list(c(logo_src, flag_html, type, enginetype, seating, rep(NA, 3))),
    attr_r = list(c(horsepower, performance, economy, mass, trunk, length, width, height)),
    attr_name_l = list(c("logo", "origin", "type", "engine", "seating", rep(NA, 3))),
    attr_name_r = list(c("horsepower", "0-100 km/h", "economy", "mass", "trunk", "length", "width", "height"))
  ) |> 
  ungroup() |> 
  unnest(c(attr_l, attr_r, attr_name_l, attr_name_r))


tbl <- reactable(
  qc_l,
  sortable = FALSE,
  searchable = FALSE,
  elementId = "qatarcars-table",
  searchMethod = JS("function(rows, columnIds, searchValue) {
    // Search by car (make + model) instead of by attribute rows
    // Group rows by car and filter entire groups
    const searchLower = searchValue.toLowerCase();
    const matchingIndices = new Set();
    
    // Check each row to see if its car matches the search
    rows.forEach(function(row, idx) {
      const make = String(row.values['make'] || '').toLowerCase();
      const model = String(row.values['model'] || '').toLowerCase();
      const carName = make + ' ' + model;
      
      // If this car matches, include all 8 rows for this car
      if (carName.includes(searchLower)) {
        // Calculate the start of this car's group (rows come in groups of 8)
        const groupStart = Math.floor(idx / 8) * 8;
        for (let i = groupStart; i < groupStart + 8; i++) {
          if (i < rows.length) {
            matchingIndices.add(i);
          }
        }
      }
    });
    
    // Return rows in matching cars
    return rows.filter(function(row, idx) {
      return matchingIndices.has(idx);
    });
  }"),
  defaultPageSize = 8,
  showPageInfo = FALSE,
  paginationType = "simple",
  width = 450,
  borderless = TRUE,
  columns = list(
    attr_l = colDef(
      name = "",
      html = TRUE,
      cell = function(value, index) {
        attr_id <- qc_l$attr_name_l[index]
        # Handle NA attr_id
        if (is.na(attr_id)) {
          return(sprintf('<div style="height: %dpx;"></div>', CELL_HEIGHT))
        }
        
        attr_name <- toupper(attr_id)
        val_display <- as.character(value)
        
        # LOGO: show car brand logo
        if (attr_name == "LOGO" && !is.na(val_display) && val_display != "") {
          value_html <- sprintf('<img src="%s" style="height: 55px; max-width: 180px; object-fit: contain; vertical-align: middle;" />', val_display)
          return(create_cell_html("", value_html, height = CELL_HEIGHT))
        # TYPE: show icon with text
        } else if (attr_name == "TYPE" && !is.na(val_display) && val_display != "") {
          img_src <- knitr::image_uri(sprintf("2025/2025-week_50/img/%s.png", tolower(val_display)))
          img_html <- sprintf('<img src="%s" style="width:32px; margin-top:-4px; margin-right: 5px; vertical-align: middle; display: inline-block;" />', img_src)
          value_html <- sprintf('<span style="display: flex; align-items: center; justify-content: center;"><span>%s</span><span>%s</span></span>', img_html, val_display)
        # SEATING: show seat icons
        } else if (attr_name == "SEATING" && !is.na(val_display) && val_display != "") {
          seats <- suppressWarnings(as.integer(val_display))
          max_seats <- max(unique(qatarcars$seating), na.rm = TRUE)
          seat_icons <- vapply(
            seq_len(max_seats),
            function(i) {
              icon_file <- if (i <= seats) "car-seat.png" else "car-seat-empty.png"
              icon_src <- knitr::image_uri(sprintf("2025/2025-week_50/img/%s", icon_file))
              sprintf('<img src="%s" style="width:18px; margin:0 -1px; vertical-align:middle;" />', icon_src)
            },
            character(1)
          )
          value_html <- sprintf('<span style="display: flex; align-items: center; justify-content: center;">%s</span>', paste(seat_icons, collapse = ""))
        } else {
          value_html <- val_display
        }
        
        create_cell_html(attr_name, value_html)
      }
    ),
    attr_r = colDef(
      name = "",
      html = TRUE,
      cell = function(value, index) {
        attr_id <- qc_l$attr_name_r[index]
        # Handle NA attr_id (logo row)
        if (is.na(attr_id)) {
          return(sprintf('<div style="height: %dpx;"></div>', CELL_HEIGHT))
        }
        # Display label: rename '0-100 km/h' for display
        attr_name <- if (attr_id == "0-100 km/h") "0-100 KM/H" else toupper(attr_id)
        # For bar calculation, use 'performance' for max lookup
        max_lookup <- if (attr_id == "0-100 km/h") "performance" else attr_id
        max_v <- max_vals[max_lookup]
        # Convert value to numeric for bar calculation
        value_num <- suppressWarnings(as.numeric(value))
        # Determine if "smaller is better" (economy and 0-100 km/h)
        is_smaller_better <- attr_id %in% c("economy", "0-100 km/h")
        # Calculate bar width and color based on "better" direction
        width_pct <- if (is.na(value_num)) 0 else {
          if (is_smaller_better) (max_v - value_num) / max_v else value_num / max_v
        }
        bar_color <- if (is_smaller_better) COLOR_BETTER_LOWER else COLOR_BETTER_HIGHER
        
        bar_styles <- bar_style(width = width_pct, color = bar_color)
        style_str <- paste(names(bar_styles), bar_styles, sep = ": ", collapse = "; ")
        
        units <- switch(attr_id,
          "length" = "m",
          "width" = "m",
          "height" = "m",
          "horsepower" = "hp",
          "trunk" = "L",
          "economy" = "L/100 km",
          "0-100 km/h" = "s",
          "mass" = "kg",
          ""
        )
        
        val_display <- if (is.na(value)) "N/A" else paste0(as.character(value), if (units != "") paste0(" ", units) else "")
        
        bar_html <- sprintf('<div style="background-color: #e1e1e1; %s; height: %dpx; min-height: 3px; min-width: 40px; width: 100%%; margin: 3px 0 3px 0; border-radius: 2px;">&nbsp;</div>', style_str, BAR_HEIGHT)
        
        create_cell_html(attr_name, val_display, bar_html = bar_html)
      }
    ),
    make = colDef(show = FALSE),
    model = colDef(show = FALSE),
    header_text = colDef(show = FALSE),
    logo_src = colDef(show = FALSE),
    attr_name_l = colDef(show = FALSE),
    attr_name_r = colDef(show = FALSE)
  ),
    columnGroups = list(
      colGroup(
        header = JS("function(column, state) {
          const rows = state.sortedData;
          const page = state.page;
          const pageSize = state.pageSize;
          const startIdx = page * pageSize;
          if (rows.length > startIdx) {
            return '<div style=\"font-family: Fjalla One, Montserrat, Arial, sans-serif; font-weight: bold; font-size: 42px; letter-spacing: 1px; color: #222; background: #e9ecef; padding: 8px; border-radius: 12px;\">' + rows[startIdx].header_text + '</div>';
          }
          return '';
        }"),
        html = TRUE,
        columns = c("attr_l", "attr_r")
      )
    ),
  theme = reactableTheme(
      style = list(
        fontFamily = FONT_FAMILY,
        fontSize = "14px",
        background = "#f8f9fa",
        border = "8px solid #222",
        borderRadius = "18px",
        boxShadow = "0 2px 12px rgba(0,0,0,0.04)",
        boxSizing = "border-box",
        padding = "8px 8px 72px 8px"
      ),
    headerStyle = list(
      display = "none"
    ),
    rowStyle = list(
      padding = "2px"
    ),
    searchInputStyle = list(
      width = "200px",
      marginBottom = "0"
    )
  ),
  language = reactableLang(
    searchPlaceholder = "Search cars...",
    searchLabel = "Search cars"
  )
)

qc_table <- browsable(
  tagList(
    div(
      style = "position: relative; display: inline-block;",
      tbl |> 
        reactablefmtr::google_font("Montserrat", font_weight = c(400, 700)) |> 
        reactablefmtr::google_font("Fjalla One", font_weight = 400),
      tags$input(
        type = "text",
        placeholder = "Search cars...",
        style = "position: absolute; bottom: 24px; left: 8px; padding: 6px 8px; margin-left: 12px; width: 180px; border: 1px solid #ddd; border-radius: 4px; font-family: Montserrat, Arial, sans-serif; font-size: 14px;",
        oninput = "Reactable.setSearch('qatarcars-table', this.value)"
      )
    ),
    tags$style(HTML("
      .reactable .rt-pagination {
        position: absolute !important;
        bottom: 16px !important;
        right: 8px !important;
        border-top: none !important;
        padding: 0 !important;
        margin: 0 !important;
      }
    "))
  )
)

qc_table

save_html(qc_table, here::here("2025/2025-week_50/plots/qatarcars.html"))
