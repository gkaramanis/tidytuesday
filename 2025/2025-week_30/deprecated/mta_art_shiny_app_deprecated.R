library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(htmltools)

# Read in data
mta_art <- read_csv(here::here("2025/2025-week_30/data/mta_art.csv")) %>%
  # Clean up strange characters in text fields
  mutate(
    art_description = str_replace_all(art_description, "Ò", "\"") %>%
      str_replace_all("Ó", "\"") %>%
      str_replace_all("Õ", "'") %>%
      str_replace_all("Ô", "'") %>%
      str_replace_all("Ð", "–"),
    art_title = str_replace_all(art_title, "Ò", "\"") %>%
      str_replace_all("Ó", "\"") %>%
      str_replace_all("Õ", "'") %>%
      str_replace_all("Ô", "'") %>%
      str_replace_all("Ð", "–"),
    artist = str_replace_all(artist, "Ò", "\"") %>%
      str_replace_all("Ó", "\"") %>%
      str_replace_all("Õ", "'") %>%
      str_replace_all("Ô", "'") %>%
      str_replace_all("Ð", "–")
  )

sub_lines_sf <- read_sf(here::here("2025/2025-week_30/data/Subway.geojson")) %>% 
  janitor::clean_names()

sub_stations_sf <- read_sf(here::here("2025/2025-week_30/data/SubwayStation_view_-2956341149621885519.geojson")) %>% 
  janitor::clean_names()

sub_stations <- read_csv(here::here("2025/2025-week_30/data/MTA_Subway_Stations_and_Complexes.csv")) %>% 
  janitor::clean_names() %>% 
  separate_longer_delim(station_i_ds, delim = "; ") %>% 
  mutate(station_id = as.numeric(station_i_ds)) %>%
  # Rename daytime_routes to line for consistency
  rename(line = daytime_routes)

# Join spatial data with station metadata and artwork
# The spatial data has station_id which matches the metadata station_id
sub_stations_sf_art <- sub_stations_sf %>%
  # Get unique spatial stations
  group_by(station_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  # Join with station metadata using station_id
  left_join(
    sub_stations %>%
      group_by(station_id) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(station_id, stop_name, line),
    by = "station_id"
  ) %>%
  # Join with artwork data using stop_name
  left_join(
    mta_art,
    by = c("stop_name" = "station_name")
  ) %>%
  # Clean up line info - use metadata line first, then spatial line  
  mutate(
    line_info = case_when(
      !is.na(line.y) ~ line.y,  # Use full line info from metadata
      !is.na(line.x) ~ str_trim(str_replace_all(line.x, " LINE$", "")),
      TRUE ~ "Unknown"
    )
  ) %>%
  st_sf()

# Transform to WGS84 for leaflet
sub_lines_sf_wgs84 <- st_transform(sub_lines_sf, 4326)
sub_stations_sf_art_wgs84 <- st_transform(sub_stations_sf_art, 4326)

# Define standard NYC subway lines
standard_lines <- c("1", "2", "3", "4", "5", "6", "7", 
                   "A", "B", "C", "D", "E", "F", "G", 
                   "J", "L", "M", "N", "Q", "R", "S", "T", "W", "Z")

# Extract actual subway lines from the complex line_info data
all_lines <- sub_stations_sf_art_wgs84 |> 
  pull(line_info) |> 
  # Split by various separators and extract individual characters/numbers
  str_split("[,\\s]+") |>
  unlist() |>
  str_trim() |>
  # Extract single characters and numbers that match standard lines
  str_extract_all("[A-Z0-9]") |>
  unlist() |>
  unique() |>
  # Keep only standard NYC subway lines
  intersect(standard_lines) |>
  sort()

# Create a simple function to get display names for lines
get_line_display_name <- function(line_code) {
  # Return simple line names
  paste("Line", line_code)
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;700&family=Inter:wght@300;400;600&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body {
        font-family: 'Inter', sans-serif;
        background: #f8fafc;
        margin: 0;
        padding: 0;
        color: #1e293b;
      }
      
      .main-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 5.5rem;
        font-weight: 700;
        color: #0f172a;
        text-align: center;
        margin: 25px 0 5px 0;
        text-transform: uppercase;
        letter-spacing: 2px;
      }
      
      .subtitle {
        font-family: 'Inter', sans-serif;
        font-size: 2rem;
        font-weight: 500;
        color: #64748b;
        text-align: center;
        margin-bottom: 25px;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .control-panel {
        background: #ffffff;
        border-radius: 0;
        padding: 15px;
        margin: 0;
        border: 2px solid #e2e8f0;
        border-top: 4px solid #3b82f6;
      }
      
      /* Responsive grid for mobile */
      @media (max-width: 768px) {
        .control-panel .row > div[class*='col-'] {
          margin-bottom: 20px;
        }
        
        .control-panel .row > div[class*='col-']:last-child {
          margin-bottom: 0;
        }
        
        .legend-spotlight-grid {
          grid-template-columns: 1fr !important;
          gap: 20px !important;
        }
        
        .main-title {
          font-size: 3rem !important;
        }
        
        .subtitle {
          font-size: 1.2rem !important;
        }
      }
      
      .sidebar-panel {
        background: #ffffff;
        border-radius: 0;
        padding: 20px;
        margin: 0;
        border: 2px solid #e2e8f0;
        border-left: 4px solid #3b82f6;
        height: calc(100vh - 320px);
        overflow: visible;
      }
      
      .search-input {
        width: 100%;
        padding: 15px 20px;
        border: 2px solid #e2e8f0;
        border-radius: 0;
        font-size: 16px;
        font-weight: 600;
        background: #ffffff;
        color: #1e293b;
        text-transform: uppercase;
      }
      
      .search-input:focus {
        outline: none;
        border-color: #3b82f6;
      }
      
      .search-input::placeholder {
        color: #64748b;
        font-weight: 600;
      }
      
      .checkbox-group {
        max-height: calc(100vh - 420px);
        overflow-y: auto;
        padding: 10px 0;
        background: #ffffff;
        border: none;
      }
      
      .checkbox-group::-webkit-scrollbar {
        width: 2px;
      }
      
      .checkbox-group::-webkit-scrollbar-track {
        background: #f1f5f9;
        border-radius: 3px;
      }
      
      .checkbox-group::-webkit-scrollbar-thumb {
        background: #8b5cf6;
        border-radius: 3px;
      }
      
      .checkbox-group::-webkit-scrollbar-thumb:hover {
        background: #7c3aed;
      }
      
      .checkbox-group .checkbox {
        margin: 8px 0;
      }
      
      .checkbox-group label {
        color: #1e293b;
        font-weight: 600;
        font-size: 14px;
        text-transform: uppercase;
      }
      
      .line-checkbox {
        display: flex;
        align-items: center;
        margin: 8px 0;
      }
      
      .line-checkbox label {
        font-weight: 600;
        font-size: 14px;
        text-transform: uppercase;
        margin: 0;
        padding-left: 5px;
        cursor: pointer;
      }
      
      .legend-item {
        display: flex;
        align-items: center;
        margin: 0 15px 0 0;
        font-size: 14px;
        font-weight: 600;
        color: #1e293b;
        text-transform: uppercase;
      }
      
      .legend-container {
        display: flex;
        align-items: flex-end;
        justify-content: center;
        flex-wrap: wrap;
        padding: 10px 0;
        height: 100%;
      }
      
      .legend-label {
        margin: 0 8px 0 0;
        font-size: 14px;
        font-weight: 600;
        color: #1e293b;
        text-transform: uppercase;
      }
      
      .legend-dot {
        width: 16px;
        height: 16px;
        border-radius: 0;
        margin-right: 12px;
        border: 2px solid #1e293b;
      }
      
      .featured-container {
        background: linear-gradient(135deg, #3b82f6 0%, #8b5cf6 100%);
        color: #ffffff;
        padding: 20px;
        border-radius: 0;
        margin: 0;
        text-align: left;
        border: 2px solid #3b82f6;
        min-height: 120px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);
        transition: all 0.3s ease;
      }
      
      .featured-container:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(59, 130, 246, 0.4);
        border-color: #8b5cf6;
      }
      
      .featured-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 0.9rem;
        font-weight: 700;
        color: #ffffff;
        text-transform: uppercase;
        margin: 0 0 8px 0;
        letter-spacing: 1px;
        text-align: center;
      }
      
      .featured-artwork-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1.3rem;
        font-weight: 700;
        color: #ffffff;
        text-transform: uppercase;
        margin: 0 0 8px 0;
        letter-spacing: 1px;
        line-height: 1.2;
        text-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      
      .featured-artist {
        font-family: 'Inter', sans-serif;
        font-size: 1rem;
        font-weight: 600;
        color: rgba(255,255,255,0.9);
        margin: 0 0 8px 0;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        line-height: 1.2;
      }
      
      .featured-station {
        font-family: 'Inter', sans-serif;
        font-size: 0.9rem;
        font-weight: 600;
        color: rgba(255,255,255,0.8);
        margin: 0;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-style: italic;
      }
      
      .featured-row {
        display: flex;
        justify-content: space-between;
        margin: 8px 0;
        font-family: 'Space Grotesk', sans-serif;
      }
      
      .featured-item {
        font-size: 1.8rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      .map-container {
        border-radius: 0;
        overflow: hidden;
        margin: 0;
        border: 2px solid #e2e8f0;
        border-top: 4px solid #3b82f6;
        height: calc(100vh - 320px);
      }
      
      .footer {
        background: #1e293b;
        color: #ffffff;
        padding: 20px;
        margin-top: 40px;
        font-family: 'Inter', sans-serif;
        font-size: 0.9rem;
        line-height: 1.4;
      }
      
      .footer-content {
        max-width: 1200px;
        margin: 0 auto;
        text-align: center;
      }
      
      .footer a {
        color: #8b5cf6;
        text-decoration: none;
      }
      
      .footer a:hover {
        color: #3b82f6;
      }
      
      .spotlight-container {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        padding: 12px;
        margin-top: 0;
        border-left: 4px solid #8b5cf6;
        min-height: 80px;
        border-radius: 0;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      
      .spotlight-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 0.9rem;
        font-weight: 700;
        color: #0f172a;
        text-transform: uppercase;
        margin: 0 0 8px 0;
        letter-spacing: 1px;
      }
      
      .spotlight-artwork {
        font-size: 0.85rem;
        font-weight: 600;
        color: #1e293b;
        margin: 0 0 4px 0;
        text-transform: uppercase;
        line-height: 1.2;
      }
      
      .spotlight-station {
        font-size: 0.7rem;
        color: #8b5cf6;
        margin: 0;
        text-transform: uppercase;
        font-weight: 600;
        line-height: 1.2;
      }
      
      /* Map legend styling */
      .map-legend {
        font-family: 'Inter', sans-serif !important;
        background: rgba(255, 255, 255, 0.95) !important;
        border: 2px solid #e2e8f0 !important;
        border-radius: 0 !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
      }
      
      .map-legend .legend-title {
        font-family: 'Space Grotesk', sans-serif !important;
        font-weight: 700 !important;
        text-transform: uppercase !important;
        letter-spacing: 1px !important;
        color: #0f172a !important;
        font-size: 14px !important;
        margin-bottom: 8px !important;
      }
      
      .map-legend .legend-text {
        font-family: 'Inter', sans-serif !important;
        font-weight: 600 !important;
        text-transform: uppercase !important;
        color: #1e293b !important;
        font-size: 12px !important;
      }
      
      /* Inline stats under search */
      .stats-inline-container {
        margin-top: 10px;
        background: #f8fafc;
        padding: 10px 15px;
        border-radius: 4px;
      }
      
      .stats-inline-label {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 0.8rem;
        font-weight: 700;
        color: #0f172a;
        text-transform: uppercase;
        margin: 0 0 5px 0;
        letter-spacing: 1px;
      }
      
      .stats-inline {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        font-family: 'Inter', sans-serif;
        font-size: 0.85rem;
        font-weight: 600;
        text-transform: uppercase;
        line-height: 1.2;
      }
      
      .stats-inline-artist {
        color: #8b5cf6;
      }
      
      .stats-inline-line {
        color: #3b82f6;
      }
    "))
  ),
  
  div(class = "main-title", "NEW YORK'S UNDERGROUND GALLERY"),
  
    # Top control bar with search, featured artwork, and current view stats
  fluidRow(
    div(class = "control-panel", style = "margin: 0 20px 20px 20px;",
      fluidRow(
        column(width = 6,
          # Search section
          tags$input(type = "text", 
                    id = "search_term",
                    class = "search-input",
                    placeholder = "SEARCH STATIONS, ARTISTS, ARTWORKS...",
                    value = ""),
          # Current view stats under search
          htmlOutput("stats_display_inline")
        ),
        
        column(width = 6,
          # Featured Artwork
          div(class = "featured-container",
            htmlOutput("artwork_spotlight")
          )
        )
      )
    )
  ),

  # Main content: Map + Right Sidebar
  fluidRow(
    column(9,
      div(class = "map-container", style = "margin: 20px 0 20px 20px;",
        leafletOutput("subway_map", height = "100%")
      )
    ),
    
    column(3,
      div(class = "sidebar-panel", style = "margin: 20px 20px 20px 0;",
        # Lines selection only
        tags$div(style = "color: #1e293b; font-weight: 600; font-size: 16px; margin-bottom: 10px;",
          checkboxInput("select_all", "ALL LINES", value = TRUE)
        ),
        div(class = "checkbox-group",
          htmlOutput("colored_line_checkboxes")
        )
      )
    )
  ),
  
  # Footer
  div(class = "footer",
    div(class = "footer-content",
      p("Data sources: ",
        a(href = "https://www.mta.info/agency/arts-design", "MTA Arts & Design Collection", target = "_blank"),
        " • Geographic data: ",
        a(href = "https://github.com/CityOfNewYork/nyc-geo-metadata/tree/main", "NYC OpenData", target = "_blank"),
        " • Data as of December 2024",
        br(),
        "Created by ", 
        a(href = "https://github.com/gkaramanis", "Georgios Karamanis", target = "_blank"),
        p("Note: Not all artworks are shown due to missing location or description information.")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values for selected lines
  selected_lines_reactive <- reactiveVal(all_lines)
  
  # Reactive value to store current featured artwork
  featured_artwork_reactive <- reactiveVal(NULL)
  
  # Render colored checkboxes for lines
  output$colored_line_checkboxes <- renderUI({
    selected_lines <- selected_lines_reactive()
    
    checkboxes <- lapply(all_lines, function(line) {
      is_checked <- line %in% selected_lines
      display_name <- get_line_display_name(line)
      
      div(class = "line-checkbox",
        tags$input(
          type = "checkbox",
          id = paste0("line_", gsub("[^A-Za-z0-9]", "_", line)),
          checked = if(is_checked) "checked" else NULL,
          onchange = paste0("Shiny.setInputValue('line_checkbox_", gsub("[^A-Za-z0-9]", "_", line), "', this.checked);")
        ),
        tags$label(
          `for` = paste0("line_", gsub("[^A-Za-z0-9]", "_", line)),
          display_name,
          style = "color: #1e293b; margin: 0; padding-left: 5px; cursor: pointer; font-weight: 600; font-size: 14px; text-transform: uppercase;"
        )
      )
    })
    
    do.call(tagList, checkboxes)
  })
  
  # Handle individual line checkbox changes
  observeEvent({
    # Create dynamic observers for each line
    lapply(all_lines, function(line) {
      input_name <- paste0("line_checkbox_", gsub("[^A-Za-z0-9]", "_", line))
      input[[input_name]]
    })
  }, {
    # Update selected lines based on individual checkboxes
    current_selected <- c()
    for(line in all_lines) {
      input_name <- paste0("line_checkbox_", gsub("[^A-Za-z0-9]", "_", line))
      if(!is.null(input[[input_name]]) && input[[input_name]]) {
        current_selected <- c(current_selected, line)
      }
    }
    selected_lines_reactive(current_selected)
    
    # Update select all checkbox
    if (length(current_selected) == length(all_lines)) {
      updateCheckboxInput(session, "select_all", value = TRUE)
    } else if (length(current_selected) == 0) {
      updateCheckboxInput(session, "select_all", value = FALSE)
    } else {
      updateCheckboxInput(session, "select_all", value = FALSE)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # Handle select/deselect all
  observeEvent(input$select_all, {
    if (input$select_all) {
      selected_lines_reactive(all_lines)
    } else {
      selected_lines_reactive(character(0))
    }
  })
  
  # Reactive data filtering with search
  filtered_lines_data <- reactive({
    req(selected_lines_reactive())
    # Since the subway lines GeoJSON might have different naming,
    # we'll skip line filtering for the geographic lines for now
    # and focus on showing all subway lines on the map
    sub_lines_sf_wgs84
  })
  
  # Random artwork spotlight
  random_artwork <- reactive({
    # Update every 30 seconds
    invalidateLater(30000)
    
    # Get all artworks from current filtered data
    stations <- filtered_stations_data() |>
      filter(!is.na(art_title))
    
    if (nrow(stations) > 0) {
      # Select a random artwork
      sample_artwork <- stations[sample(nrow(stations), 1), ]
      # Store in reactive value for click functionality
      featured_artwork_reactive(sample_artwork)
      return(sample_artwork)
    } else {
      featured_artwork_reactive(NULL)
      return(NULL)
    }
  })
  
  filtered_stations_data <- reactive({
    req(selected_lines_reactive())
    
    stations <- sub_stations_sf_art_wgs84
    selected_lines <- selected_lines_reactive()
    
    # Filter by selected lines using a simpler approach
    if (length(selected_lines) > 0) {
      # For each station, check if any of its lines match the selected lines
      stations <- stations |>
        filter(
          if_else(
            is.na(line_info) | line_info == "Unknown",
            "Unknown" %in% selected_lines,
            # Check if any character in line_info matches selected lines
            map_lgl(line_info, ~ any(str_extract_all(.x, "[A-Z0-9]")[[1]] %in% selected_lines))
          )
        )
    } else {
      stations <- stations |> filter(FALSE)
    }
    
    # Apply search filter
    if (!is.null(input$search_term) && nchar(trimws(input$search_term)) > 0) {
      search_term <- tolower(trimws(input$search_term))
      stations <- stations |>
        filter(
          str_detect(tolower(stop_name), search_term) |
          str_detect(tolower(coalesce(artist, "")), search_term) |
          str_detect(tolower(coalesce(art_title, "")), search_term) |
          str_detect(tolower(coalesce(art_description, "")), search_term)
        )
    }
    
    return(stations)
  })
  
  # Inline statistics output under search
  output$stats_display_inline <- renderUI({
    req(selected_lines_reactive())
    
    tryCatch({
      stations <- filtered_stations_data()
      total_stations <- nrow(stations)
      art_stations <- sum(!is.na(stations$art_title))
      unique_artists <- length(unique(stations$artist[!is.na(stations$artist)]))
      
      div(class = "stats-inline-container",
        div(class = "stats-inline-label", "CURRENT VIEW"),
        div(class = "stats-inline",
          span(class = "stats-inline-artist", paste(unique_artists, "ARTISTS")),
          span(class = "stats-inline-artist", paste(art_stations, "ARTWORKS")),
          span(class = "stats-inline-line", paste(length(selected_lines_reactive()), "LINES")),
          span(class = "stats-inline-line", paste(total_stations, "STATIONS"))
        )
      )
    }, error = function(e) {
      div(class = "stats-inline-container",
        div(class = "stats-inline-label", "CURRENT VIEW"),
        div(class = "stats-inline",
          span(class = "stats-inline-artist", "NO DATA AVAILABLE")
        )
      )
    })
  })
  
  # Random artwork spotlight with enhanced styling
  output$artwork_spotlight <- renderUI({
    req(selected_lines_reactive())
    
    artwork <- random_artwork()
    
    if (!is.null(artwork) && nrow(artwork) > 0) {
      # Create clickable content that will trigger map popup
      tagList(
        div(
          class = "featured-title", 
          "FEATURED",
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('featured_artwork_click', Math.random());"
        ),
        div(
          class = "featured-artwork-title", 
          ifelse(!is.na(artwork$art_title) && nchar(trimws(artwork$art_title)) > 0, 
                 trimws(artwork$art_title), "UNTITLED"),
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('featured_artwork_click', Math.random());"
        ),
        div(
          class = "featured-artist",
          ifelse(!is.na(artwork$artist) && nchar(trimws(artwork$artist)) > 0, 
                 paste("BY", trimws(artwork$artist)), "BY UNKNOWN ARTIST"),
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('featured_artwork_click', Math.random());"
        ),
        div(
          class = "featured-station",
          paste("AT", toupper(artwork$stop_name)),
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('featured_artwork_click', Math.random());"
        )
      )
    } else {
      tagList(
        div(class = "featured-title", "FEATURED"),
        div(class = "featured-artwork-title", "SELECT LINES"),
        div(class = "featured-artist", "TO SEE ARTWORK")
      )
    }
  })
  
  # Handle featured artwork click to show popup on map
  observeEvent(input$featured_artwork_click, {
    artwork <- featured_artwork_reactive()
    
    if (!is.null(artwork) && nrow(artwork) > 0) {
      # Extract coordinates from the geometry
      coords <- st_coordinates(artwork)
      if (nrow(coords) > 0) {
        lng <- coords[1, "X"]
        lat <- coords[1, "Y"]
        
        # Create popup content matching the map format
        popup_html <- paste0(
          "<div style='font-family: \"Inter\", sans-serif; max-width: 400px; padding: 15px; background: #ffffff; color: #1e293b; border: 2px solid #3b82f6;'>",
          "<h3 style='color: #0f172a; font-weight: 700; margin: 0 0 15px 0; font-size: 1.4rem; text-align: center; text-transform: uppercase; border-bottom: 2px solid #e2e8f0; padding-bottom: 10px;'>", 
          artwork$stop_name, "</h3>",
          "<p style='color: #64748b; text-align: center; margin: 0 0 20px 0; font-size: 1.1rem; font-weight: 600; text-transform: uppercase;'>1 ARTWORK</p>",
          "<div style='border: 1px solid #e2e8f0; margin: 10px 0; padding: 15px; background: #f8fafc;'>",
          "<h4 style='color: #0f172a; font-weight: 700; margin: 0 0 8px 0; font-size: 1.2rem; text-transform: uppercase; font-family: \"Space Grotesk\", sans-serif;'>",
          ifelse(!is.na(artwork$art_title) && nchar(trimws(artwork$art_title)) > 0, 
                 trimws(artwork$art_title), "UNTITLED"), "</h4>",
          "<p style='color: #1e293b; margin: 0 0 10px 0; font-size: 1rem; font-weight: 600; text-transform: uppercase;'>",
          ifelse(!is.na(artwork$artist) && nchar(trimws(artwork$artist)) > 0, 
                 trimws(artwork$artist), "UNKNOWN ARTIST"), "</p>",
          # Date and material
          ifelse((!is.na(artwork$art_date) && nchar(trimws(artwork$art_date)) > 0) || 
                 (!is.na(artwork$art_material) && nchar(trimws(artwork$art_material)) > 0),
                 paste0("<p style='color: #64748b; font-size: 0.9rem; margin: 0 0 10px 0; font-weight: 600; text-transform: uppercase;'>",
                        ifelse(!is.na(artwork$art_date) && nchar(trimws(artwork$art_date)) > 0, 
                               trimws(artwork$art_date), ""),
                        ifelse((!is.na(artwork$art_date) && nchar(trimws(artwork$art_date)) > 0) && 
                               (!is.na(artwork$art_material) && nchar(trimws(artwork$art_material)) > 0), 
                               " • ", ""),
                        ifelse(!is.na(artwork$art_material) && nchar(trimws(artwork$art_material)) > 0, 
                               trimws(artwork$art_material), ""),
                        "</p>"),
                 ""),
          # Description and buttons
          ifelse(!is.na(artwork$art_description) && nchar(trimws(artwork$art_description)) > 0,
                 paste0("<div id='desc-short-featured'>",
                        "<p style='color: #1e293b; font-size: 0.9rem; line-height: 1.4; margin: 0 0 10px 0;'>",
                        ifelse(nchar(trimws(artwork$art_description)) > 80,
                               paste0(substr(trimws(artwork$art_description), 1, 80), "..."),
                               trimws(artwork$art_description)),
                        "</p>",
                                "<div style='display: flex; gap: 8px; margin-top: 10px;'>",
                        ifelse(nchar(trimws(artwork$art_description)) > 80,
                               "<button onclick=\"document.getElementById('desc-short-featured').style.display='none'; document.getElementById('desc-full-featured').style.display='block';\" style='background: #3b82f6; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; cursor: pointer; font-size: 0.8rem;'>READ MORE</button>",
                               ""),
                        ifelse(!is.na(artwork$art_image_link) && nchar(trimws(artwork$art_image_link)) > 0,
                               paste0("<a href='", trimws(artwork$art_image_link), "' target='_blank' style='background: #8b5cf6; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; text-decoration: none; cursor: pointer; font-family: \"Space Grotesk\", sans-serif; display: inline-block; font-size: 0.8rem;'>VIEW ON MTA</a>"),
                               ""),
                        "</div>",
                        "</div>",
                        ifelse(nchar(trimws(artwork$art_description)) > 80,
                               paste0("<div id='desc-full-featured' style='display: none;'>",
                                      "<p style='color: #1e293b; font-size: 0.9rem; line-height: 1.4; margin: 0 0 10px 0;'>",
                                      trimws(artwork$art_description), "</p>",
                                      "<div style='display: flex; gap: 8px; margin-top: 10px;'>",
                                      "<button onclick=\"document.getElementById('desc-full-featured').style.display='none'; document.getElementById('desc-short-featured').style.display='block';\" style='background: #64748b; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; cursor: pointer; font-size: 0.8rem;'>READ LESS</button>",
                                      ifelse(!is.na(artwork$art_image_link) && nchar(trimws(artwork$art_image_link)) > 0,
                                             paste0("<a href='", trimws(artwork$art_image_link), "' target='_blank' style='background: #8b5cf6; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; text-decoration: none; cursor: pointer; font-family: \"Space Grotesk\", sans-serif; display: inline-block; font-size: 0.8rem;'>VIEW ON MTA</a>"),
                                             ""),
                                      "</div>",
                                      "</div>"),
                               "")),
                 ""),
          "</div>",
          "</div>"
        )
        
        # Center map on artwork and show popup
        leafletProxy("subway_map") %>%
          setView(lng = lng, lat = lat, zoom = 15) %>%
          addPopups(lng = lng, lat = lat, popup = popup_html)
      }
    }
  })
  
  # Render leaflet map with artistic styling
  output$subway_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.9857, lat = 40.7484, zoom = 11) %>%
      addLegend(
        position = "topleft",
        colors = c("#3b82f6", "#64748b"),
        labels = c("WITH ART", "WITHOUT ART"),
        title = "STATIONS",
        opacity = 1,
        className = "map-legend"
      )
  })
  
  # Update map based on filtered data with detailed artwork cards
  observe({
    req(selected_lines_reactive())
    
    tryCatch({
      leafletProxy("subway_map") %>%
        clearShapes() %>%
        clearMarkers()
      
      if (length(selected_lines_reactive()) > 0) {
        lines_data <- filtered_lines_data()
        stations_data <- filtered_stations_data()
        
        # Add subway lines with official MTA colors
        if (nrow(lines_data) > 0) {
          # The lines data structure might not have individual line info
          # For now, add all lines with a default color and let stations show the line colors
          leafletProxy("subway_map") %>%
            addPolylines(
              data = lines_data,
              color = "#34495e",  # Default gray for all lines
              weight = 3,
              opacity = 0.7,
              popup = ~paste("Subway Line")
            )
        }
        
        # Separate stations with and without art
        stations_with_art <- stations_data %>% 
          filter(!is.na(art_title))
        stations_without_art <- stations_data %>% 
          filter(is.na(art_title))
        
        # Add stations without artwork - use grey color
        if (nrow(stations_without_art) > 0) {
          for (i in seq_len(nrow(stations_without_art))) {
            station <- stations_without_art[i, ]
            
            leafletProxy("subway_map") %>%
              addCircleMarkers(
                data = station,
                radius = 4,
                color = "#64748b",
                fillColor = "#64748b",
                fillOpacity = 0.6,
                stroke = TRUE,
                weight = 2,
                popup = paste0(
                  "<div style='max-width: 250px; text-align: center; padding: 15px;'>",
                  "<h3>", station$stop_name, "</h3>",
                  ifelse(!is.na(station$line_info) && station$line_info != "Unknown",
                         paste0("<p>Lines: ", station$line_info, "</p>"),
                         ""),
                  "<p>NO ART INSTALLATIONS</p>",
                  "</div>"
                )
              )
          }
        }
        
        # Add stations with artwork - use blue color and detailed popups
        if (nrow(stations_with_art) > 0) {
          unique_art_stations <- stations_with_art %>%
            distinct(stop_name, .keep_all = TRUE)
          
          for (i in seq_len(nrow(unique_art_stations))) {
            station <- unique_art_stations[i, ]
            
            # Count artworks at this station
            current_stop <- station$stop_name
            station_artworks <- stations_with_art[stations_with_art$stop_name == current_stop, ]
            artwork_count <- nrow(station_artworks)
            
            # Create detailed popup content
            popup_html <- paste0(
              "<div style='font-family: \"Inter\", sans-serif; max-width: 400px; padding: 15px; background: #ffffff; color: #1e293b; border: 2px solid #3b82f6;'>",
              "<h3 style='color: #0f172a; font-weight: 700; margin: 0 0 15px 0; font-size: 1.4rem; text-align: center; text-transform: uppercase; border-bottom: 2px solid #e2e8f0; padding-bottom: 10px;'>", 
              station$stop_name, "</h3>",
              "<p style='color: #64748b; text-align: center; margin: 0 0 20px 0; font-size: 1.1rem; font-weight: 600; text-transform: uppercase;'>", artwork_count, " ARTWORK", ifelse(artwork_count > 1, "S", ""), "</p>",
              
              "<div style='border: 1px solid #e2e8f0; margin: 10px 0; padding: 15px; background: #f8fafc;'>",
              "<h4 style='color: #0f172a; font-weight: 700; margin: 0 0 8px 0; font-size: 1.2rem; text-transform: uppercase; font-family: \"Space Grotesk\", sans-serif;'>",
              ifelse(!is.na(station$art_title) && nchar(trimws(station$art_title)) > 0, 
                     trimws(station$art_title), "UNTITLED"), "</h4>",
              "<p style='color: #1e293b; margin: 0 0 10px 0; font-size: 1rem; font-weight: 600; text-transform: uppercase;'>",
              ifelse(!is.na(station$artist) && nchar(trimws(station$artist)) > 0, 
                     trimws(station$artist), "UNKNOWN ARTIST"), "</p>"
            )
            
            # Date and material
            if ((!is.na(station$art_date) && nchar(trimws(station$art_date)) > 0) || 
                (!is.na(station$art_material) && nchar(trimws(station$art_material)) > 0)) {
              popup_html <- paste0(popup_html,
                "<p style='color: #64748b; font-size: 0.9rem; margin: 0 0 10px 0; font-weight: 600; text-transform: uppercase;'>",
                ifelse(!is.na(station$art_date) && nchar(trimws(station$art_date)) > 0, 
                       trimws(station$art_date), ""),
                ifelse((!is.na(station$art_date) && nchar(trimws(station$art_date)) > 0) && 
                       (!is.na(station$art_material) && nchar(trimws(station$art_material)) > 0), 
                       " • ", ""),
                ifelse(!is.na(station$art_material) && nchar(trimws(station$art_material)) > 0, 
                       trimws(station$art_material), ""),
                "</p>"
              )
            }
            
            # Description with read more functionality
            if (!is.na(station$art_description) && nchar(trimws(station$art_description)) > 0) {
              station_id <- gsub("[^A-Za-z0-9]", "", station$stop_name)
              popup_html <- paste0(popup_html,
                "<div id='desc-short-", station_id, "'>",
                "<p style='color: #1e293b; font-size: 0.9rem; line-height: 1.4; margin: 0 0 10px 0;'>",
                ifelse(nchar(trimws(station$art_description)) > 120,
                       paste0(substr(trimws(station$art_description), 1, 120), "..."),
                       trimws(station$art_description)),
                "</p>",
                "<div style='display: flex; gap: 8px; margin-top: 10px;'>",
                ifelse(nchar(trimws(station$art_description)) > 120,
                       paste0("<button onclick=\"document.getElementById('desc-short-", station_id, 
                              "').style.display='none'; document.getElementById('desc-full-", station_id, 
                              "').style.display='block';\" style='background: #3b82f6; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; cursor: pointer; font-size: 0.8rem;'>READ MORE</button>"),
                       ""),
                ifelse(!is.na(station$art_image_link) && nchar(trimws(station$art_image_link)) > 0,
                       paste0("<a href='", trimws(station$art_image_link), "' target='_blank' style='background: #8b5cf6; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; text-decoration: none; cursor: pointer; font-family: \"Space Grotesk\", sans-serif; display: inline-block; font-size: 0.8rem;'>VIEW ON MTA</a>"),
                       ""),
                "</div>",
                "</div>"
              )
              
              if (nchar(trimws(station$art_description)) > 120) {
                popup_html <- paste0(popup_html,
                  "<div id='desc-full-", station_id, "' style='display: none;'>",
                  "<p style='color: #1e293b; font-size: 0.9rem; line-height: 1.4; margin: 0 0 10px 0;'>",
                  trimws(station$art_description), "</p>",
                  "<div style='display: flex; gap: 8px; margin-top: 10px;'>",
                  "<button onclick=\"document.getElementById('desc-full-", station_id, 
                  "').style.display='none'; document.getElementById('desc-short-", station_id, 
                  "').style.display='block';\" style='background: #64748b; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; cursor: pointer; font-size: 0.8rem;'>READ LESS</button>",
                  ifelse(!is.na(station$art_image_link) && nchar(trimws(station$art_image_link)) > 0,
                         paste0("<a href='", trimws(station$art_image_link), "' target='_blank' style='background: #8b5cf6; color: #ffffff; border: none; padding: 3px 8px; font-weight: 600; text-transform: uppercase; text-decoration: none; cursor: pointer; font-family: \"Space Grotesk\", sans-serif; display: inline-block; font-size: 0.8rem;'>VIEW ON MTA</a>"),
                         ""),
                  "</div>",
                  "</div>"
                )
              }
            }
            
            popup_html <- paste0(popup_html, "</div></div>")
            
            leafletProxy("subway_map") %>%
              addCircleMarkers(
                data = station,
                radius = 6,
                color = "#3b82f6",
                fillColor = "#3b82f6",
                fillOpacity = 0.9,
                stroke = TRUE,
                weight = 3,
                popup = popup_html
              )
          }
        }
      }
    }, error = function(e) {
      print(paste("Error in map update:", e$message))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
