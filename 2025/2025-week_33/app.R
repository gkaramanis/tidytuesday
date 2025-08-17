library(shiny)
library(leaflet)
library(leafpop)
library(DT)
library(sf)
library(shinyjs)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggcirclepack)

utils::globalVariables(c(
  'iso_country_code', 'sovereignt', 'adm0_a3', 'event_type', 'n', 'event_name', 
  'classification', 'event_period', 'publication_year', 'link', 'summary_statement', 
  'text_color'
))

# ==============================================================================
# DATA LOADING AND PREPROCESSING (Outside server for performance)
# ==============================================================================

# Load and prepare data once at startup
message("Loading attribution studies data...")
attribution_studies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-12/attribution_studies.csv')

# Create long-format version once for all downstream use
studies_long <- attribution_studies |> 
  separate_longer_delim(iso_country_code, delim = ",") |>
  # Pre-filter any invalid data
  filter(!is.na(iso_country_code), iso_country_code != "")

# Count studies per country for map join
as_n <- studies_long |> count(iso_country_code, sort = TRUE)

# Load world map data
world <- read_sf("/Users/georgios/Documents/R/30daymapchallenge/2022/data/world.geo.json") |> 
  select(sovereignt, adm0_a3) |> 
  left_join(as_n, by = c("adm0_a3" = "iso_country_code"))

# Define constants
f1 <- "Outfit"

# Pre-define consistent color palette for event types (keeping original for map consistency)
event_colors <- c(
  "Heat" = "#D7263D",
  "Rain & flooding" = "#1B998B",
  "Drought" = "#F46036",
  "Storm" = "#2E294E",
  "Cold, snow & ice" = "#00A6FB",
  "Oceans" = "#FFB400",
  "Compound" = "#6A4C93",
  "Wildfire" = "#FF6F59",
  "Impact" = "#3A86FF",
  "River flow" = "#43AA8B",
  "Atmosphere" = "#FF006E",
  "Sunshine" = "#FFD166"
)

# Pre-compute dark events for text color optimization
dark_events <- c("Heat", "Storm", "Compound", "Impact", "Atmosphere")

# Modern sophisticated color palette for UI elements
ui_colors <- list(
  primary = "#1a365d",        # Deep navy blue
  secondary = "#2d3748",      # Charcoal gray
  accent = "#3182ce",         # Professional blue
  surface = "#ffffff",        # Pure white
  surface_alt = "#f7fafc",    # Very light gray
  surface_elevated = "#edf2f7", # Light gray
  text_primary = "#1a202c",   # Near black
  text_secondary = "#4a5568", # Medium gray
  text_muted = "#718096",     # Light gray
  border = "#e2e8f0",         # Very light border
  shadow = "rgba(0, 0, 0, 0.1)", # Subtle shadow
  success = "#38a169",        # Green
  warning = "#d69e2e",        # Amber
  error = "#e53e3e"          # Red
)

message("Data preprocessing complete.")

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%} .leaflet-container { background: #e5e8ea !important; }"),
  # Enhanced Outfit font with multiple weights and modern CSS
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800&display=swap"),
    tags$style(HTML(paste0("
      /* ==== GLOBAL STYLES ==== */
      :root {
        --primary: ", ui_colors$primary, ";
        --secondary: ", ui_colors$secondary, ";
        --accent: ", ui_colors$accent, ";
        --surface: ", ui_colors$surface, ";
        --surface-alt: ", ui_colors$surface_alt, ";
        --surface-elevated: ", ui_colors$surface_elevated, ";
        --text-primary: ", ui_colors$text_primary, ";
        --text-secondary: ", ui_colors$text_secondary, ";
        --text-muted: ", ui_colors$text_muted, ";
        --border: ", ui_colors$border, ";
        --shadow: ", ui_colors$shadow, ";
        --success: ", ui_colors$success, ";
        --warning: ", ui_colors$warning, ";
        --error: ", ui_colors$error, ";
      }
      
      * {
        box-sizing: border-box;
      }
      
      body, .panel, .btn, .navbar, .card, .dataTable, .tab-content, 
      .shiny-output-error, .shiny-input-container, .form-control {
        font-family: 'Outfit', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
      }
      
      /* ==== LAYOUT ==== */
      .outer {
        position: fixed;
        top: 60px;
        left: 0;
        right: 0;
        bottom: 0;
        overflow: hidden;
        padding: 0;
        background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
      }
      
      /* ==== NAVBAR ENHANCEMENTS ==== */
      .navbar {
        background: rgba(255, 255, 255, 0.95) !important;
        backdrop-filter: blur(20px);
        border-bottom: 1px solid var(--border);
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
        padding: 12px 0;
        min-height: 60px;
      }
      
      .navbar-brand {
        font-weight: 700 !important;
        font-size: 24px !important;
        letter-spacing: -0.5px !important;
        color: var(--primary) !important;
        transition: all 0.3s ease;
      }
      
      .navbar-brand:hover {
        color: var(--accent) !important;
        transform: translateY(-1px);
      }
      
      .navbar-nav .nav-link {
        font-weight: 500 !important;
        font-size: 15px !important;
        color: var(--text-secondary) !important;
        padding: 8px 16px !important;
        border-radius: 8px;
        transition: all 0.3s ease;
        margin: 0 4px;
      }
      
      .navbar-nav .nav-link:hover,
      .navbar-nav .nav-link.active {
        background: var(--surface-elevated) !important;
        color: var(--primary) !important;
        transform: translateY(-1px);
      }
      
      /* ==== CONTROL PANEL WITH GLASS MORPHISM ==== */
      #controls {
        background: rgba(255, 255, 255, 0.85);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border: 1px solid rgba(255, 255, 255, 0.2);
        padding: 24px;
        cursor: move;
        border-radius: 20px;
        max-height: calc(100vh - 100px);
        overflow: hidden;
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1), 0 6px 16px rgba(0, 0, 0, 0.08);
      }
      
      #controls:hover {
        background: rgba(255, 255, 255, 0.95);
      }
      
      /* ==== STUDY CARDS CONTAINER ==== */
      #study_cards {
        max-height: 480px;
        overflow-y: auto;
        padding-bottom: 16px;
        scroll-padding-bottom: 16px;
        display: flex;
        flex-direction: column;
        gap: 12px;
      }
      
      #study_cards::-webkit-scrollbar {
        width: 6px;
      }
      
      #study_cards::-webkit-scrollbar-track {
        background: var(--surface-alt);
        border-radius: 3px;
      }
      
      #study_cards::-webkit-scrollbar-thumb {
        background: var(--border);
        border-radius: 3px;
        transition: background 0.3s ease;
      }
      
      #study_cards::-webkit-scrollbar-thumb:hover {
        background: var(--text-muted);
      }
      
      /* ==== STUDY CARDS ==== */
      .study-card {
        background: var(--surface);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 16px;
        margin: 0;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04), 0 1px 3px rgba(0, 0, 0, 0.06);
        transition: all 0.3s ease;
        flex-shrink: 0;
        position: relative;
        overflow: hidden;
      }
      
      .study-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        background: linear-gradient(90deg, var(--accent), var(--primary));
        opacity: 0;
        transition: opacity 0.3s ease;
      }
      
      .study-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 25px rgba(0, 0, 0, 0.08), 0 3px 10px rgba(0, 0, 0, 0.06);
        border-color: var(--accent);
      }
      
      .study-card:hover::before {
        opacity: 1;
      }
      
      /* ==== ENHANCED TYPOGRAPHY ==== */
      .study-card h6 {
        font-size: 14px !important;
        font-weight: 600 !important;
        line-height: 1.4 !important;
        color: var(--text-primary) !important;
        margin: 0 0 8px 0 !important;
        letter-spacing: -0.2px;
      }
      
      .study-card p {
        font-size: 12px !important;
        line-height: 1.5 !important;
        color: var(--text-secondary) !important;
        margin: 12px 0 !important;
      }
      
      .study-card a {
        font-size: 11px !important;
        font-weight: 600 !important;
        color: var(--accent) !important;
        text-decoration: none !important;
        transition: all 0.3s ease;
        position: relative;
      }
      
      .study-card a:hover {
        color: var(--primary) !important;
        transform: translateX(2px);
      }
      
      /* ==== ENHANCED BADGES ==== */
      .badge-enhanced {
        padding: 4px 8px !important;
        border-radius: 6px !important;
        font-size: 10px !important;
        font-weight: 600 !important;
        letter-spacing: 0.3px;
        text-transform: uppercase;
        margin-right: 6px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.12);
        transition: all 0.3s ease;
      }
      
      /* ==== BUTTONS ==== */
      .btn {
        border-radius: 8px !important;
        font-weight: 500 !important;
        transition: all 0.3s ease !important;
        border: none !important;
        position: relative;
        overflow: hidden;
      }
      
      .btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
      }
      
      .btn:active {
        transform: translateY(0);
      }
      
      /* ==== TOGGLE BUTTON ==== */
      #toggle_panel_pos {
        background: var(--surface) !important;
        color: var(--text-secondary) !important;
        border: 1px solid var(--border) !important;
        font-size: 14px !important;
        padding: 6px 12px !important;
        border-radius: 10px !important;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
      }
      
      #toggle_panel_pos:hover {
        background: var(--surface-elevated) !important;
        color: var(--primary) !important;
        border-color: var(--accent) !important;
      }
      
      /* ==== COUNTRY HEADER ==== */
      .country-header {
        background: linear-gradient(135deg, var(--surface-alt) 0%, var(--surface-elevated) 100%) !important;
        border-radius: 12px !important;
        padding: 16px !important;
        margin-bottom: 16px !important;
        text-align: center;
        border: 1px solid var(--border);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
      }
      
      .country-name {
        font-size: 22px !important;
        font-weight: 700 !important;
        color: var(--primary) !important;
        margin-bottom: 4px !important;
        letter-spacing: -0.3px;
      }
      
      .study-count {
        font-size: 16px !important;
        color: var(--text-secondary) !important;
        font-weight: 500 !important;
      }
      
      /* ==== FILTER SECTION ==== */
      .filter-section {
        background: var(--surface) !important;
        border: 1px solid var(--border) !important;
        border-radius: 12px !important;
        padding: 16px !important;
        margin-bottom: 20px !important;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.04);
      }
      
      /* ==== DATA TABLE ==== */
      .dataTable {
        font-size: 14px !important;
        border-collapse: separate !important;
        border-spacing: 0 !important;
      }
      
      .dataTable thead th {
        background: var(--surface-elevated) !important;
        color: var(--text-primary) !important;
        font-weight: 600 !important;
        border-bottom: 2px solid var(--border) !important;
        padding: 12px !important;
      }
      
      .dataTable tbody tr {
        transition: all 0.3s ease;
      }
      
      .dataTable tbody tr:hover {
        background: var(--surface-alt) !important;
      }
      
      .dataTable tbody td {
        padding: 12px !important;
        border-bottom: 1px solid var(--border) !important;
      }
      
      /* ==== LEAFLET POPUP ==== */
      .leaflet-popup-content, .leaflet-popup-tip {
        font-family: 'Outfit', sans-serif !important;
      }
      
      .leaflet-popup-content-wrapper {
        border-radius: 12px !important;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2) !important;
        border: 1px solid var(--border) !important;
      }
      
      .leaflet-popup-content {
        margin: 0 !important;
        padding: 0 !important;
        overflow: visible !important;
        border-radius: 12px;
      }
      
      /* ==== ABOUT PAGE ==== */
      .about-container {
        max-width: 800px !important;
        margin: 40px auto !important;
        background: var(--surface) !important;
        border-radius: 16px !important;
        padding: 40px !important;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08) !important;
        border: 1px solid var(--border);
      }
      
      .about-container h3 {
        font-weight: 700 !important;
        color: var(--primary) !important;
        margin-bottom: 24px !important;
        font-size: 28px !important;
        letter-spacing: -0.5px;
      }
      
      .about-container p {
        font-size: 16px !important;
        color: var(--text-secondary) !important;
        line-height: 1.6 !important;
        margin-bottom: 20px !important;
      }
      
      .about-container a {
        color: var(--accent) !important;
        text-decoration: none !important;
        font-weight: 500;
        transition: all 0.3s ease;
      }
      
      .about-container a:hover {
        color: var(--primary) !important;
        text-decoration: underline !important;
      }
      
      .about-container ul {
        margin: 16px 0 24px 0;
      }
      
      .about-container li {
        margin-bottom: 8px;
        color: var(--text-secondary);
      }
      
      /* ==== EMPTY STATE ==== */
      .empty-state {
        height: 140px !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        background: var(--surface-alt) !important;
        border: 2px dashed var(--border) !important;
        border-radius: 12px !important;
        transition: all 0.3s ease;
      }
      
      .empty-state:hover {
        border-color: var(--accent);
        background: var(--surface-elevated);
      }
      
      .empty-state p {
        color: var(--text-muted) !important;
        text-align: center !important;
        font-size: 16px !important;
        margin: 0 !important;
        font-weight: 500;
      }
      
      /* ==== LOADING STATES ==== */
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      
      .loading {
        animation: pulse 2s infinite;
      }
      
      /* ==== ACCESSIBILITY ==== */
      @media (prefers-reduced-motion: reduce) {
        *, *::before, *::after {
          animation-duration: 0.01ms !important;
          animation-iteration-count: 1 !important;
          transition-duration: 0.01ms !important;
        }
      }
      
      /* ==== RESPONSIVE DESIGN ==== */
      @media (max-width: 768px) {
        #controls {
          width: 95% !important;
          left: 2.5% !important;
          right: 2.5% !important;
          max-height: calc(100vh - 120px);
        }
        
        .outer {
          top: 55px;
        }
        
        .navbar-brand {
          font-size: 20px !important;
        }
      }
    ")))
  ),
  useShinyjs(),
  navbarPage(
    title = div(
      tags$a(
        "Extreme Weather Attribution Studies",
        href = "#",
        id = "home-title",
        style = "color: var(--primary); text-decoration: none; font-weight: 700; font-size: 24px; letter-spacing: -0.5px;",
        onclick = "Shiny.setInputValue('go_home', true, {priority: 'event'}); return false;"
      )
    ),
    id = "nav",
    tabPanel("Interactive map",
      div(class="outer",
        # Leaflet map
        div(style = "position: relative; width: 100%; height: 100%;",
          leafletOutput("world_map", width="100%", height="100%"),
          div(
            style = "position: absolute; left: 0; right: 0; bottom: 8px; z-index: 1000; text-align: center; pointer-events: none;",
            tags$div(
              style = "display: inline-block; background: rgba(255,255,255,0.85); border-radius: 10px; padding: 8px 18px; font-size: 13px; color: var(--text-muted); box-shadow: 0 2px 8px rgba(0,0,0,0.07); border: 1px solid var(--border); font-family: 'Outfit', sans-serif;",
              HTML('Data source: <a href="https://interactive.carbonbrief.org/attribution-studies/index.html" target="_blank" style="color: var(--accent); text-decoration: none;">Carbon Brief</a> &nbsp;|&nbsp; Application by Georgios Karamanis')
            )
          )
        ),
        # Control panel with modern glass morphism design
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
          width = 420, height = "auto",
          # Studies section
          conditionalPanel(
            condition = "output.country_selected",
            div(
              div(
                uiOutput("country_header_box"),
                uiOutput("event_type_filters"),
                uiOutput("study_cards")
              )
            )
          ),
          conditionalPanel(
            condition = "!output.country_selected",
            div(
              class = "empty-state",
              p("Study details will appear here")
            )
          )
        )
      )
    ),
    tabPanel("All studies table",
      fluidPage(
        DTOutput("studies_table")
      )
    ),
    tabPanel("About",
      fluidPage(
        div(class = "about-container",
          h3("About"),
          p("This application provides an interactive exploration of extreme weather attribution studies from around the world. The dataset compiles peer-reviewed research examining how anthropogenic climate change has influenced the intensity, frequency, or likelihood of specific extreme events including heatwaves, floods, droughts, storms, and wildfires."),
          p("Attribution science represents a critical frontier in climate research, employing sophisticated statistical methods and climate models to quantify the role of human-caused climate change in individual extreme events. These studies provide essential evidence for understanding climate risks, informing adaptation strategies, and supporting climate litigation and policy decisions."),
          p("The methodology typically involves comparing the probability or intensity of observed events in our current climate against counterfactual scenarios representing pre-industrial or natural climate conditions. This approach enables researchers to determine whether and to what extent climate change has made specific events more likely or severe."),
          div(
            h4("Data Sources", style = "font-size: 18px; font-weight: 600; color: var(--primary); margin: 24px 0 12px 0;"),
            p("This dataset is sourced from Carbon Brief's comprehensive tracking of attribution studies:", style = "margin-bottom: 8px;"),
            tags$ul(
              tags$li(tags$a(href = "https://interactive.carbonbrief.org/attribution-studies/index.html", target = "_blank", "Carbon Brief: Mapped - How climate change affects extreme weather around the world")),
              tags$li(tags$a(href = "https://www.carbonbrief.org/qa-the-evolving-science-of-extreme-weather-attribution/", target = "_blank", "Q&A: The evolving science of extreme weather attribution"))
            )
          ),
          div(
            p("Studies are classified based on their findings regarding climate change influence on the analyzed events. The visualization employs country-level aggregation with interactive filtering by event type and temporal parameters. Data quality and study methodology vary across included research."),
          ),
          div(style = "margin-top: 32px; padding-top: 20px; border-top: 1px solid var(--border);",
            p("Application developed by Georgios Karamanis", style = "font-size: 15px; color: var(--text-muted);"),
            p("Dataset last updated: November 2024", style = "font-size: 13px; color: var(--text-muted); margin-top: 8px;")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER FUNCTION
# ==============================================================================

server <- function(input, output, session) {
  # Title click: go to map tab
  observeEvent(input$go_home, {
    updateTabsetPanel(session, "nav", selected = "Interactive map")
  })
  
  # Reactive values for selections
  selected_country <- reactiveVal(NULL)
  selected_event_type <- reactiveVal(NULL)
  
  # Panel position (collision detection only)
  panel_pos <- reactiveVal("right")

  # Create leaflet map with caching
  output$world_map <- renderLeaflet({
    robin_crs <- leafletCRS(
      crsClass = "L.Proj.CRS", 
      code = "ESRI:54030",
      proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
      resolutions = 1.8^(25:15)
    )
    
    pal <- MetBrewer::met.brewer("Tam", n_distinct(as_n$iso_country_code), direction = -1)
    
    leaflet(
      data = world,
      options = leafletOptions(
        minZoom = 7,
        maxZoom = 10,
        crs = robin_crs
      )
    ) |>
      setView(lng = 0, lat = 0, zoom = 8) |>
      addPolygons(
        layerId = ~adm0_a3,
        fillColor = ~ifelse(is.na(n), "#f6f7f9", pal),
        fillOpacity = ~ifelse(is.na(n), 0.7, 0.95),
        color = "#e5e8ea",
        weight = 0.7,
        opacity = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = ~ifelse(is.na(n), 0.8, 0.98),
          bringToFront = TRUE
        )
      )
  })

  # Cached reactive for country data to avoid recomputation
  country_plot_data <- reactive({
    req(selected_country())
    
    studies_long |> 
      filter(iso_country_code == selected_country()) |> 
      count(event_type, sort = TRUE) |>
      mutate(text_color = ifelse(event_type %in% dark_events, "white", "#222"))
  }) |> bindCache(selected_country())

  # Show popup with circle pack plot for selected country
  observeEvent(input$world_map_shape_click, {
    click <- input$world_map_shape_click
    if (!is.null(click$id)) {
      selected_country(click$id)
      selected_event_type(NULL)

      # Intelligent collision avoidance: move panel if popup is on same side
      if (!is.null(click$lng)) {
        if (click$lng > 0 && panel_pos() == "right") {
          panel_pos("left")
          runjs('$("#controls").css({top: "80px", left: "20px", right: "auto"});')
        } else if (click$lng < 0 && panel_pos() == "left") {
          panel_pos("right")
          runjs('$("#controls").css({top: "80px", right: "20px", left: "auto"});')
        }
      }

      # Use cached country data for popup generation
      country_data <- country_plot_data()
      
      country_row <- world |> filter(adm0_a3 == click$id)
      n_studies <- ifelse(is.na(country_row$n), 0, country_row$n)

      # Popup: show only the plot (no country name, no study count)
      container_width <- 300
      graph_height <- if (n_studies > 0) 300 else 50
      plot_width <- container_width

      if (n_studies > 0) {
        plot_html <- as.character(popupGraph(
          ggplot(country_data, aes(
            id = event_type, 
            area = n, 
            fill = event_type, 
            label = paste0(event_type, "\n", n),
            size = n
          )) +
            geom_circlepack() +
            geom_circlepack_text(aes(colour = text_color), family = f1) +
            scale_fill_manual(values = event_colors, na.value = "#95a5a6") +
            scale_color_identity() +
            scale_size_area(max_size = 6) +
            coord_fixed(clip = "off") +
            theme_void(base_family = f1) +
            theme(
              legend.position = "none"
            ),
          width = plot_width,
          height = graph_height
        ))

        popup_content <- paste0(
          '<div style="width: ', container_width, 'px; min-width: ', container_width, 'px; box-sizing: border-box;">',
          gsub('<img ', paste0('<img style="width:100%;height:', graph_height, 'px;display:block;object-fit:contain;box-sizing:border-box;" '), plot_html),
          '</div>'
        )
      } else {
        popup_content <- paste0(
          '<div style="width: ', container_width, 'px; min-width: ', container_width, 'px; box-sizing: border-box; height:', graph_height, 'px; display:flex; align-items:center; justify-content:center; color:#6c757d; font-size:14px;">',
          'No attribution studies found for this country',
          '</div>'
        )
      }
      
      # Create popup with options that match container width exactly
      popup_options <- popupOptions(
        maxWidth = container_width,
        minWidth = container_width,
        autoPan = TRUE,
        keepInView = TRUE,
        closeButton = TRUE,
        autoClose = TRUE,
        closeOnEscapeKey = TRUE
      )
      
      leafletProxy("world_map") |> 
        clearPopups() |> 
        addPopups(
          lng = click$lng, 
          lat = click$lat, 
          popup = popup_content, 
          layerId = click$id,
          options = popup_options
        )
    }
  })
  
  # Output for conditional panel
  output$country_selected <- reactive({
    !is.null(selected_country())
  })
  outputOptions(output, "country_selected", suspendWhenHidden = FALSE)
  
  # Cached reactive for available events by country
  country_events <- reactive({
    req(selected_country())
    
    studies_long |> 
      filter(iso_country_code == selected_country()) |> 
      count(event_type, sort = TRUE) |> 
      pull(event_type)
  }) |> bindCache(selected_country())
  
  # Event type filter buttons
  output$event_type_filters <- renderUI({
    req(selected_country()) # Only render if a country is selected
    
    available_events <- country_events()
    
    if (length(available_events) == 0) return(div())
    
    # Create enhanced filter buttons with modern styling
    filter_buttons <- map(available_events, function(event) {
      event_color <- event_colors[event]
      if (is.na(event_color)) event_color <- "#95a5a6"
      
      is_selected <- !is.null(selected_event_type()) && selected_event_type() == event
      
      tags$button(
        event,
        class = "btn btn-sm",
        style = paste0(
          "margin: 4px; padding: 6px 12px; border: none; border-radius: 16px; font-size: 10px; font-weight: 600; cursor: pointer; transition: all 0.3s ease; letter-spacing: 0.3px;",
          if (is_selected) {
            paste0("background-color: ", event_color, "; color: white; box-shadow: 0 2px 8px ", event_color, "40, 0 0 0 2px ", event_color, "20; transform: translateY(-1px);")
          } else {
            paste0("background-color: ", event_color, "15; color: ", event_color, "; border: 1px solid ", event_color, "30; box-shadow: 0 1px 3px rgba(0,0,0,0.1);")
          }
        ),
        onclick = paste0("Shiny.setInputValue('event_filter_clicked', '", event, "', {priority: 'event'});")
      )
    })
    
    # Enhanced "All" button
    all_button <- tags$button(
      "All Studies",
      class = "btn btn-sm",
      style = paste0(
        "margin: 4px; padding: 6px 12px; border: none; border-radius: 16px; font-size: 10px; font-weight: 600; cursor: pointer; transition: all 0.3s ease; letter-spacing: 0.3px;",
        if (is.null(selected_event_type())) {
          "background: linear-gradient(135deg, var(--primary), var(--accent)); color: white; box-shadow: 0 2px 8px rgba(0,0,0,0.15), 0 0 0 2px rgba(0,0,0,0.1); transform: translateY(-1px);"
        } else {
          "background-color: var(--surface-elevated); color: var(--text-secondary); border: 1px solid var(--border); box-shadow: 0 1px 3px rgba(0,0,0,0.1);"
        }
      ),
      onclick = "Shiny.setInputValue('event_filter_clicked', 'All', {priority: 'event'});"
    )
    
    div(
      class = "filter-section",
      all_button,
      filter_buttons
    )
  })
  
  # Handle filter button clicks
  observeEvent(input$event_filter_clicked, {
    if (input$event_filter_clicked == "All") {
      selected_event_type(NULL)
    } else {
      selected_event_type(input$event_filter_clicked)
    }
  })
  
  # Study cards with filtering
  output$study_cards <- renderUI({
    req(selected_country()) # Only render if a country is selected
    
    country_filter <- selected_country()
    event_type_filter <- selected_event_type()
    
  # Get all attribution studies for selected country
  country_studies <- studies_long |> 
    filter(iso_country_code == country_filter)
    
    # Filter by event type if selected
    if (!is.null(event_type_filter)) {
      country_studies <- country_studies |> filter(event_type == event_type_filter)
    }
    
    country_studies <- country_studies |> arrange(desc(publication_year))
    
    if (nrow(country_studies) == 0) {
      return(div(
        style = "text-align: center; padding: 20px; color: var(--text-muted);",
        h6(
          if (!is.null(event_type_filter)) {
            paste("No", event_type_filter, "attribution studies found for this country")
          } else {
            "No attribution studies found for this country"
          },
          style = "margin: 0; font-size: 14px; font-weight: 500;"
        )
      ))
    }
    
    # Create enhanced study cards with modern design
    study_cards <- map(seq_len(nrow(country_studies)), function(i) {
      study <- country_studies[i, ]
      
      # Use consistent event colors
      event_color <- event_colors[study$event_type]
      if (is.na(event_color)) event_color <- "#95a5a6"
      
      # Create enhanced event type badge 
      event_badge <- tags$span(
        study$event_type,
        class = "badge-enhanced",
        style = paste0(
          "background: linear-gradient(135deg, ", event_color, ", ", event_color, "E6); color: white;"
        )
      )
      
      # Create enhanced classification badge
      class_info <- case_when(
        grepl("Less", study$classification) ~ list(color = ui_colors$success, text = "Reduced Likelihood"),
        grepl("More", study$classification) ~ list(color = ui_colors$warning, text = "Increased Likelihood"), 
        TRUE ~ list(color = ui_colors$text_muted, text = "Inconclusive")
      )
      
      class_badge <- tags$span(
        if(nchar(study$classification) > 20) paste0(substr(study$classification, 1, 18), "...") else study$classification,
        class = "badge-enhanced",
        style = paste0(
          "background: linear-gradient(135deg, ", class_info$color, ", ", class_info$color, "E6); color: white; margin-left: 6px;"
        )
      )
      
      # Enhanced summary with better truncation
      summary_text <- if (nchar(study$summary_statement) > 180) {
        paste0(substr(study$summary_statement, 1, 180), "...")
      } else {
        study$summary_statement
      }
      
      # Create enhanced study card
      div(
        class = "study-card",
        
        # Enhanced header with event name and badges
        div(
          style = "margin-bottom: 12px;",
          h6(study$event_name, style = "margin: 0 0 8px 0; color: var(--text-primary); font-size: 14px; font-weight: 600; line-height: 1.4; letter-spacing: -0.2px;"),
          div(
            event_badge,
            class_badge,
            style = "margin-bottom: 8px;"
          ),
          div(
            style = "display: flex; flex-wrap: wrap; gap: 8px; align-items: center; font-size: 11px; color: var(--text-muted);",
            tags$span(
              tags$strong("Event Period: ", style = "font-weight: 600;"),
              study$event_period
            ),
            tags$span("•", style = "color: var(--border);"),
            tags$span(
              tags$strong("Published: ", style = "font-weight: 600;"),
              study$publication_year
            )
          )
        ),
        
        # Enhanced summary
        p(
          summary_text,
          style = "font-size: 12px; line-height: 1.5; color: var(--text-secondary); margin: 12px 0; text-align: justify;"
        ),
        
        # Enhanced link with better styling
        if (!is.na(study$link)) {
          div(
            style = "margin-top: 12px; padding-top: 12px; border-top: 1px solid var(--border);",
            tags$a(
              "Read Full Study →",
              href = study$link,
              target = "_blank",
              style = "color: var(--accent); text-decoration: none; font-size: 11px; font-weight: 600; display: inline-flex; align-items: center; transition: all 0.3s ease;"
            )
          )
        }
      )
    })
    
    return(study_cards)
    
  })
  
  # Cached studies table for better performance
  studies_table_data <- reactive({
    attribution_studies |> 
      arrange(desc(publication_year)) |> 
      mutate(
        # Make link clickable
        link = ifelse(!is.na(link), paste0('<a href="', link, '" target="_blank">Read Study</a>'), NA)
      ) |> 
      select(
        Event = event_name,
        Type = event_type,
        Classification = classification,
        Period = event_period,
        Year = publication_year,
        Country = iso_country_code,
        Summary = summary_statement,
        Link = link
      )
  }) |> bindCache("studies_table")  # Cache this expensive operation

  output$studies_table <- renderDT({
    studies_tbl <- studies_table_data()

    datatable(
      studies_tbl,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        columnDefs = list(
          list(width = '300px', targets = c("Event", "Summary")),
          list(width = '60px', targets = c("Period", "Year"))
        )
      ),
      colnames = c('Event', 'Type', 'Classification', 'Period', 'Year', 'Country', 'Summary', "")
    )
  })
  
  # Cached country header to avoid repeated lookups
  country_header_data <- reactive({
    req(selected_country())
    country_id <- selected_country()
    country_row <- world |> filter(adm0_a3 == country_id)
    
    list(
      name = if (nrow(country_row) > 0) country_row$sovereignt else country_id,
      n_studies = if (nrow(country_row) > 0 && !is.na(country_row$n)) country_row$n else 0
    )
  }) |> bindCache(selected_country())
  
  # Enhanced country header with modern design
  output$country_header_box <- renderUI({
    country_data <- country_header_data()
    if (is.null(country_data)) return(NULL)
    
    studies_label <- if (country_data$n_studies == 1) "attribution study" else "attribution studies"
    
    div(
      class = "country-header",
      div(
        class = "country-name",
        country_data$name
      ),
      div(
        class = "study-count",
        paste0(format(country_data$n_studies, big.mark = " "), " ", studies_label)
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)