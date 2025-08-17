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
  'iso_country_code', 'sovereignt', 'adm0_a3', 'event_type', 'n', 'event_name', 'classification', 'event_period', 'publication_year', 'link', 'summary_statement', 'text_color'
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

# Pre-define consistent color palette for event types
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

message("Data preprocessing complete.")

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%} .leaflet-container { background: #e5e8ea !important; }"),
  # Outfit font and custom CSS for layout
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Outfit:wght@400;700&display=swap"),
    tags$style(HTML("
      body, .panel, .btn, .navbar, .card, .dataTable, .tab-content, .shiny-output-error, .shiny-input-container, .form-control {
        font-family: 'Outfit', sans-serif !important;
      }
      .outer {
        position: fixed;
        top: 41px;
        left: 0;
        right: 0;
        bottom: 0;
        overflow: hidden;
        padding: 0;
      }
      #controls {
        background-color: white;
        padding: 20px;
        cursor: move;
        zoom: 1.2;
        border-radius: 16px;
        transition: opacity 0.25s ease-in-out;
        max-height: calc(100vh - 80px);
        overflow: hidden;
      }
      #controls.panel {
        opacity: 0.9;
      }
      #controls.panel:hover {
        opacity: 1;
      }
      #study_cards {
        max-height: 450px;
        overflow-y: auto;
        padding-bottom: 10px;
        scroll-padding-bottom: 10px;
        display: flex;
        flex-direction: column;
      }
      .study-card {
        font-family: 'Outfit', sans-serif !important;
        flex-shrink: 0;
      }
      .study-card:last-child {
        margin-bottom: 10px !important;
      }
      .dataTable {
        font-family: 'Outfit', sans-serif !important;
        font-size: 13px;
      }
      .leaflet-popup-content, .leaflet-popup-tip {
        font-family: 'Outfit', sans-serif !important;
      }
      .leaflet-popup-content-wrapper {
        box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
      }
      .leaflet-popup-content {
        margin: 0 !important;
        padding: 0 !important;
        overflow: visible !important;
      }
    "))
  ),
  useShinyjs(),
  navbarPage(
    title = div(
      tags$a(
        "Extreme Weather Attribution Studies",
        href = "#",
        id = "home-title",
        style = "color: #2c3e50; text-decoration: none; font-weight: bold; font-size: 22px; letter-spacing: 0.5px;",
        onclick = "Shiny.setInputValue('go_home', true, {priority: 'event'}); return false;"
      )
    ),
    id = "nav",
    tabPanel("Interactive map",
      div(class="outer",
        # Leaflet map
        leafletOutput("world_map", width="100%", height="100%"),
        # Control panel with circle pack and studies
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
          width = 400, height = "auto",
          # Left/right toggle button
          div(style = "display: flex; justify-content: flex-end; margin-bottom: 8px;",
            actionButton("toggle_panel_pos", "⇄", class = "btn btn-xs", style = "font-size: 12px; border-radius: 8px; padding: 2px 10px; background: #e9ecef; color: #2c3e50; border: 1px solid #dee2e6;")
          ),
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
              style = "height: 120px; display: flex; align-items: center; justify-content: center; background-color: #f8f9fa; border: 2px dashed #dee2e6; border-radius: 8px;",
              p("Study details will appear here", style = "color: #6c757d; text-align: center; font-size: 16px; margin: 0;")
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
        div(style = "max-width: 700px; margin: 40px auto; background: #f8f9fa; border-radius: 12px; padding: 32px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          h3("About this app", style = "font-weight: bold; color: #2c3e50; margin-bottom: 18px;"),
          p("This app visualizes extreme weather attribution studies from around the world. The dataset compiles published research on how climate change has influenced the intensity, frequency, or impact of extreme events such as heatwaves, floods, droughts, storms, and wildfires. Users can explore the studies interactively by country, event type, and year, and view details and summaries for each study.", style = "font-size: 16px; color: #495057; margin-bottom: 18px;"),
          p("Attribution science is crucial for understanding how human-caused climate change is affecting our world. By quantifying the influence of climate change on specific extreme events, these studies help inform adaptation, resilience, and policy decisions, and provide evidence for legal and societal action. As the field evolves, rapid and impact-focused attribution is becoming increasingly important for communicating risks and supporting communities facing climate-related challenges.", style = "font-size: 16px; color: #495057; margin-bottom: 18px;"),
          p("The data is sourced from Carbon Brief's interactive map and articles:", style = "font-size: 15px; color: #495057; margin-bottom: 8px;"),
          tags$ul(
            tags$li(tags$a(href = "https://interactive.carbonbrief.org/attribution-studies/index.html", target = "_blank", "Carbon Brief: Mapped - How climate change affects extreme weather around the world")),
            tags$li(tags$a(href = "https://www.carbonbrief.org/qa-the-evolving-science-of-extreme-weather-attribution/", target = "_blank", "Q&A: The evolving science of extreme weather attribution"))
          ),
          p("App created by Georgios Karamanis.", style = "font-size: 15px; color: #495057; margin-top: 18px;"),
          p("Data last updated: November 2024.", style = "font-size: 13px; color: #6c757d; margin-top: 8px;")
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
  
  # Panel position toggle
  panel_pos <- reactiveVal("right")

  observeEvent(input$toggle_panel_pos, {
    if (panel_pos() == "right") {
      panel_pos("left")
      runjs('$("#controls").css({top:60, left:20, right:"auto"});')
    } else {
      panel_pos("right")
      runjs('$("#controls").css({top:60, right:20, left:"auto"});')
    }
  })

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

      # Estimate collision: move panel if popup is on same side
      if (!is.null(click$lng)) {
        if (click$lng > 0 && panel_pos() == "right") {
          panel_pos("left")
          runjs('$("#controls").css({top:60, left:20, right:"auto"});')
        } else if (click$lng < 0 && panel_pos() == "left") {
          panel_pos("right")
          runjs('$("#controls").css({top:60, right:20, left:"auto"});')
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
    
    # Create filter buttons
    filter_buttons <- map(available_events, function(event) {
      event_color <- event_colors[event]
      if (is.na(event_color)) event_color <- "#95a5a6"
      
      is_selected <- !is.null(selected_event_type()) && selected_event_type() == event
      
      tags$button(
        event,
        class = "btn btn-sm",
        style = paste0(
          "margin: 3px; padding: 3px 8px; border: none; border-radius: 12px; font-size: 9px; font-weight: bold; cursor: pointer;",
          if (is_selected) {
            paste0("background-color: ", event_color, "; color: white; box-shadow: 0 0 0 2px ", event_color, "40;")
          } else {
            paste0("background-color: ", event_color, "20; color: ", event_color, "; border: 1px solid ", event_color, ";")
          }
        ),
        onclick = paste0("Shiny.setInputValue('event_filter_clicked', '", event, "', {priority: 'event'});")
      )
    })
    
    # Add "All" button
    all_button <- tags$button(
      "All studies",
      class = "btn btn-sm",
      style = paste0(
          "margin: 3px; padding: 4px 8px; border: none; border-radius: 12px; font-size: 9px; font-weight: bold; cursor: pointer;",
        if (is.null(selected_event_type())) {
          "background-color: black; color: white; box-shadow: 0 0 0 2px #2c3e5040;"
        } else {
          "background-color: #2c3e5020; color: #2c3e50; border: 1px solid #2c3e50;"
        }
      ),
      onclick = "Shiny.setInputValue('event_filter_clicked', 'All', {priority: 'event'});"
    )
    
    div(
      style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 8px;",
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
        style = "text-align: center; padding: 15px; color: #6c757d;",
          h6(
            if (!is.null(event_type_filter)) {
              paste("No", event_type_filter, "studies found for this country")
            } else {
              "No attribution studies found for this country"
            },
            style = "margin: 0; font-size: 11px;"
          )
        ))
    }
    
    # Create cards for each study
    study_cards <- map(seq_len(nrow(country_studies)), function(i) {
      study <- country_studies[i, ]
      
      # Use consistent event colors
      event_color <- event_colors[study$event_type]
      if (is.na(event_color)) event_color <- "#95a5a6"
      
      # Create event type badge 
      event_badge <- tags$span(
        study$event_type,
        style = paste0(
          "background-color: ", event_color,
          "; color: white; padding: 2px 6px; border-radius: 10px; font-size: 9px; font-weight: bold;"
        )
      )
      
      # Create classification badge
      class_color <- case_when(
        grepl("Less", study$classification) ~ "#27ae60",
        grepl("More", study$classification) ~ "#e67e22", 
        TRUE ~ "#95a5a6"
      )
      
      class_badge <- tags$span(
        study$classification,
        style = paste0(
          "background-color: ", class_color,
          "; color: white; padding: 2px 6px; border-radius: 10px; font-size: 8px; font-weight: bold; margin-left: 4px;"
        )
      )
      
      # Truncate summary
      summary_text <- if (nchar(study$summary_statement) > 150) {
        paste0(substr(study$summary_statement, 1, 150), "...")
      } else {
        study$summary_statement
      }
      
      # Create study card
      div(
        class = "study-card",
        style = "background: white; border: 1px solid #e9ecef; border-radius: 6px; padding: 10px; margin-bottom: 10px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
        
        # Header with event name and badges
        div(
          style = "margin-bottom: 8px;",
          h6(study$event_name, style = "margin: 0 0 6px 0; color: #2c3e50; font-size: 11px; font-weight: bold; line-height: 1.2;"),
          div(
            event_badge,
            class_badge,
            style = "margin-bottom: 6px;"
          ),
          div(
            tags$span("Event period: ", style = "color: #6c757d; font-size: 9px; font-weight: 500;"),
            tags$span(study$event_period, style = "color: #495057; font-size: 9px; margin-right: 8px;"),
            tags$span("Publication year: ", style = "color: #6c757d; font-size: 9px; font-weight: 500; margin-left: 8px;"),
            tags$span(study$publication_year, style = "color: #495057; font-size: 9px;"),
            style = "display: flex; gap: 4px; align-items: center; margin-bottom: 2px;"
          )
        ),
        
        # Summary
        p(
          summary_text,
          style = "font-size: 10px; line-height: 1.3; color: #495057; margin: 8px 0;"
        ),
        
        # Link
        if (!is.na(study$link)) {
          tags$a(
            "Read Study →",
            href = study$link,
            target = "_blank",
            style = "color: #007bff; text-decoration: none; font-size: 9px; font-weight: bold;"
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
  
  # Unified country header box
  output$country_header_box <- renderUI({
    country_data <- country_header_data()
    if (is.null(country_data)) return(NULL)
    
    studies_label <- if (country_data$n_studies == 1) "study" else "studies"
    
    div(
      style = "background: #f8f9fa; border-radius: 8px; padding: 8px 8px 4px 8px; margin-bottom: 10px; text-align: center;",
      div(
        style = "font-size: 20px; font-weight: bold; color: #2c3e50; margin-bottom: 2px;",
        country_data$name
      ),
      div(
        style = "font-size: 15px; color: #495057; font-weight: 500;",
        paste0(country_data$n_studies, ' ', studies_label)
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)