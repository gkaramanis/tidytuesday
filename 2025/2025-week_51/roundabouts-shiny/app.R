library(shiny)
library(leaflet)
library(reactable)
library(dplyr)

roundabouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv') |> 
  rename(long = lat, lat = long) |>
  group_by(town_city, county_area, state_region, country) |>
  mutate(city_id = cur_group_id()) |>
  ungroup()

cities <- roundabouts |> 
  count(city_id, town_city, county_area, state_region, country, name = "n_roundabouts") |>
  mutate(
    location_detail = paste(
      coalesce(county_area, ""),
      coalesce(state_region, ""),
      coalesce(country, ""),
      sep = ", "
    ) |> stringr::str_remove_all("^, |, , |, $")
  )

ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Manrope:wght@400;600&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      * { font-family: 'Inter', sans-serif !important; }
      .reactable .rt-tr-selected { background-color: #e6f2ff !important; }
      .leaflet-popup-content { font-size: 16px; font-weight: 600; }
      .reactable input[type='radio'] { accent-color: purple !important; }
      .page-header { border: 3px solid #333; border-radius: 8px; padding: 15px; margin-bottom: 20px; }
      body { margin: 0; background-color: #2a2a2a; }
    "))
  ),
  div(style = "background-color: #2a2a2a; min-height: 100vh; padding: 20px;",
    div(style = "display: grid; grid-template-columns: 1.4fr 1fr; gap: 20px; align-items: start;",
      div(
        div(style = "border: 3px solid #333; border-radius: 8px; padding: 15px; margin-bottom: 10px; display: flex; align-items: center; justify-content: space-between; background-color: #f5f5f5;",
          div(
            h3(style = "margin: 0 0 10px 0; font-size: 28px;", "Roundabouts"),
            helpText(style = "margin: 0 0 8px 0; font-size: 16px;", "Select a city in the table to zoom to it on the map")
          ),
          actionButton("reset_view", "Reset View", style = "border: 2px solid #333; padding: 8px 16px; border-radius: 6px; font-weight: 600; margin-left: 10px;")
        ),
        div(style = "border: 3px solid #333; border-radius: 8px; overflow: hidden; background-color: #f5f5f5;", leafletOutput("city_map", height = 600)),
        div(style = "border: 3px solid #333; border-radius: 8px; padding: 15px; margin-top: 10px; background-color: #f5f5f5;",
          p(style = "margin: 0; font-size: 14px; color: #666;", 
            "Data from the ", 
            tags$a(href = "https://github.com/EmilHvitfeldt/roundabouts", "roundabouts R package", target = "_blank"),
            " by Emil Hvitfeldt",
            tags$br(),
            "Original data compiled by ",
            tags$a(href = "https://roundabouts.kittelson.com/", "Kittelson & Associates", target = "_blank"),
            tags$br(),
            "Design by Georgios Karamanis"
          )
        )
      ),
      div(style = "border: 3px solid #333; border-radius: 8px; padding: 10px; background-color: #f5f5f5; display: flex; flex-direction: column;", reactableOutput("city_table"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Default map view
  default_view <- list(lng = 9, lat = 20, zoom = 2.3)
  
  # Render table
  output$city_table <- renderReactable({
    reactable(
      cities,
      selection = "single",
      onClick = "select",
      searchable = TRUE,
      defaultPageSize = 12,
      defaultSorted = list("n_roundabouts" = "desc"),
      columns = list(
        city_id = colDef(show = FALSE),
        town_city = colDef(
          name = "Location",
          width = 350,
          html = TRUE,
          cell = function(value, index) {
            location <- cities$location_detail[index]
            sprintf('<div style="font-weight: 600; font-size: 16px;">%s</div><div style="font-size: 14px; color: #666;">%s</div>', value, location)
          }
        ),
        county_area = colDef(show = FALSE, searchable = TRUE),
        state_region = colDef(show = FALSE, searchable = TRUE),
        country = colDef(show = FALSE, searchable = TRUE),
        location_detail = colDef(show = FALSE),
        n_roundabouts = colDef(name = "Roundabouts")
      ),
      theme = reactableTheme(
        style = list(fontFamily = "'Inter', sans-serif"),
        headerStyle = list(fontWeight = 600)
      )
    )
  })
  
  # Filtered cities based on table search/filter
  filtered_cities <- reactive({
    state <- getReactableState("city_table")
    if (is.null(state)) {
      cities
    } else {
      # Get all filtered data
      sorted_data <- state$sortedData
      if (!is.null(sorted_data) && length(sorted_data) > 0) {
        cities[sorted_data, ]
      } else {
        cities
      }
    }
  })
  
  # All roundabouts in filtered cities
  filtered_points <- reactive({
    filtered <- filtered_cities()
    roundabouts |> filter(city_id %in% filtered$city_id)
  })
  
  # Track previous selection to detect changes
  prev_selection <- reactiveVal(NULL)
  
  # Render map
  output$city_map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles("CartoDB.Positron", group = "Light", options = providerTileOptions(minZoom = 1.5, maxZoom = 12)) |>
      addProviderTiles("OpenStreetMap.Mapnik", group = "Street") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addLayersControl(
        baseGroups = c("Light", "Street", "Satellite"),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) |>
      addMiniMap(toggleDisplay = TRUE, minimized = TRUE) |>
      setView(lng = default_view$lng, lat = default_view$lat, zoom = default_view$zoom)
  })
  
  # Calculate radius based on zoom level
  marker_radius <- reactive({
    zoom <- input$city_map_zoom
    if (is.null(zoom)) {
      # Default zoom is 2.3, base radius should be 1 at zoom 2.3
      base_radius <- 1
    } else {
      base_radius <- (zoom - 2) * 0.5
    }
    
    # Apply selection multiplier if city is selected
    state <- getReactableState("city_table")
    if (!is.null(state$selected) && length(state$selected) > 0) {
      base_radius * 1.7
    } else {
      base_radius
    }
  })
  
  # Update map when selection changes or zoom level changes
  observe({
    state <- getReactableState("city_table")
    current_selection <- if (!is.null(state$selected)) state$selected else NA
    prev_sel <- prev_selection()
    
    # Update whenever selection changes or zoom changes (via marker_radius reactive)
    if (!identical(current_selection, prev_sel) || !is.null(input$city_map_zoom)) {
      prev_selection(current_selection)
      
      points <- filtered_points()
      point_radius <- marker_radius()
      
      # Purple for selected city, orange otherwise
      if (!is.null(state$selected) && length(state$selected) > 0) {
        selected_city_id <- cities[state$selected, ]$city_id
        points <- points |> 
          mutate(color = ifelse(city_id == selected_city_id, "purple", "orange"))
      } else {
        points <- points |> 
          mutate(color = "orange")
      }
      
      leafletProxy("city_map") |> 
        clearMarkers() |> 
        addCircleMarkers(
          data = points,
          lng = ~long,
          lat = ~lat,
          radius = point_radius,
          fillColor = ~color,
          fillOpacity = 0.7,
          stroke = FALSE,
          popup = ~paste0("<strong>", name, "</strong><br>", town_city)
        )
    }
  })
  
  # Zoom to selected city
  observe({
    state <- getReactableState("city_table")
    if (!is.null(state)) {
      selected <- state$selected
      if (!is.null(selected) && length(selected) == 1) {
        city <- cities[selected, ]
        city_points <- roundabouts |> filter(city_id == city$city_id)
        if (nrow(city_points) > 0) {
          # Use fitBounds to show all roundabouts in the city
          leafletProxy("city_map") |> 
            fitBounds(
              lng1 = min(city_points$long, na.rm = TRUE),
              lat1 = min(city_points$lat, na.rm = TRUE),
              lng2 = max(city_points$long, na.rm = TRUE),
              lat2 = max(city_points$lat, na.rm = TRUE)
            )
        }
      } else if (is.null(selected) || length(selected) == 0) {
        # Reset to world view when deselected
        leafletProxy("city_map") |> 
          setView(lng = default_view$lng, lat = default_view$lat, zoom = default_view$zoom)
      }
    }
  })
  
  # Reset view button
  observeEvent(input$reset_view, {
    updateReactable("city_table", selected = NA)
    leafletProxy("city_map") |> 
      setView(lng = default_view$lng, lat = default_view$lat, zoom = default_view$zoom)
  })
}

shinyApp(ui, server)