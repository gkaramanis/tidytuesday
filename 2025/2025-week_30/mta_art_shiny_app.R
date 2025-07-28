library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(htmltools)

# Read in data
mta_art <- read_csv("data/mta_art.csv") %>%
  # Clean up strange characters in text fields
  mutate(
    art_description = str_replace_all(
      art_description, "Ò", "\"") %>%
      str_replace_all("Ó", "\"") %>%
      str_replace_all("Õ", "'") %>%
      str_replace_all("Ô", "'") %>%
      str_replace_all("Ð", "–")
  )

# Filter for notable artworks using tribble
notable_artworks <- tribble(
  ~art_title, ~artist, ~station_name,
  "Wall-Slide", "Vito Acconci", "161 St-Yankee Stadium",
  "Subway Portraits", "Chuck Close", "86 St",
  "Blueprint for a Landscape", "Sarah Sze", "96 St",
  "Perfect Strangers", "Vik Muniz", "72 St",
  "CHORUS", "Ann Hamilton", "WTC Cortlandt",
  "Grand Central: Arches, Towers, Pyramids", "Jackie Ferrara", "Grand Central-42 St",
  "Radiant Site", "Michele Oka Doner", "34 St-Herald Sq",
  "Framing Union Square", "Mary Miss", "14 St-Union Sq",
  "Masstransiscope", "Bill Brand", "DeKalb Av",
  "Polyrhythmics of Consciousness and Light", "Valerie Maynard", "125 St",
  "For Want of a Nail", "MTA Arts & Design Collaborative", "81 St-Museum of Natural History",
  "The Subway: Design for a Modern Icon", "MTA Arts & Design and The Museum of Modern Art", "5 Av/53 St",
  "Great Waves of Immigration", "Carmen Lizardo", "181 St",
  "Landscapes adrift-cosmically woven and earthly bonded", "David Rios Ferreira", "7 Av",
  "Marine Grill Murals", "Frederick Dana Marsh", "Fulton St"
)

# Filter mta_art to only include notable artworks
mta_art <- mta_art %>%
  inner_join(notable_artworks, by = c("art_title", "artist", "station_name"))

# Read spatial data and station coordinates
sub_lines_sf <- read_sf("data/Subway.geojson") %>% 
  janitor::clean_names()

# Read station coordinates from CSV and prepare for better matching
stations_csv <- read_csv("data/MTA_Subway_Stations_and_Complexes.csv", show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  # For stations with multiple entries, keep only the first occurrence of each unique stop name
  # This helps avoid duplicates from multiple station entrances/exits
  group_by(stop_name) %>%
  slice(1) %>%
  ungroup()

# Join artwork with station coordinates
mta_art_with_coords <- mta_art %>%
  left_join(
    stations_csv %>% select(stop_name, latitude, longitude),
    by = c("station_name" = "stop_name")
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  # Ensure unique artworks (remove any remaining duplicates)
  distinct(art_title, artist, station_name, .keep_all = TRUE) %>%
  # Convert to sf object
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Transform subway lines to WGS84 for leaflet
sub_lines_sf_wgs84 <- st_transform(sub_lines_sf, 4326)

# Use the artwork with coordinates as our main data
sub_stations_sf_art_wgs84 <- mta_art_with_coords

# Function to generate image filename from artist and title
get_image_filename <- function(artist, art_title) {
  safe_artist <- str_replace_all(artist, "[^A-Za-z0-9 ]", "") %>%
    str_replace_all("\\s+", "_") %>%
    str_to_lower()
  
  safe_title <- str_replace_all(art_title, "[^A-Za-z0-9 ]", "") %>%
    str_replace_all("\\s+", "_") %>%
    str_to_lower()
  
  # Truncate to prevent long filenames that exceed TAR limits
  safe_artist <- substr(safe_artist, 1, 20)  # Limit artist name to 20 characters
  safe_title <- substr(safe_title, 1, 30)    # Limit title to 30 characters
  
  filename <- paste0(safe_artist, "_", safe_title, ".jpg")
  file_path <- paste0("data/artwork_images/", filename)
  
  # Check if file exists, return relative path if it does
  if (file.exists(file_path)) {
    return(paste0("data/artwork_images/", filename))
  } else {
    return(NULL)
  }
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
      
      .control-panel {
        background: #ffffff;
        border-radius: 0;
        padding: 10px;
        margin: 0 20px 10px 20px;
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
        
        .main-title {
          font-size: 3rem !important;
        }
      }
      
      .map-container {
        border-radius: 0;
        overflow: hidden;
        margin: 10px 0 2px 10px;
        border: 2px solid #e2e8f0;
        border-top: 4px solid #3b82f6;
        height: calc(100vh - 320px);
      }
      
      .artwork-viewer {
        background: #ffffff;
        border-radius: 0;
        padding: 0;
        margin: 10px 20px 2px 0;
        border: 2px solid #e2e8f0;
        border-left: 4px solid #8b5cf6;
        height: calc(100vh - 320px);
        overflow-y: auto;
        display: flex;
        flex-direction: column;
      }
      
      .artwork-viewer-header {
        background: linear-gradient(135deg, #8b5cf6 0%, #3b82f6 100%);
        color: #ffffff;
        padding: 20px;
        text-align: center;
        border-bottom: 2px solid #e2e8f0;
      }
      
      .artwork-viewer-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1.2rem;
        font-weight: 700;
        text-transform: uppercase;
        margin: 0;
        letter-spacing: 1px;
      }
      
      .artwork-viewer-content {
        padding: 20px;
        flex: 1;
        overflow-y: auto;
      }
      
      /* Custom scrollbar for artwork viewer */
      .artwork-viewer-content::-webkit-scrollbar {
        width: 4px;
      }
      
      .artwork-viewer-content::-webkit-scrollbar-track {
        background: #f1f5f9;
      }
      
      .artwork-viewer-content::-webkit-scrollbar-thumb {
        background: #8b5cf6;
        border-radius: 2px;
      }
      
      .artwork-viewer-content::-webkit-scrollbar-thumb:hover {
        background: #7c3aed;
      }
      
      .artwork-gallery {
        max-width: 100%;
        margin: 0 auto;
      }
      
      .artwork-image-container {
        width: 100%;
        height: 300px;
        background: #f1f5f9;
        border: 2px solid #e2e8f0;
        margin-bottom: 25px;
        display: flex;
        align-items: center;
        justify-content: center;
        position: relative;
        overflow: hidden;
      }
      
      .artwork-image-container img {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
      
      .artwork-image-placeholder {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1rem;
        font-weight: 700;
        color: #64748b;
        text-transform: uppercase;
        letter-spacing: 1px;
        text-align: center;
        line-height: 1.4;
      }
      
      .artwork-gallery-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1.8rem;
        font-weight: 700;
        color: #0f172a;
        text-transform: uppercase;
        margin: 0 0 15px 0;
        letter-spacing: 1px;
        line-height: 1.2;
        border-bottom: 3px solid #8b5cf6;
        padding-bottom: 10px;
      }
      
      .artwork-gallery-artist {
        font-family: 'Inter', sans-serif;
        font-size: 1.3rem;
        font-weight: 600;
        color: #8b5cf6;
        text-transform: uppercase;
        margin: 0 0 10px 0;
        letter-spacing: 0.5px;
      }
      
      .artwork-gallery-station {
        font-family: 'Inter', sans-serif;
        font-size: 1.1rem;
        font-weight: 600;
        color: #64748b;
        text-transform: uppercase;
        margin: 0 0 25px 0;
        font-style: italic;
        letter-spacing: 0.5px;
      }
      
      .artwork-gallery-meta {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-left: 4px solid #8b5cf6;
        padding: 20px;
        margin: 0 0 25px 0;
      }
      
      .artwork-gallery-meta-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1rem;
        font-weight: 700;
        color: #0f172a;
        text-transform: uppercase;
        margin: 0 0 15px 0;
        letter-spacing: 1px;
      }
      
      .artwork-gallery-meta-item {
        font-family: 'Inter', sans-serif;
        font-size: 1rem;
        font-weight: 600;
        color: #1e293b;
        margin: 0 0 8px 0;
        display: flex;
      }
      
      .artwork-gallery-meta-label {
        min-width: 100px;
        font-weight: 700;
        text-transform: uppercase;
        color: #64748b;
      }
      
      .artwork-gallery-description {
        background: #ffffff;
        border: 1px solid #e2e8f0;
        padding: 20px;
        margin: 0 0 25px 0;
      }
      
      .artwork-gallery-description-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1rem;
        font-weight: 700;
        color: #0f172a;
        text-transform: uppercase;
        margin: 0 0 15px 0;
        letter-spacing: 1px;
      }
      
      .artwork-gallery-description-text {
        font-family: 'Inter', sans-serif;
        font-size: 1rem;
        line-height: 1.6;
        color: #1e293b;
        margin: 0;
      }
      
      .artwork-gallery-actions {
        text-align: center;
        margin-top: 25px;
      }
      
      .artwork-gallery-button {
        background: linear-gradient(135deg, #8b5cf6 0%, #3b82f6 100%);
        color: #ffffff;
        border: none;
        padding: 15px 30px;
        font-family: 'Space Grotesk', sans-serif;
        font-weight: 700;
        text-transform: uppercase;
        cursor: pointer;
        text-decoration: none;
        display: inline-block;
        font-size: 1rem;
        letter-spacing: 1px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 12px rgba(139, 92, 246, 0.3);
      }
      
      .artwork-gallery-button:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(139, 92, 246, 0.4);
      }
      
      .no-artwork-selected {
        text-align: center;
        padding: 60px 20px;
        color: #64748b;
      }
      
      .no-artwork-selected-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 1.4rem;
        font-weight: 700;
        color: #64748b;
        text-transform: uppercase;
        margin: 0 0 10px 0;
        letter-spacing: 1px;
      }
      
      .no-artwork-selected-text {
        font-family: 'Inter', sans-serif;
        font-size: 1rem;
        font-weight: 600;
        color: #94a3b8;
        text-transform: uppercase;
        margin: 0;
      }
      
      .artwork-index-container {
        display: flex;
        flex-wrap: wrap;
        gap: 6px;
        margin: 0;
      }
      
      .artwork-index-button {
        background: #f8fafc;
        border: 2px solid #e2e8f0;
        color: #64748b;
        padding: 5px 8px;
        font-family: 'Inter', sans-serif;
        font-size: 0.65rem;
        font-weight: 600;
        text-transform: uppercase;
        cursor: pointer;
        transition: all 0.2s ease;
        border-radius: 0;
        text-align: center;
        letter-spacing: 0.5px;
        line-height: 1.2;
        flex: 0 0 auto;
        width: auto;
      }
      
      .artwork-index-button:hover {
        background: #ede9fe;
        border-color: #8b5cf6;
        color: #8b5cf6;
        transform: translateY(-1px);
      }
      
      .artwork-index-button.selected {
        background: #8b5cf6;
        border-color: #8b5cf6;
        color: #ffffff;
        font-weight: 700;
      }
      
      .clear-selection-button {
        background: #ef4444;
        border: 2px solid #ef4444;
        color: #ffffff;
        padding: 5px 12px;
        font-family: 'Inter', sans-serif;
        font-size: 0.65rem;
        font-weight: 700;
        text-transform: uppercase;
        cursor: pointer;
        transition: all 0.2s ease;
        border-radius: 0;
        text-align: center;
        letter-spacing: 0.5px;
        line-height: 1.2;
        flex: 0 0 auto;
        width: auto;
        margin-left: 8px;
      }
      
      .clear-selection-button:hover {
        background: #dc2626;
        border-color: #dc2626;
        transform: translateY(-1px);
      }
      
      .artwork-index-title {
        font-family: 'Space Grotesk', sans-serif;
        font-size: 0.9rem;
        font-weight: 700;
        color: #0f172a;
        text-transform: uppercase;
        margin: 0 0 10px 0;
        letter-spacing: 1px;
        text-align: center;
      }
      
      .footer {
        background: #1e293b;
        color: #ffffff;
        padding: 20px;
        margin-top: 2px;
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
    "))
  ),
  
  div(class = "main-title", "NEW YORK'S UNDERGROUND GALLERY"),
  
  # Top control bar with artwork index
  fluidRow(
    div(class = "control-panel",
        div(class = "artwork-index-title", "NOTABLE ARTWORKS"),
        htmlOutput("artwork_index_buttons")
    )
  ),
  
  # Main content: Map + Right Sidebar
  fluidRow(
    column(7,
           div(class = "map-container",
               leafletOutput("subway_map", height = "100%")
           )
    ),
    
    column(5,
           div(class = "artwork-viewer",
               div(class = "artwork-viewer-header",
                   div(class = "artwork-viewer-title", "ARTWORK DETAILS")
               ),
               div(class = "artwork-viewer-content",
                   htmlOutput("artwork_viewer_content")
               )
           )
    )
  ),
  
  # Footer
  div(class = "footer",
      div(class = "footer-content",
          p("Data sources: ",
            a(href = "https://www.mta.info/agency/arts-design", "MTA Arts & Design Collection", target = "_blank"),
            " (as of December 2024)",
            " • Geographic data: ",
            a(href = "https://github.com/CityOfNewYork/nyc-geo-metadata/tree/main", "NYC OpenData", target = "_blank"),
            " • Created by ", 
            a(href = "https://github.com/gkaramanis", "Georgios Karamanis", target = "_blank")
          )
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Add resource path for artwork images
  addResourcePath("data", "data")
  
  # Reactive value to store current selected artwork
  selected_artwork_reactive <- reactiveVal(NULL)
  
  # Get all notable artworks with their station data
  notable_artworks_data <- reactive({
    sub_stations_sf_art_wgs84 %>%
      filter(!is.na(art_title)) %>%
      # Ensure we only have unique artworks (no duplicates)
      distinct(art_title, artist, station_name, .keep_all = TRUE) %>%
      arrange(art_title)
  })
  
  # Render artwork index buttons
  output$artwork_index_buttons <- renderUI({
    artworks_data <- notable_artworks_data()
    selected_artwork <- selected_artwork_reactive()
    
    if (nrow(artworks_data) == 0) {
      return(div("No artworks available"))
    }
    
    buttons <- lapply(seq_len(nrow(artworks_data)), function(i) {
      artwork <- artworks_data[i, ]
      is_selected <- !is.null(selected_artwork) && 
                     identical(artwork$art_title, selected_artwork$art_title) &&
                     identical(artwork$artist, selected_artwork$artist)
      
      button_class <- paste("artwork-index-button", if(is_selected) "selected" else "")
      
      tags$button(
        class = button_class,
        onclick = paste0("Shiny.setInputValue('artwork_button_selected', ", i, ", {priority: 'event'});"),
        ifelse(!is.na(artwork$art_title) && nchar(trimws(artwork$art_title)) > 0, 
               trimws(artwork$art_title), "UNTITLED")
      )
    })
    
    # Add clear button
    clear_button <- tags$button(
      class = "clear-selection-button",
      onclick = "Shiny.setInputValue('clear_selection', Math.random(), {priority: 'event'});",
      "CLEAR"
    )
    
    div(class = "artwork-index-container", buttons, clear_button)
  })
  
  # Handle artwork button selection
  observeEvent(input$artwork_button_selected, {
    artworks_data <- notable_artworks_data()
    if (input$artwork_button_selected > 0 && input$artwork_button_selected <= nrow(artworks_data)) {
      selected_artwork <- artworks_data[input$artwork_button_selected, ]
      selected_artwork_reactive(selected_artwork)
      
      # Center map on selected artwork
      coords <- st_coordinates(selected_artwork)
      if (nrow(coords) > 0) {
        lng <- coords[1, "X"]
        lat <- coords[1, "Y"]
        leafletProxy("subway_map") %>%
          setView(lng = lng, lat = lat, zoom = 15)
      }
    }
  })
  
  # Handle clear selection button
  observeEvent(input$clear_selection, {
    selected_artwork_reactive(NULL)
    
    # Reset map view to show all of NYC
    leafletProxy("subway_map") %>%
      setView(lng = -73.9857, lat = 40.7484, zoom = 11)
  })

  # Render artwork viewer content
  output$artwork_viewer_content <- renderUI({
    selected_artwork <- selected_artwork_reactive()
    
    if (is.null(selected_artwork)) {
      return(
        div(class = "no-artwork-selected",
            div(class = "no-artwork-selected-title", "SELECT AN ARTWORK"),
            div(class = "no-artwork-selected-text", "Click on a marker on the map or button above to view artwork details")
        )
      )
    }
    
    # Gallery-style layout for selected artwork
    div(class = "artwork-gallery",
        # Image container with actual image or placeholder
        {
          image_path <- get_image_filename(selected_artwork$artist, selected_artwork$art_title)
          if (!is.null(image_path)) {
            div(class = "artwork-image-container",
                tags$img(
                  src = image_path,
                  alt = paste("Artwork:", selected_artwork$art_title, "by", selected_artwork$artist)
                )
            )
          } else {
            div(class = "artwork-image-container",
                div(class = "artwork-image-placeholder",
                    "ARTWORK IMAGE",
                    br(),
                    "PLACEHOLDER"
                )
            )
          }
        },
        
        # Title
        div(class = "artwork-gallery-title",
            ifelse(!is.na(selected_artwork$art_title) && nchar(trimws(selected_artwork$art_title)) > 0, 
                   trimws(selected_artwork$art_title), "UNTITLED")
        ),
        
        # Artist
        div(class = "artwork-gallery-artist",
            ifelse(!is.na(selected_artwork$artist) && nchar(trimws(selected_artwork$artist)) > 0, 
                   trimws(selected_artwork$artist), "UNKNOWN ARTIST")
        ),
        
        # Station
        div(class = "artwork-gallery-station",
            paste("LOCATED AT", toupper(selected_artwork$station_name))
        ),
        
        # Meta information
        if ((!is.na(selected_artwork$art_date) && nchar(trimws(selected_artwork$art_date)) > 0) || 
            (!is.na(selected_artwork$art_material) && nchar(trimws(selected_artwork$art_material)) > 0) ||
            (!is.na(selected_artwork$line) && nchar(trimws(selected_artwork$line)) > 0)) {
          div(class = "artwork-gallery-meta",
              div(class = "artwork-gallery-meta-title", "ARTWORK DETAILS"),
              if (!is.na(selected_artwork$art_date) && nchar(trimws(selected_artwork$art_date)) > 0) {
                div(class = "artwork-gallery-meta-item",
                    span(class = "artwork-gallery-meta-label", "DATE:"),
                    span(trimws(selected_artwork$art_date))
                )
              },
              if (!is.na(selected_artwork$art_material) && nchar(trimws(selected_artwork$art_material)) > 0) {
                div(class = "artwork-gallery-meta-item",
                    span(class = "artwork-gallery-meta-label", "MEDIUM:"),
                    span(trimws(selected_artwork$art_material))
                )
              },
              if (!is.na(selected_artwork$line) && nchar(trimws(selected_artwork$line)) > 0) {
                div(class = "artwork-gallery-meta-item",
                    span(class = "artwork-gallery-meta-label", "LINE:"),
                    span(trimws(selected_artwork$line))
                )
              },
              if (!is.na(selected_artwork$agency) && nchar(trimws(selected_artwork$agency)) > 0) {
                div(class = "artwork-gallery-meta-item",
                    span(class = "artwork-gallery-meta-label", "AGENCY:"),
                    span(trimws(selected_artwork$agency))
                )
              }
          )
        },
        
        # Description
        if (!is.na(selected_artwork$art_description) && nchar(trimws(selected_artwork$art_description)) > 0) {
          div(class = "artwork-gallery-description",
              div(class = "artwork-gallery-description-title", "DESCRIPTION"),
              div(class = "artwork-gallery-description-text", 
                  trimws(selected_artwork$art_description))
          )
        },
        
        # Action button
        if (!is.na(selected_artwork$art_image_link) && nchar(trimws(selected_artwork$art_image_link)) > 0) {
          div(class = "artwork-gallery-actions",
              tags$a(
                href = trimws(selected_artwork$art_image_link),
                target = "_blank",
                class = "artwork-gallery-button",
                "VIEW ON MTA WEBSITE"
              )
          )
        }
    )
  })
  
  # Handle artwork selection from map markers
  observeEvent(input$subway_map_marker_click, {
    click_info <- input$subway_map_marker_click
    if (!is.null(click_info$id) && grepl("^artwork_", click_info$id)) {
      artwork_index <- as.numeric(gsub("artwork_", "", click_info$id))
      artworks_data <- notable_artworks_data()
      
      if (artwork_index > 0 && artwork_index <= nrow(artworks_data)) {
        selected_artwork <- artworks_data[artwork_index, ]
        selected_artwork_reactive(selected_artwork)
        
        # Center map on selected artwork
        coords <- st_coordinates(selected_artwork)
        if (nrow(coords) > 0) {
          lng <- coords[1, "X"]
          lat <- coords[1, "Y"]
          leafletProxy("subway_map") %>%
            setView(lng = lng, lat = lat, zoom = 15)
        }
      }
    }
  })
  
  # Render leaflet map with artistic styling
  output$subway_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 10, maxZoom = 14)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.9857, lat = 40.7484, zoom = 11)
  })
  
  # Update map based on notable artworks with selection highlighting
  observe({
    tryCatch({
      selected_artwork <- selected_artwork_reactive()
      
      leafletProxy("subway_map") %>%
        clearShapes() %>%
        clearMarkers()
      
      # Add subway lines with default color
      if (nrow(sub_lines_sf_wgs84) > 0) {
        leafletProxy("subway_map") %>%
          addPolylines(
            data = sub_lines_sf_wgs84,
            color = "#34495e",  # Default gray for all lines
            weight = 3,
            opacity = 0.7
          )
      }
      
      # Add only notable artworks as markers
      stations_data <- notable_artworks_data()
      
      if (nrow(stations_data) > 0) {
        for (i in seq_len(nrow(stations_data))) {
          station <- stations_data[i, ]
          
          # Check if this artwork is selected
          is_selected <- !is.null(selected_artwork) && 
                         identical(station$art_title, selected_artwork$art_title) &&
                         identical(station$artist, selected_artwork$artist)
          
          # Use different colors for selected vs unselected
          marker_color <- if(is_selected) "#3b82f6" else "#8b5cf6"
          marker_radius <- if(is_selected) 8 else 6
          
          leafletProxy("subway_map") %>%
            addCircleMarkers(
              data = station,
              radius = marker_radius,
              color = marker_color,
              fillColor = marker_color,
              fillOpacity = if(is_selected) 1.0 else 0.9,
              stroke = TRUE,
              weight = if(is_selected) 4 else 3,
              layerId = paste0("artwork_", i)
            )
        }
      }
    }, error = function(e) {
      print(paste("Error in map update:", e$message))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)