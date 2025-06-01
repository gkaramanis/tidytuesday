library(shiny)
library(tidyverse)
library(fmsb)
library(bslib)

monsters_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

monsters <- monsters_raw |> 
    select(name, size, type, alignment, ac, hp, cr, initiative, str:cha, speed, skills, senses, languages) |> 
    mutate(across(str:cha, function(x) paste0(x, " (", ifelse(floor((x - 10) / 2) > 0, "+", ""), floor((x - 10) / 2), ")"), .names = "mod_{.col}")) |> 
    mutate(across(speed:senses, function(x) replace_na(x, "-")))

ui <- page_fluid(
    
    tags$head(tags$style(HTML("
    body {
      background: linear-gradient(135deg, #f4f1e8 0%, #e8dcc0 50%, #d4c7a1 100%);
      min-height: 100vh;
    }
    
    .card {
      background: linear-gradient(145deg, #fffdf7e6, #f8f3e6f2) !important;
      border: 1px solid #8b745266 !important;
      box-shadow: 0 4px 16px #8b745233, inset 0 1px 0 #ffffffcc !important;
    }
    
    .card-body {
      color: #4a3728 !important;
    }
    
    h2 {
      color: #8b5a3c !important;
      text-shadow: 1px 1px 2px #ffffff80;
      margin-bottom: 30px;
    }
    
    .form-select, .form-control {
      background: #fffdf7e6 !important;
      border: 1px solid #8b745280 !important;
      color: #4a3728 !important;
    }
    
    .form-select:focus, .form-control:focus {
      border-color: #8b5a3c !important;
      box-shadow: 0 0 0 0.2rem #8b5a3c40 !important;
      background: #fffdf7f2 !important;
    }
    
    .selectize-input, .selectize-control.single .selectize-input {
      background: #fffdf7e6 !important;
      border: 1px solid #8b745280 !important;
      color: #4a3728 !important;
    }
    
    .selectize-dropdown {
      background: #fffdf7fa !important;
      border: 1px solid #8b745280 !important;
      color: #4a3728 !important;
    }
    
    .selectize-dropdown-content .option {
      color: #4a3728 !important;
    }
    
    .selectize-dropdown-content .option:hover,
    .selectize-dropdown-content .option.active {
      background: #8b5a3c26 !important;
      color: #8b5a3c !important;
    }
    
    .overflow {
      overflow: visible !important;
    }
    
    .comparison * {
      color: #4a6b2e !important;
      border-color: #6b8b4a66 !important;
    }
    
    .stats {
      font-size: 1.2em;
    }
    
    .comparison .form-select, .comparison .selectize-input {
      background: #a8c5871a !important;
      border-color: #6b8b4a66 !important;
    }
    
    label {
      color: #8b5a3c !important;
      font-weight: 600;
    }
    
    strong {
      color: #6b4423 !important;
    }
    
    #monster_name + .selectize-control .selectize-input {
      font-size: 1.3em !important;
    }
    
  "))),
    theme = bs_theme(
        base_font = c("Iosevka Fixed", "monospace"),
        heading_font = c("Trattatello", "fantasy"),
        font_scale = 1.1
    ),
    
    titlePanel(h2("Dungeons and Dragons Monsters (2024)", align = "center", style = "font-size: 3em")),
    
    fluidRow(
        selectInput("monster_name", "Select or type monster name", choices = NULL, selectize = TRUE, width = "500px")
    ),
    
    layout_columns(
        col_widths = c(7, 5),
        card(
            textOutput("stats1"),
            layout_columns(
                card(htmlOutput("stats2", inline = TRUE, class = "stats")),
                card(htmlOutput("stats3", inline = TRUE, class = "stats"))
            ),
            htmlOutput("stats4", inline = TRUE)
        ),
        card(
            class = "overflow",
            wrapper = function(...) card_body(..., class = "overflow"),
            plotOutput("radarPlot"),
            tags$div(class = "comparison",
                     layout_columns(
                         selectInput("comparison_category", "Compare by", choices = NULL),
                         selectInput("comparison_value", "Filter", choices = NULL)
                     )
            )
        )
    )
)

server <- function(session, input, output) {
    
    updateSelectInput(session, 'monster_name', choices = unique(monsters_raw$name))
    
    updateSelectInput(session, 'comparison_category', 
                      choices = c("All monsters", "Size" = "size", "Type" = "type", "Alignment" = "alignment"),
                      selected = "All monsters")
    
    observe({
        if (input$comparison_category == "All monsters") {
            updateSelectInput(session, 'comparison_value', choices = "All", selected = "All")
        } else {
            column_values <- unique(monsters_raw[[input$comparison_category]])
            column_values <- column_values[!is.na(column_values)]
            updateSelectInput(session, 'comparison_value', 
                              choices = c("All", column_values),
                              selected = "All")
        }
    })
    
    subsetted <- reactive({
        req(input$monster_name)
        monsters |> 
            filter(name == input$monster_name)
    })
    
    filtered_monsters <- reactive({
        req(input$comparison_category, input$comparison_value)
        if (input$comparison_category == "All monsters" || input$comparison_value == "All") {
            monsters_raw
        } else {
            monsters_raw |> 
                filter(.data[[input$comparison_category]] == input$comparison_value)
        }
    })
    
    output$stats1 <- renderText(
        paste0(
            subsetted()$size, ", ",
            subsetted()$type, ", ",
            subsetted()$alignment
        )
    )
    
    output$stats2 <- renderText(
        HTML(paste0(
            "<strong>AC </strong>", subsetted()$ac, "<br>",
            "<strong>HP </strong>", subsetted()$hp, "<br>",
            "<strong>CR </strong>", subsetted()$cr, "<br>",
            "<strong>Init </strong>+", subsetted()$initiative
        ))
    )
    
    output$stats3 <- renderText(
        HTML(paste0(
            "<strong>STR </strong>", ifelse(subsetted()$str < 10, "&nbsp;", ""), subsetted()$mod_str, "<br>",
            "<strong>DEX </strong>", ifelse(subsetted()$dex < 10, "&nbsp;", ""), subsetted()$mod_dex, "<br>",
            "<strong>CON </strong>", ifelse(subsetted()$con < 10, "&nbsp;", ""), subsetted()$mod_con, "<br>",
            "<strong>INT </strong>", ifelse(subsetted()$int < 10, "&nbsp;", ""), subsetted()$mod_int, "<br>",
            "<strong>WIS </strong>", ifelse(subsetted()$wis < 10, "&nbsp;", ""), subsetted()$mod_wis, "<br>",
            "<strong>CHA </strong>", ifelse(subsetted()$cha < 10, "&nbsp;", ""), subsetted()$mod_cha
        ))
    )
    
    output$radarPlot <- renderPlot({
        par(mar = c(0, 0, 0, 0), family = "Iosevka Fixed", font = 2, cex = 1.5, 
            bg = "#f8f3e6")
        
        # Calculate group means
        group_means <- filtered_monsters() |> 
            summarise(across(str:cha, mean, na.rm = TRUE))
        
        # Prepare data for radar chart
        radar_data <- subsetted() |> 
            select(str:cha) |> 
            add_row(group_means, .after = 1) |> 
            add_row(
                str = 30, dex = 30, con = 30, int = 30, wis = 30, cha = 30,
                .before = 1
            ) |> 
            add_row(
                str = 0, dex = 0, con = 0, int = 0, wis = 0, cha = 0,
                .before = 2
            ) |> 
            select(str, cha:dex) |> 
            janitor::clean_names(case = "all_caps")
        
        radarchart(
            radar_data,
            seg = 2,
            cglty = 1, 
            cglcol = alpha("#8b7452", 0.4),
            pcol = c("#8b5a3c", "#6b8b4a"), 
            plwd = c(4, 2),
            pdensity = c(12, 0),
            pfcol = c(alpha("#8b5a3c", 0.3), alpha("#6b8b4a", 0.2))
        )
    })
    
    output$stats4 <- renderText(
        HTML(paste0(
            "<strong>Speed </strong><br>", subsetted()$speed, "<br>",
            "<strong>Skills </strong><br>", subsetted()$skills, "<br>",
            "<strong>Senses </strong><br>", subsetted()$senses, "<br>",
            "<strong>Languages </strong><br>", subsetted()$languages, "<br>"
        ))
    )
}

shinyApp(ui = ui, server = server)