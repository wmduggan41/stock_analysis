# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - BOOTSTRAP -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Add Settings Toggle using shinyjs 


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyquant)
library(tidyverse)
library(zoo)
library(pipeR)
library(httr)
library(jsonlite)
library(DT)

source(file = "00_scripts/stock_analysis_functions.R")

#api_key = "."

stock_list_tbl <- get_stock_list("SP500")

gold_data <- get_gold_list(api_key = api_key)

# UI ----
ui <- navbarPage(
    title = "Stock Analyzer",
    inverse = FALSE,
    collapsible = TRUE,
    
    theme = shinytheme("paper"),
    
    tabPanel(
        title = "WM Duggan",
        
        # CSS ----
        shinythemes::themeSelector(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
        ),
        
        # JS ----
        shinyjs::useShinyjs(),
        
        # 1.0 HEADER ----
        div(
            class = "container",
            id = "header",
            h1(class = "page-header", "Stock Analyzer", tags$small("Business Science")),
            p(class = "lead", "This is the first mini-project completed in", 
              a(href = "https://www.business-science.io/", target = "_blank", "Expert Shiny Applications Course (DS4B 202-R)"))
        ),

        # 2.0 APPLICATION UI -----
        div(
            class = "container",
            id = "application_ui",
            column(
                width = 4, 
                wellPanel(
                    div(
                        id = "input_main",
                        pickerInput(
                            inputId = "stock_selection", 
                            label   = "Stock List (Pick One to Analyze)",
                            choices = stock_list_tbl$label,
                            multiple = FALSE, 
                            selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label),
                            options = pickerOptions(
                                actionsBox = FALSE,
                                liveSearch = TRUE,
                                size = 10
                            )
                        )
                    ),
                    div(
                        id = "input_buttons",
                        actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
                        div(
                            class = "pull-right",
                            actionButton(inputId = "settings_toggle", label = NULL, icon = icon("cog"))
                        )
                    ),
                    div(
                        id = "input_settings",
                        hr(),
                        sliderInput(inputId = "mavg_short", label = "Short Moving Average", value = 20, min = 5, max = 40),
                        sliderInput(inputId = "mavg_long", label = "Long Moving Average", value = 50, min = 50, max = 120)
                    ) %>% hidden()
                )
            ),
            column(
                width = 8, 
                div(
                    class = "panel", 
                    div(
                        class = "panel-header",
                        h4(textOutput(outputId = "plot_header"))
                    ),
                    div(
                        class = "panel-body",
                        plotlyOutput(outputId = "plotly_plot")
                    )
                )
            )
        ),
        
        # 3.0 ANALYST COMMENTARY ----
        div(
            class = "container",
            id = "commentary",
            column(
                width = 12,
                div(
                    class = "panel",
                    div(class = "panel-header", h4("Analyst Commentary")),
                    div(
                        class = "panel-body",
                        textOutput(outputId = "analyst_commentary")
                    )
                )
            )
        ),
        
        # 3.1 GOLD DATA ----
        div(
            class = "container",
            id = "gold_data_plot",
            column(
                width = 12,
                div(
                    class = "panel",
                    div(
                        class = "panel-header",
                        h4("Gold Price Trend")),
                    div(
                        class = "panel-body",
                        plotlyOutput(outputId = "plotly_gold_plot")
                    )
                )
            )
        ),
    )
)
    
# SERVER ----
server <- function(input, output, session) {
    
    # Toggle Input Settings ----
    observeEvent(input$settings_toggle, {
        toggle(id = "input_settings", anim = TRUE)
    })
    
    # Stock Symbol ----
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = FALSE)
    
    # User Input ----
    stock_selection_triggered <- eventReactive(input$analyze, {
        input$stock_selection
    }, ignoreNULL = FALSE)

    # Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>% 
            get_stock_data(
                from = today() - days(180), 
                to   = today(),
                mavg_short = input$mavg_short,
                mavg_long  = input$mavg_long)
    })
    
    # Get Gold Price Data ----
    gold_data_tbl <- reactive({
        get_gold_list(api_key = api_key)
    })
    
    # Plot Header ----
    output$plot_header <- renderText({
        stock_selection_triggered()
    })
    
    # Plotly stock Plot ----
    output$plotly_plot <- renderPlotly({
        stock_data_tbl() %>% plot_stock_data()
    })
    
    # Plotly Gold Plot ----
    output$plotly_gold_plot <- renderPlotly({
        ggplotly(plot_gold_data(gold_data_tbl()))
    })
    
    # Generate Commentary ----
    output$analyst_commentary <- renderText({
        generate_commentary(data = stock_data_tbl(), user_input = stock_selection_triggered())
    })
    
}


# RUN APP ----
shinyApp(ui = ui, server = server)
