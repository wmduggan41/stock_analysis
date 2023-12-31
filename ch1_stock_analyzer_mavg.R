# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - CHALLENGE #1 -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Add Moving Average Functionality
# - UI Placement: 
#   - Add a horizontal rule between the Analyze button and the new UI. 
#   - Place the Sliders below the button and horizontal rule
# - Short MAVG Requirements: Starting value of 20, min of 5, max of 40
# - Long MAVG Requirements: Starting value of 50, min of 50, max of 120
# - Server requirements: Update immediately on change


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyquant)
library(tidyverse)

source(file = "00_scripts/stock_analysis_functions.R")

stock_list_tbl <- get_stock_list("SP500")

# UI ----
ui <- fluidPage(
    title = "Stock Analyzer",
    
    # 1.0 HEADER ----
    div(
        h1("Stock Analyzer", "by WM Duggan"),
        p("This is the first mini-project completed in the", "Expert Shiny Applications Course (DS4B 202-R)")
    ),
    
    # 2.0 APPLICATION UI -----
    div(
        column(
            width = 4, 
            wellPanel(
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
                ),
                actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
                hr(),
                sliderInput(inputId = "mavg_short", label = "Short Moving Average", value = 20, min = 5, max = 40),
                sliderInput(inputId = "mavg_long", label = "Long Moving Average", value = 50, min = 50, max = 120)
                # verbatimTextOutput(outputId = "slider_1")
            )
        ),
        column(
            width = 8, 
            div(
                div(h4(textOutput(outputId = "plot_header"))),
                div(
                    plotlyOutput(outputId = "plotly_plot")
                )
            )
        )
    ),
    
    # 3.0 ANALYST COMMENTARY ----
    div(
        column(
            width = 12,
            div(
                div(h4("Analyst Commentary")),
                div(
                    textOutput(outputId = "analyst_commentary")
                )
            )
        )
    )
)

# SERVER ----
server <- function(input, output, session) {
    
    # Stock Symbol ----
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = FALSE)
    
    # User Input ----
    stock_selection_triggered <- eventReactive(input$analyze, {
        input$stock_selection
    }, ignoreNULL = FALSE)
    
    # output$slider_1 <- renderPrint(input$mavg_short)
    
    # Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>% 
            get_stock_data(
                from = today() - days(180), 
                to   = today(),
                mavg_short = input$mavg_short,
                mavg_long  = input$mavg_long)
    })
    
    # Plot Header ----
    output$plot_header <- renderText({
        stock_selection_triggered()
    })
    
    # Plotly Plot ----
    output$plotly_plot <- renderPlotly({
        stock_data_tbl() %>% plot_stock_data()
    })
    
    # Generate Commentary ----
    output$analyst_commentary <- renderText({
        generate_commentary(data = stock_data_tbl(), user_input = stock_selection_triggered())
    })
    
}

# RUN APP ----
shinyApp(ui = ui, server = server)