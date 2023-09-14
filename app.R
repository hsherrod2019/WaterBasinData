library(tidyverse)
library(randomForest)
library(janitor)
library(caret)
library(missForest)
library(NADA)
library(ggplot2)
library(bs4Dash)
library(leaflet)
library(shiny)
library(plotly)
library(dplyr)

# Read data files
rf_data = readRDS("rf_model.rds")
full_data = readRDS("imputed_data.rds")
map_data = readRDS("map_data.rds")

# Extract unique river names from map data
river_name <- unique(map_data$river_name)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Plastic Predictions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map of the US", tabName = "maps", icon = icon("map")),
      menuItem("Predictions", tabName = "prediction", icon = icon("sliders-h")),
      menuItem("About", tabName = "about", icon = icon("question"))
    )
  ),
  dashboardBody(
    tags$style(
      HTML(
        "
      /* Change the color of the navbar */
      .navbar,
      .main-header .logo {
        background-color: #6d929b;
      }

      /* Change the color of the sidebar */
      .main-sidebar {
        background-color: #6d929b;
      }

      /* Change the color of the sidebar text */
      .main-sidebar .nav-sidebar .nav-item .nav-link {
        color: #f5fafa;
      }
      
      /* Change the color of the title */
      .main-header .navbar-brand {
        color: #f5fafa; */
      }
      "
      )
    ),
    tabItems(
      tabItem(
        tabName = "maps",
        h2("Map of the United States- Plastics Concentration"),
        tags$p("Samples used to create the random forest model in the Predictions tab and concentration of plastics predicted by the model. Darker popup markers represent higher concentrations, while lighter popup markers represent lower concentrations. Locations can be selected on the map or queried using the Select a River Query. Predictions are made for the full particle size range of plastics studied (1 um - 100k um)"),
        tags$br(),
        fluidRow(
          column(
            width = 6,
            selectInput("river_name", "Select a river", choices = river_name, multiple = TRUE)),
          column(width = 6,
                 div(
                   style = "margin-top: 30px;", 
                   actionButton(inputId = "clear_all", label = "Clear All")
                 ))
        ),
        leafletOutput("map", width = "100%", height = "500px")
      ),
      tabItem(
        tabName = "prediction",
        h2("Predict how much plastic you will find in your river"), 
        tags$p("Change the sliders and dropdown tuning parameter options below using your sampling parameters to query the random forest model to determine how many plastic particle you may find in your sample."),
        br(),
        h2("Prediction"),
        h4(HTML("Predicted Plastic Concentration (in ppm<sup>3</sup>):")),
        uiOutput("formatted_predictedvalue"),
        br(),
        h2("Tuning Parameters"),
        fluidRow(
          column(
            width = 12,
            tags$div(
              tags$style(HTML("
                      .navbar {
                      background-color: #6D929B;
                      }
                      .breadcrumb {
                      font-size: 14px;
                      font-family: Arial, sans-serif;
                      }"))
            ),
            tags$div(
              id = "breadcrumb",
              style = "max-width: 800px; white-space: normal;",
              verbatimTextOutput("breadcrumb_output")
            )
          )
        ),
        fluidRow(
          column(width = 4,
                 sliderInput("bsldem30m_input", "Drainage Mean Slope (%)",
                             min = min(full_data$bsldem30m),
                             max = max(full_data$bsldem30m),
                             value = median(full_data$bsldem30m))),
          column(width = 4,
                 sliderInput("lc01dev_lc11dev_input", "Urban land-use (%)",
                             min = min(full_data$lc01dev_lc11dev),
                             max = max(full_data$lc01dev_lc11dev),
                             value = median(full_data$lc01dev_lc11dev))),
          column(width = 4,
                 sliderInput("x50_percent_aep_flood_input", "50% AEP Flood (m^3/s)",
                             min = min(full_data$x50_percent_aep_flood),
                             max = max(full_data$x50_percent_aep_flood),
                             value = median(full_data$x50_percent_aep_flood)))
        ),
        fluidRow(
          column(width = 4,
                 textInput("bsldem30m_text", label = NULL, value = round(median(full_data$bsldem30m)))),
          column(width = 4,
                 textInput("lc01dev_lc11dev_text", label = NULL, value = 5)),
          column(width = 4,
                 textInput("x50_percent_aep_flood_text", label = NULL, value = round(median(full_data$x50_percent_aep_flood))))),
        fluidRow(
          column(width = 4,
                 sliderInput("sample_size_input", "Sample Size (in L)",
                             min = min(full_data$sample_size),
                             max = max(full_data$sample_size),
                             value = median(full_data$sample_size))),
          column(width = 4,
                 sliderInput("filter_size_input", HTML("Smallest Particle Size (in &mu;m)"),
                             min = 1,
                             max = 100000,
                             value = 5000)),
          column(width = 4,
                 sliderInput("top_particle_input", HTML("Largest Particle Size (in &mu;m)"),
                             min = 1,
                             max = 100000,
                             value = 50000))
        ),
        fluidRow(
          column(width = 4,
                 textInput("sample_size_text", label = NULL, value = "")),
          column(width = 4,
                 textInput("filter_size_text", label = NULL, value = "")),
          column(width = 4,
                 textInput("top_particle_text", label = NULL, value = ""))),
        fluidRow(
          column(width = 4,
                 selectInput("deployment_method_input", "Sample Collection Method",
                             choices = c("Grab" = "grab", "Net" = "net"),
                             selected = "grab")),
          column(width = 4,
                 selectInput("macro_or_micro", "Particle Size Focus",
                             choices = c("Microplastics", "Macroplastics"),
                             selected = "Microplastics")),
          column(width = 4,
                 div(
                   style = "margin-top: 30px;", 
                   actionButton(inputId = "clear_filters", label = "Reset All")
                 ))
        ),
        fluidRow(
          column(
            width = 12,
            h2("Other Visuals"),
            selectInput("predictionselection", "Select a plot to observe",
                        choices = c("Actual vs. Predicted Values",
                                    "Log Transformed Actual vs. Predicted Values",
                                    "Quantile Histogram of Log-transformed Plastic Concentration",
                                    "Quantile Histogram of Log-transformed Macro vs Micro Concentration"),
                        selected = "Actual vs Predicted Values"))
        ),
        fluidRow(
          column(width = 12,
                 plotOutput("visuals"),
                 HTML("&nbsp;<br>"))
        )
      ),
      tabItem(
        tabName = "about",
        h1("Welcome to Plastic Predictions"),
        tags$h2("About using the tool"),
        tags$p("This site can be used to predict plastic concentrations is rivers based on watershed and sampling parameters.",
               "At the core we use a random forest model on river plastic data from peer reviewed studies conducted across the United States along with watershed data from the USGS.",
               "The Map of the US shows where sampling locations that are being fed to the model are. Predictions is the tunable model output."),
        tags$h2("About us"),
        tags$p("We are a group of researchers at the Moore Institute for Plastic Pollution Research (www.mooreplasticresearch.org)",
               "This app and model was developed by Hannah Sherrod and Nicholas Leong.",
               "The dataset was created by Hannah Hapich, Haig Minasian, Anja Oca, and Holden Ford.",
               "The team was led by Dr. Win Cowger and Shelly Moore."
               ), 
        tags$h2("Acknowledgements"),
        tags$p("We recieved funding from the Water PACT project of the National Renewable Energy Laboratory and the Possibility Lab from UC Berkeley"),
        tags$h2("Contact Us"),
        tags$p("Please reach out to Win at wincowger@gmail.com")
      )
    )
  )
)


server <- function(input, output, session) {
  full_data = readRDS("imputed_data.rds")
  rf_data = readRDS("rf_model.rds")
  
  # Create reactive values to store x1D, x2D, and correction factor
  reactive_values <- reactiveValues(
    x1D = 1,
    x2D = 100000,
    correction_factor = NULL,
    predictions = NULL
  )
  
  # Update x1D and x2D based on slider input
  observe({
    reactive_values$x1D <- input$filter_size_input
    reactive_values$x2D <- input$top_particle_input
  })
  
  # Calculate correction factor
  observe({
    x1D_set <- reactive_values$x1D
    x2D_set <- reactive_values$x2D
    
    alpha <- 2.65 
    
    CFfnx = function(a, x2D, x1D, x2M, x1M){
      CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
      return(CF)
    }
    
    CF <- CFfnx(
      x1M = 1,
      x2M = 100000,
      x1D = x1D_set,
      x2D = x2D_set,
      a = alpha
    )
    reactive_values$correction_factor <- CF
    
  })
  
  filtered_breadcrumb <- reactive({
    bsldem30m <- input$bsldem30m_input
    lc01dev_lc11dev <- input$lc01dev_lc11dev_input
    x50_percent_aep_flood <- input$x50_percent_aep_flood_input
    sample_size <- input$sample_size_input
    top_particle <- input$top_particle_input
    filter_size <- input$filter_size_input
    deployment_method <- input$deployment_method_input
    macro_or_micro <- input$macro_or_micro
    
    # Create a list of variable names and values
    filters <- list(
      "Drainage Mean Slope" = bsldem30m,
      "Percentage of urban land-use" = lc01dev_lc11dev,
      "50% AEP Flood" = x50_percent_aep_flood,
      "Sample Size" = sample_size,
      "Top Particle Size" = top_particle,
      "Smallest Particle Size" = filter_size,
      "Deployment Method" = deployment_method,
      "Plastic Type" = macro_or_micro
    )
    
    # Filter out empty values and create a formatted breadcrumb text
    breadcrumb_text <- paste("Filters >", 
                             paste(names(filters)[!sapply(filters, is.null)],
                                   unlist(filters)[!sapply(filters, is.null)],
                                   sep = ": ", collapse = " > "),
                             sep = " ")
    
    breadcrumb_text
  })
  
  output$breadcrumb_output <- renderText({
    breadcrumb_text <- filtered_breadcrumb()
    breadcrumb_text
  })
  
  full_data = readRDS("imputed_data.rds")
  rf_data <- readRDS("rf_model.rds")
  map_data = readRDS("map_data.rds")
  
  # Create a two-way binding function with validation
  two_way_binding <- function(slider_input, textbox_input, min_value, max_value) {
    observe({
      req(input[[textbox_input]])
      textbox_value <- as.numeric(input[[textbox_input]])
      if (textbox_value > max_value || textbox_value < min_value) {
        showModal(modalDialog(
          title = "Error",
          paste("Textbox value should be between", min_value, "and", max_value),
          easyClose = TRUE
        ))
      } else {
        updateSliderInput(session, slider_input, value = textbox_value)
      }
    })
    
    observe({
      req(input[[slider_input]])
      slider_value <- input[[slider_input]]
      updateTextInput(session, textbox_input, value = slider_value)
    })
  }
  
  # Set up two-way binding for each variable
  two_way_binding("bsldem30m_input", "bsldem30m_text",
                  min = min(full_data$bsldem30m),
                  max = max(full_data$bsldem30m))
  two_way_binding("lc01dev_lc11dev_input", "lc01dev_lc11dev_text",min = min(full_data$lc01dev_lc11dev),
                  max = max(full_data$lc01dev_lc11dev))
  two_way_binding("x50_percent_aep_flood_input", "x50_percent_aep_flood_text",
                  min = min(full_data$x50_percent_aep_flood),
                  max = max(full_data$x50_percent_aep_flood))
  two_way_binding("sample_size_input", "sample_size_text",
                  min = min(full_data$sample_size),
                  max = max(full_data$sample_size))
  two_way_binding("top_particle_input", "top_particle_text",
                  min = 1,
                  max = 100000)
  two_way_binding("filter_size_input", "filter_size_text",
                  min = 1,
                  max = 100000)
  
  # Add an observer to clear filters
  observeEvent(input$clear_filters, {
    # Update all filter inputs to their default values or suitable values
    updateSliderInput(session, "bsldem30m_input", value = median(full_data$bsldem30m))
    updateSliderInput(session, "lc01dev_lc11dev_input", value = median(full_data$lc01dev_lc11dev))
    updateSliderInput(session, "x50_percent_aep_flood_input", value = median(full_data$x50_percent_aep_flood))
    updateSliderInput(session, "sample_size_input", value = median(full_data$sample_size))
    updateSliderInput(session, "top_particle_input", value = 50000)
    updateSliderInput(session, "filter_size_input", value = 5000)
  })
  
  # Reactive expression for predicting the target variable
  output$formatted_predictedvalue <- renderUI({
    selected_data <- data.frame(
      bsldem30m = input$bsldem30m_input,
      lc01dev_lc11dev = input$lc01dev_lc11dev_input,
      x50_percent_aep_flood = input$x50_percent_aep_flood_input,
      deployment_method = input$deployment_method_input,
      sample_size = input$sample_size_input,
      top_particle = input$top_particle_input,
      filter_size = input$filter_size_input,
      macro_or_micro = input$macro_or_micro
    )
    
    # Print statements to debug
    print("Selected Data:")
    print(head(selected_data))
    
    # Predict using the random forest model
    predicted <- 10^predict(rf_data, selected_data) * as.numeric(reactive_values$correction_factor)
    
    # Round predicted values to four significant figures
    predicted <- signif(predicted, 4)
    
    # Print the predicted values
    print("Predicted Values:")
    print(head(predicted))
    
    # Create the formatted output with styling
    formatted_output <- shiny::tags$div(
      shiny::tags$span(
        style = "font-size: 40px; color: red;",
        predicted
      )
    )
    
    # Return the formatted output
    formatted_output
  })
  
  # Log transform predicted value
  output$formatted_logpredictedvalue <- renderUI({
    selected_data <- data.frame(
      bsldem30m = input$bsldem30m_input,
      lc01dev_lc11dev = input$lc01dev_lc11dev_input,
      x50_percent_aep_flood = input$x50_percent_aep_flood_input,
      deployment_method = input$deployment_method_input,
      sample_size = input$sample_size_input,
      top_particle = input$top_particle_input,
      filter_size = input$filter_size_input,
      macro_or_micro = input$macro_or_micro
    )
    
    # Predict using the random forest model
    predictedlog <- 10^predict(rf_data, selected_data) * as.numeric(reactive_values$correction_factor)
    
    # Calculate the log value after rounding
    predictedlog <- log10(predictedlog)
    
    predictedlog <- round(predictedlog, digits = 4)
    
    # Create the formatted output with styling
    formatted_output2 <- shiny::tags$div(
      shiny::tags$span(
        style = "font-size: 24px",
        predictedlog
      )
    )
    
    # Return the formatted output
    formatted_output2
  })
  
  # Reactive expression for generating line plots comparing actual and predicted values
  output$visuals <- renderPlot({
    selected_data <- data.frame(
      bsldem30m = input$bsldem30m_input,
      lc01dev_lc11dev = input$lc01dev_lc11dev_input,
      x50_percent_aep_flood = input$x50_percent_aep_flood_input,
      deployment_method = input$deployment_method_input,
      sample_size = input$sample_size_input,
      top_particle = input$top_particle_input,
      filter_size = input$filter_size_input,
      macro_or_micro = input$macro_or_micro
    )
    
    # Predict using the random forest model
    predicted <- 10^predict(rf_data, selected_data) * as.numeric(reactive_values$correction_factor)
    
    actual <- full_data$corrected_concentration
    
    if (input$predictionselection == "Actual vs. Predicted Values") { 
      p <- ggplot(full_data, aes(x = corrected_concentration)) +
        geom_density(aes(color = "Actual"), alpha = 0.5, show.legend = FALSE, size = 1.5) +
        geom_vline(aes(xintercept = predicted, color = "Predicted"), linetype = "dashed") +
        labs(x = paste("Predicted =", round(predicted, 2), "ppm³"),
             y = "Density") +
        scale_x_log10() +
        scale_color_manual(values = c("Actual" = "#CD5C5C", "Predicted" = "#5c80cd")) +
        theme_minimal() +
        theme(axis.title.x = element_text(size = 14))  # Adjust the size as needed
      
      p
      
    } else if (input$predictionselection == "Log Transformed Actual vs. Predicted Values") { 
      lp <- ggplot(full_data, aes(x = log10(corrected_concentration))) +
        geom_density(aes(color = "Actual"), alpha = 0.5, show.legend = FALSE, size = 1.5) +
        geom_vline(aes(xintercept = log10(predicted), color = "Predicted"), linetype = "dashed") + 
        labs(x = paste("Predicted =", round(log10(predicted), 2), "ppm³"),
             y = "Density") +
        scale_x_log10() +
        scale_color_manual(values = c("Actual" = "#CD5C5C", "Predicted" = "#5c80cd")) +  # Set custom colors
        theme_minimal() + 
        theme(axis.title.x = element_text(size = 14))  # Adjust the size as needed
      
      lp
      
    } else if (input$predictionselection == "Quantile Histogram of Log-transformed Plastic Concentration") {
      
      ### Filter out negative values from the concentration data to calculate quantiles; modify code once we get entire data
      positive_concentration <- full_data$corrected_concentration[full_data$corrected_concentration > 0]
      log_concentration <- log10(positive_concentration)
      quantile_data <- data.frame(
        quantiles <- quantile(log_concentration, probs = c(0.1, 0.5, 0.9)),
        Quantile <- c("10% Quantile", "50% Quantile", "90% Quantile")
      )
      
      
      h <- ggplot(full_data, aes(x = log10(corrected_concentration))) +
        geom_histogram(binwidth = 0.1, fill = "#CCE5FF", alpha = 0.7) +
        geom_vline(data = quantile_data, aes(xintercept = quantiles, color = Quantile), linetype = "dashed") +
        geom_vline(xintercept = log10(predicted), color = "black", linetype = "solid") +
        labs(
          x = "Log-transformed Concentration Data",
          y = "Frequency") +
        theme_minimal() +
        scale_color_manual(values = c("10% Quantile" = "#CD5C5C", "50% Quantile" = "#2E8B57", "90% Quantile" = "#8A2bE2")) +
        scale_y_continuous(labels = scales::comma_format())
      
      h
      
    } else if (input$predictionselection == "Quantile Histogram of Log-transformed Macro vs Micro Concentration") {
      # Reactive expression for generating histograms comparing actual and predicted values
      # Predict using the random forest model
      positive_concentration <- full_data$corrected_concentration[full_data$corrected_concentration > 0]
      log_concentration <- log10(positive_concentration)
      quantile_data <- data.frame(
        quantiles <- quantile(log_concentration, probs = c(0.1, 0.5, 0.9)),
        Quantile <- c("10% Quantile", "50% Quantile", "90% Quantile")
      )
      
      m <- ggplot(full_data, aes(x = log10(corrected_concentration), fill = macro_or_micro)) +
        geom_histogram(data = subset(full_data, macro_or_micro == "Macroplastics"), binwidth = 0.1, alpha = 0.7, position = "identity") +
        geom_histogram(data = subset(full_data, macro_or_micro == "Microplastics"), binwidth = 0.1, alpha = 0.7, position = "identity") +
        geom_vline(data = quantile_data, aes(xintercept = quantiles, color = Quantile), linetype = "dashed") +
        geom_vline(xintercept = log10(predicted), color = "black", linetype = "solid") +
        labs(
          x = "Log-transformed Concentration Data",
          y = "Frequency",
          fill = "Macro or Micro"
        ) +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma_format()) +
        scale_color_manual(values = c("10% Quantile" = "#CD5C5C", "50% Quantile" = "#2E8B57", "90% Quantile" = "#8A2bE2")) +
        scale_fill_manual(values = c("Macroplastics" = "#FFA500", "Microplastics" = "#CCE5FF"))
      
      m
    }
  })
  
  ### Map Tab
  filtered_data <- reactive({
    selected_rivers <- input$river_name
    if (is.null(selected_rivers) || length(selected_rivers) == 0) {
      return(map_data)  # If no rivers are selected, show all points
    } else {
      return(filter(map_data, river_name %in% selected_rivers))
    }
  })
  
  observeEvent(input$clear_all, {
    updateSelectInput(session, "river_name", selected = character(0))
  })
  
  YlOrRd <- colorFactor("YlOrRd", domain = map_data$corrected_concentration)
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~YlOrRd(corrected_concentration),
        fillOpacity = 0.5,
        radius = 6,
        popup = ~paste("<b>River Name:</b> ", river_name, 
                       "<br><b>Plastic Concentration (in ppm³):</b> ", corrected_concentration,
                       "<br><b>Coordinates:</b> ", paste(latitude, longitude, sep = ", ")
        ),
        label = ~river_name
      )
  })
}

shinyApp(ui, server)

