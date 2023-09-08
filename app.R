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

rf_data = readRDS("rf_model.rds")
dataframeclean = readRDS("imputed_data.rds")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Water Basin Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("home")),
      menuItem("Predictions", tabName = "prediction", icon = icon("sliders-h"))
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
        tabName = "introduction",
        h2("Welcome to our app!"),
        p("This app uses data from the USGS Streamstats website."),
        fluidRow(
          column(width = 6,
                 h3("Scatter Plot"),
                 plotOutput("scatterplot")),
          column(width = 6,
                 h3("Density Plot"),
                 plotOutput("densityplot"))
        )
      ),
      tabItem(
        tabName = "prediction",
        fluidRow(
          column(
          width = 12,
          h3("Other Visuals"),
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
        ),
        fluidRow(
          column(width = 9,
                 h3(HTML("Predicted Microplastic Concentration (in ppm<sup>3</sup>):"))),
          column(width = 3,
                 uiOutput("formatted_predictedvalue"))
        ),
        fluidRow(
          column(width = 9,
                 h3(HTML("Log Transformed Microplastic Concentration: "))),
          column(width = 3,
                 uiOutput("formatted_logpredictedvalue"),
                 HTML("&nbsp;<br>"))
        ),
        fluidRow(
          column(width = 4,
                 sliderInput("bsldem30m_input", "Drainage Mean Slope",
                             min = min(dataframeclean$bsldem30m),
                             max = max(dataframeclean$bsldem30m),
                             value = median(dataframeclean$bsldem30m))),
          column(width = 4,
                 sliderInput("lc01dev_lc11dev_input", "Percentage of land-use",
                             min = min(dataframeclean$lc01dev_lc11dev),
                             max = max(dataframeclean$lc01dev_lc11dev),
                             value = median(dataframeclean$lc01dev_lc11dev))),
          column(width = 4,
                 sliderInput("x50_percent_aep_flood_input", "50% AEP Flood",
                             min = min(dataframeclean$x50_percent_aep_flood),
                             max = max(dataframeclean$x50_percent_aep_flood),
                             value = median(dataframeclean$x50_percent_aep_flood))),
        ),
        fluidRow(
          column(width = 4,
                 textInput("bsldem30m_text", label = NULL, value = "")),
          column(width = 4,
                 textInput("lc01dev_lc11dev_text", label = NULL, value = "")),
          column(width = 4,
                 textInput("x50_percent_aep_flood_text", label = NULL, value = ""))),
        fluidRow(
          column(width = 4,
                 sliderInput("sample_size_input", "Sample Size (in L)",
                             min = min(dataframeclean$sample_size),
                             max = max(dataframeclean$sample_size),
                             value = median(dataframeclean$sample_size))),
          column(width = 4,
                 sliderInput("top_particle_input", HTML("Largest Particle Size (in &mu;m)"),
                             min = min(dataframeclean$top_particle),
                             max = max(dataframeclean$top_particle),
                             value = median(dataframeclean$top_particle))),
          column(width = 4,
                 sliderInput("filter_size_input", HTML("Smallest Particle Size Based on Filter (in &mu;m)"),
                             min = min(dataframeclean$filter_size),
                             max = max(dataframeclean$filter_size),
                             value = median(dataframeclean$filter_size)))
        ),
        fluidRow(
          column(width = 4,
                 textInput("sample_size_text", label = NULL, value = "")),
          column(width = 4,
                 textInput("top_particle_text", label = NULL, value = "")),
          column(width = 4,
                 textInput("filter_size_text", label = NULL, value = ""))),
        fluidRow(
          column(width = 4,
                 selectInput("deployment_method_input", "Deployment Method",
                             choices = c("grab", "net"),
                             selected = "grab"))
        ),
        fluidRow(
          column(width = 4,
                 selectInput("macro_or_micro", "Plastic Type",
                             choices = c("Microplastics", "Macroplastics"),
                             selected = "Microplastics"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataframeclean = readRDS("dataframeclean.rds")
  rf_data <- readRDS("rf_model.rds")
  
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
                  min = min(dataframeclean$bsldem30m),
                  max = max(dataframeclean$bsldem30m))
  two_way_binding("lc01dev_lc11dev_input", "lc01dev_lc11dev_text",min = min(dataframeclean$lc01dev_lc11dev),
                  max = max(dataframeclean$lc01dev_lc11dev))
  two_way_binding("x50_percent_aep_flood_input", "x50_percent_aep_flood_text",
                  min = min(dataframeclean$x50_percent_aep_flood),
                  max = max(dataframeclean$x50_percent_aep_flood))
  two_way_binding("sample_size_input", "sample_size_text",
                  min = min(dataframeclean$sample_size),
                  max = max(dataframeclean$sample_size))
  two_way_binding("top_particle_input", "top_particle_text",
                  min = min(dataframeclean$top_particle),
                  max = max(dataframeclean$top_particle))
  two_way_binding("filter_size_input", "filter_size_text",
                  min = min(dataframeclean$filter_size),
                  max = max(dataframeclean$filter_size))
  
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
    
    # Predict using the random forest model
    predicted <- predict(rf_data, selected_data)
    
    # Create the formatted output with styling
    formatted_output <- shiny::tags$div(
      shiny::tags$span(
        style = "font-size: 24px",
        10^predicted
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
    predictedlog <- predict(rf_data, selected_data)
    
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
      macro_or_micro = input$macro_or_micro,
      corrected_concentration = input$corrected_concentration
    )
    
    # Predict using the random forest model
    predicted <- predict(rf_data, newdata = selected_data)
    
    actual <- dataframeclean$corrected_concentration
    
    if (input$predictionselection == "Actual vs. Predicted Values") { 
      p <- ggplot(dataframeclean, aes(x = corrected_concentration)) +
        geom_density(aes(color = "Actual"), alpha = 0.5, show.legend = FALSE) +
        geom_vline(aes(xintercept = 10^predicted, color = "Predicted")) +
        labs(x = paste("Predicted =", round(10^predicted, 2), "ppm³"),
             y = "Density") +
        scale_x_log10() +
        scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +
        theme_minimal() +
        theme(axis.title.x = element_text(size = 14))  # Adjust the size as needed
      
      p
      
    } else if (input$predictionselection == "Log Transformed Actual vs. Predicted Values") { 
      lp <- ggplot(dataframeclean, aes(x = log10(corrected_concentration))) +
        geom_density(aes(color = "Actual"), alpha = 0.5, show.legend = FALSE) +
        geom_vline(aes(xintercept = predicted, color = "Predicted")) + 
        labs(x = paste("Predicted =", round(predicted, 2), "ppm³"),
             y = "Density") +
        scale_x_log10() +
        scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue")) +  # Set custom colors
        theme_minimal() + 
        theme(axis.title.x = element_text(size = 14))  # Adjust the size as needed
      
      lp
    
  } else if (input$predictionselection == "Quantile Histogram of Log-transformed Plastic Concentration") {
    # Reactive expression for generating histograms comparing actual and predicted values
    # Predict using the random forest model
    quantiles <- quantile(log10(dataframeclean$corrected_concentration), probs = c(0.1, 0.5, 0.9))
    
    h <- ggplot(dataframeclean, aes(x = log10(corrected_concentration))) +
      geom_histogram(binwidth = 0.1, fill = "#CCE5FF", alpha = 0.7) +
      geom_vline(xintercept = quantiles, color = c("#CD5C5C", "#2E8B57", "#8a2be2"), linetype = "dashed") +
      geom_vline(xintercept = predicted, color = "black", linetype = "solid") +
      labs(
           x = "Log-transformed Concentration Data",
           y = "Frequency") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma_format())
    
    h
    
  } else if (input$predictionselection == "Quantile Histogram of Log-transformed Macro vs Micro Concentration") {
    # Reactive expression for generating histograms comparing actual and predicted values
    # Predict using the random forest model
    quantiles <- quantile(log10(dataframeclean$corrected_concentration), probs = c(0.1, 0.5, 0.9))
    
    m <- ggplot(dataframeclean, aes(x = log10(corrected_concentration), fill = macro_or_micro)) +
      geom_histogram(data = subset(dataframeclean, macro_or_micro == "Macroplastics"), binwidth = 0.1, alpha = 0.7, position = "identity") +
      geom_histogram(data = subset(dataframeclean, macro_or_micro == "Microplastics"), binwidth = 0.1, alpha = 0.7, position = "identity") +
      geom_vline(xintercept = quantiles, color = c("#CD5C5C", "#2E8B57", "#8a2be2"), linetype = "dashed") +
      geom_vline(xintercept = predicted, color = "black", linetype = "solid") +
      labs(
        x = "Log-transformed Concentration Data",
        y = "Frequency",
        fill = "Macro or Micro"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_fill_manual(values = c("Macroplastics" = "#FFA500", "Microplastics" = "#CCE5FF"))
    
    m
  }
  })
  
  # Reactive expression for generating the scatter plot
  output$scatterplot <- renderPlot({
    ggplot(dataframeclean, aes(x = filter_size, y = corrected_concentration)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Smoothed Scatter Plot: Corrected Concentration Data vs. Filter Size",
           x = "Filter Size",
           y = "Corrected Concentration") +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal()
  })
  
  # Reactive expression for generating the density plot
  output$densityplot <- renderPlot({
    ggplot(dataframeclean, aes(x = corrected_concentration, fill = deployment_method)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density Plot of Corrected Concentration Data by Deployment Method",
           x = "Corrected Concentration",
           y = "Density",
           fill = "Deployment Method") +
      scale_x_log10() +  # Add logarithmic scale to the x-axis
      scale_fill_manual(values = c("#1f7872", "#f1948a"))
  })
}

shinyApp(ui, server)

