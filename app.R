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

rf_data = readRDS("rf_model.rds")
full_data = readRDS("full_data.rds")

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
          column(width = 6,
                 sliderInput("bsldem30m_input", "Drainage Mean Slope",
                             min = min(full_data$bsldem30m),
                             max = max(full_data$bsldem30m),
                             value = median(full_data$bsldem30m))),
        column(width = 6,
               h3("Predict Imputed Standardized Data"),
               verbatimTextOutput("predictedvalue"))
        ),
        fluidRow(
          column(width = 6,
                 sliderInput("lc01dev_lc11dev_input", "Percentage of land-use",
                             min = min(full_data$lc01dev_lc11dev),
                             max = max(full_data$lc01dev_lc11dev),
                             value = median(full_data$lc01dev_lc11dev)))
        ),
        fluidRow(
          column(width = 6,
                 sliderInput("x50_percent_aep_flood_input", "50% AEP Flood",
                             min = min(full_data$x50_percent_aep_flood),
                             max = max(full_data$x50_percent_aep_flood),
                             value = median(full_data$x50_percent_aep_flood)))
        ),
        
        fluidRow(
          column(width = 6,
                 selectInput("deployment_method_input", "Deployment Method",
                             choices = c("grab", "net"),
                             selected = "grab")
          )
        ),
        fluidRow(
          column(width = 6,
                 sliderInput("sample_size_input", "Sample Size (in L)",
                             min = min(full_data$sample_size),
                             max = max(full_data$sample_size),
                             value = median(full_data$sample_size)))
        ),
        
        fluidRow(
          column(width = 6,
                 sliderInput("top_particle_input", "Largest Particle Size",
                             min = min(full_data$top_particle),
                             max = max(full_data$top_particle),
                             value = median(full_data$top_particle)))
        ),fluidRow(
          column(width = 6,
                 sliderInput("filter_size_input", "Smallest Particle Size Based on Filter",
                             min = min(full_data$filter_size),
                             max = max(full_data$filter_size),
                             value = median(full_data$filter_size)))
          )
        )
      )
    )
  )


server <- function(input, output, session) {
  full_data <- readRDS("full_data.rds")
  rf_data <- readRDS("rf_model.rds")
  
  # Reactive expression for generating the scatter plot
  output$scatterplot <- renderPlot({
    ggplot(full_data, aes(x = filter_size, y = imputed_standardized_data)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Smoothed Scatter Plot: Imputed Standardized Data vs. Filter Size",
           x = "Filter Size",
           y = "Imputed Standardized Data") +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal()
  })
  
  # Reactive expression for generating the density plot
  output$densityplot <- renderPlot({
    ggplot(full_data, aes(x = imputed_standardized_data, fill = deployment_method)) +
      geom_density(alpha = 0.5) +
      labs(title = "Density Plot of Imputed Standardized Data by Deployment Method",
           x = "Imputed Standardized Data",
           y = "Density") +
      scale_x_log10() +  # Add logarithmic scale to the x-axis
      theme_minimal()
  })
  
  # Reactive expression for predicting the target variable
  output$predictedvalue <- renderText({
    selected_data <- data.frame(
      bsldem30m = input$bsldem30m_input,
      lc01dev_lc11dev = input$lc01dev_lc11dev_input,
      x50_percent_aep_flood = input$x50_percent_aep_flood_input,
      deployment_method = input$deployment_method_input,
      sample_size = input$sample_size_input,
      top_particle = input$top_particle_input,
      filter_size = input$filter_size_input
    )
    
    # Predict using the random forest model
    predicted <- predict(rf_data, newdata = selected_data)
    
    paste("Predicted Value: ", predicted)
  })
}

shinyApp(ui, server)

