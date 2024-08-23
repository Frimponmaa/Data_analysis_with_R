#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("plotly")

# Load required libraries 
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)

# Load data using a relative path
data <- read.csv("data/TelcoCustomeChurn.csv")

# Exclude specific columns
data <- data %>% select(-customerID)  # Excludes 'CustomerID'

churn_data <- data
# Convert necessary variables to appropriate data types
churn_data$SeniorCitizen <- as.factor(churn_data$SeniorCitizen)
churn_data$tenure <- as.numeric(churn_data$tenure)
churn_data$MonthlyCharges <- as.numeric(churn_data$MonthlyCharges)
churn_data$TotalCharges <- as.numeric(as.character(churn_data$TotalCharges)) # Convert TotalCharges to numeric
churn_data$Churn <- as.factor(churn_data$Churn)


# Define UI for the application
ui <- fluidPage(
  titlePanel("Customer Churn Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Key Variable:", 
                  choices = setdiff(names(churn_data), "Churn"), 
                  selected = "tenure"),
      
      selectInput("yvar", "Key Metric:", 
                  choices = c("Churn Rate (%)", "Churn Count", "Average Value"), 
                  selected = "Churn Rate (%)")
    ),
    
    mainPanel(
      plotlyOutput("churnPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$churnPlot <- renderPlotly({
    # Prepare data based on user selections
    if (input$yvar == "Churn Rate (%)") {
      plot_data <- churn_data %>%
        group_by_at(input$xvar) %>%
        summarize(ChurnRate = mean(Churn == "Yes") * 100)
      
      
      print(plot_data)  # Check if ChurnRate exceeds 100%
      
      
      y_label <- "Churn Rate (%)"
    } else if (input$yvar == "Churn Count") {
      plot_data <- churn_data %>%
        group_by_at(input$xvar) %>%
        summarize(ChurnCount = sum(Churn == "Yes"))
      
      y_label <- "Churn Count"
    } else if (input$yvar == "Average Value") {
      # Ensure xvar is a numeric variable
      plot_data <- churn_data %>%
        group_by_at(input$xvar) %>%
        summarize(AverageValue = mean(as.numeric(as.character(churn_data[[input$xvar]])), na.rm = TRUE))
      
      y_label <- "Average Value"
    }
    
    # Create plot with churn legend
    p <- ggplot(churn_data, aes_string(x = input$xvar, fill = "Churn")) +
      geom_bar(stat = "count", position = "dodge") +
      scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
      theme_minimal() +
      labs(x = input$xvar, y = y_label, title = paste("Customer Churn Analysis -", input$yvar), fill = "Churn Status") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)