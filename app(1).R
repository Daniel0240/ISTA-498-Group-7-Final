# MultiLinear Regression Model

library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(tidymodels)

Amazon_Sale_Report <- read_excel("~/Greg's School/ISTA 498/Amazon Sale Report.xlsx")
ASR <- Amazon_Sale_Report

ui <- dashboardPage(
  dashboardHeader(title = "Multilinear Regression Model" ,titleWidth = 400),
  dashboardSidebar(
    selectInput("variable1", "Variable 1", choices = colnames(ASR)),
    selectInput("variable2", "Variable 2", choices = colnames(ASR)),
    selectInput("variable3", "Variable 3", choices = colnames(ASR)),
    actionButton("update", "Update Model")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Sales Forecast Analysis Model",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("salesPlot")
      ),
      box(
        title = "P-Values and Variance",
        status = "primary",
        solidHeader = TRUE,
        textOutput("pValuesOutput"),
        textOutput("varianceOutput"),
        textOutput("pValueMessage"),
        textOutput("varianceMessage")
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    ASR %>%
      select("Amount", input$variable1, input$variable2, input$variable3)
  })
  
  observeEvent(input$update, {
    output$salesPlot <- renderPlot({
      
      model <- lm(Amount ~ ., data = filtered_data())
      
      predictions <- predict(model, filtered_data())
      
      ggplot(filtered_data(), aes(x = predictions, y = Amount)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = "Multilinear Regression Plot",
             x = "Predicted Sales Amount",
             y = "Actual Sales Amount")
    })
    
  
    summary_data <- summary(lm(Amount ~ ., data = filtered_data()))
    p_values <- summary_data$coefficients[, 4]
    variance <- summary_data$sigma
    
    output$pValuesOutput <- renderText({
      paste("P-values: ", paste(round(p_values, 4), collapse = ", "))
    })
    
    output$varianceOutput <- renderText({
      paste("Variance: ", round(variance, 4))
    })
    
  
    output$pValueMessage <- renderText({
      if (all(p_values < 0.05)) {
        "The p-values = Good"
      } else {
        "P-values are poor, try other variables for better significance."
      }
    })
    

    output$varianceMessage <- renderText({
      if (variance < 0.1) {
        "Variance = Good"
      } else {
        "Variance is poor, check model fit."
      }
    })
  })
}

shinyApp(ui, server)
