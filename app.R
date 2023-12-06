# Linear Regression Model

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)

# Load data
Amazon_Sale_Report <- read_excel("~/Greg's School/ISTA 498/Amazon Sale Report.xlsx")
ASR <- Amazon_Sale_Report

ASR$`promotion_ids` <- str_replace_all(ASR$`promotion_ids`, 'Amazon PLCC Free-Financing Universal Merchant AAT','Yes')
ASR$`promotion_ids` <- str_replace_all(ASR$`promotion_ids`, 'IN Core Free Shipping','Yes')
ASR$`promotion_ids` <- str_replace_all(ASR$`promotion_ids`, 'VPC-44571 Coupon','Yes')

m1 <- lm(Amount ~ discounted_price + rating, data = ASR)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Template for Variables"),
  dashboardSidebar(
    selectInput("variable", "Variable", choices = c("discounted_price", "rating","rating_count","Qty","Savings" )),
    actionButton("update", "Update Plot")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Test",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("salesPlot")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    ASR %>%
      select("Amount", input$variable)
  })
  
  observeEvent(input$update, {
  
    updated_model <- lm(formula = as.formula(paste("Amount ~", input$variable)), data = ASR)
    
 
    x_val <- seq(from = min(ASR[[input$variable]]), to = max(ASR[[input$variable]]), length.out = nrow(ASR))
    

    y_pred <- predict(updated_model, newdata = data.frame(filtered_data()))
    
    # Plot
    output$salesPlot <- renderPlot({
      ggplot(ASR, aes_string(x = input$variable, y = "Amount")) +
        geom_point() +
        geom_line(aes(x = x_val, y = y_pred), color = "blue") +
        labs(title = paste("Sales Performance vs.", input$variable),
             x = input$variable, y = "Amount")
    })
  })
}


# Run the application
shinyApp(ui, server)
