# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library('stringr')
library('dplyr')

# Loading Amazon Dataset
Amazon_Sale_Report <- read_excel("/Users/danielorrego/Desktop/Classes/ISTA 498/multiple models dataset.xlsx")
ASR <- Amazon_Sale_Report

ASR <- ASR |>
  mutate(discount = as.numeric(str_remove(discount, "%")))

unique(ASR$Status)

ASR$Status <- ifelse(ASR$Status == 'Shipped - Returned to Seller', 'Returned', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Returning to Seller', 'Returned', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Rejected by Buyer', 'Returned', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Cancelled', 'Returned', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Damaged', 'Returned', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipping', 'Shipped', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Delivered to Buyer', 'Shipped', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Picked Up', 'Shipped', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Pending', 'Shipped', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Pending - Waiting for Pick Up', 'Shipped', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Lost in Transit', 'Shipped', ASR$Status)
ASR$Status <- ifelse(ASR$Status == 'Shipped - Out for Delivery', 'Shipped', ASR$Status)

unique(ASR$Status)

ASR <- ASR %>%
  select(-`ship_service_level`, -`Courier Status`, -`ship_city`, -`ship_state`, -`ship_country`)

unique(ASR$`promotion_ids`)
summary(ASR)

ASR$`promotion_ids` <- str_replace_all(ASR$`promotion_ids`, 'Amazon PLCC Free-Financing Universal Merchant AAT','Yes')
ASR$`promotion_ids` <- str_replace_all(ASR$`promotion_ids`, 'IN Core Free Shipping','Yes')
ASR$`promotion_ids` <- str_replace_all(ASR$`promotion_ids`, 'VPC-44571 Coupon','Yes')

ASR$`promotion_ids` <- ASR$`promotion_ids` %>% replace_na('No')

unique(ASR$`promotion_ids`)

# Implementing different linear regression models m1 to m6

# model m1
m1 <- lm(Amount ~ discounted_price + rating, data = ASR)
summary(m1)
m1_summary <- summary(m1)
m1_summary$r.squared
b1 <- m1_summary$coefficients[2,1]
b1_se <- m1_summary$coefficients[2,2]

b1_upper_ci <- b1 + 1.98*b1_se
b1_lower_ci <- b1- 1.98*b1_se

confint(m1)

b0 <- m1_summary$coefficients[1,1]
b1 <- m1_summary$coefficients[2,1]

b0 + b1*68 

x_val <- seq(from = min(ASR$Amount), to = max(ASR$Amount), length.out  = nrow(ASR))

y_pred <- b0 + b1*x_val

# model m2
m2 <- lm(discount ~ rating, data = ASR)
m2_summary <- summary(m2)

# model m3
m3 <- lm(discount ~ rating + discounted_price + Qty, data = ASR)
m3_summary <- summary(m3)

# model m4
ASR$Savings <- (ASR$Amount - ASR$discounted_price)
m4 <- lm(Savings ~ Amount + rating + rating_count + Qty + discounted_price, data = ASR)
m4_summary <- summary(m4)

# model m5
m5 <- lm(Savings ~ rating, data = ASR)
summary(m5)

# model m6
m6 <- lm(Qty ~ discount*Amount, data = ASR)
m6_summary <- summary(m6)
m6_coefs <- m6_summary$coefficients

# Define UI using dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Sales Analysis"),
  dashboardSidebar(
    selectInput("variable", "Choose Variable", choices = c("Amount", "Savings", "Qty", "discounted_price", "rating")),
    selectInput("model", "Choose Model", choices = c("m1", "m2", "m3", "m4", "m5", "m6")),
    actionButton("update", "Update Plot")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Sales Performance",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("salesPlot")
      ),
      box(
        title = "Model Information",
        status = "primary",
        solidHeader = TRUE,
        textOutput("modelInfo")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive expression/ changes with variable/ filters data
  filtered_data <- reactive({
    selected_data <- ASR %>%
      select("Amount", "Savings", "Qty", "discounted_price", "rating", input$variable)
    
  })
  
  # Update plot, pick which model
  observeEvent(input$update, {
    output$salesPlot <- renderPlot({
      model <- switch(input$model,"m1" = m1, "m2" = m2, "m3" = m3, "m4" = m4, "m5" = m5, "m6" = m6)
      
      
      ggplot(filtered_data(), aes_string(x = input$variable, y = "Amount")) +
        geom_point() +
        labs(title = paste("Sales Performance vs.", input$variable),
             x = input$variable, y = "Amount")  + geom_smooth(method = "lm", formula = y ~ x, data = as.data.frame(filtered_data()), color = "red")
    })
    
    # Shows the model info for each lm
    output$modelInfo <- renderText({
      switch(input$model,
             "m1" = paste("Model: m1", "\nR-squared:", round(summary(m1)$r.squared, 3), "\nP-value:", round(summary(m1)$coefficients[2, 4], 3)),
             "m2" = paste("Model: m2", "\nR-squared:", round(summary(m2)$r.squared, 3), "\nP-value:", round(summary(m2)$coefficients[2, 4], 3)),
             "m3" = paste("Model: m3", "\nR-squared:", round(summary(m3)$r.squared, 3), "\nP-value:", round(summary(m3)$coefficients[2, 4], 3)),
             "m4" = paste("Model: m4", "\nR-squared:", round(summary(m4)$r.squared, 3), "\nP-value:", round(summary(m4)$coefficients[2, 4], 3)),
             "m5" = paste("Model: m5", "\nR-squared:", round(summary(m5)$r.squared, 3), "\nP-value:", round(summary(m5)$coefficients[2, 4], 3)),
             "m6" = paste("Model: m6", "\nR-squared:", round(summary(m6)$r.squared, 3), "\nP-value:", round(summary(m6)$coefficients[2, 4], 3))
      )
    })
  })
}

# Run the app
shinyApp(ui, server)
