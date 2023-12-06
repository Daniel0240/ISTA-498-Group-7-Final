#Namig Alakbarzade
#Senior CAPSTONE
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Reading the CSV file
amazon_data <- read.csv("defined_amazon.csv")

# Convert RatingCount to integers
amazon_data$RatingCount <- as.integer(amazon_data$RatingCount)

# Fit a linear regression model
model <- lm(Rating ~ RatingCount, data = amazon_data)

# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Apply a colorful theme
  titlePanel("Product Rating Prediction", windowTitle = "Product Rating"),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "background-color: beige; padding: 10px;",
        textOutput("prediction_result")  # Output element for prediction result
      ),
      numericInput("rating_count", "Rating Count:", value = 1000, min = 0),
      actionButton("predict_btn", "Predict", class = "btn-success"),
      hr(),
      downloadButton("save_plots_btn", "Save Plots as PNG", class = "btn-primary")
    ),
    mainPanel(
      plotOutput("scatter_plot"),
      plotOutput("bar_chart"),
      plotOutput("histogram"),
      plotlyOutput("pie_chart"),
      plotlyOutput("line_graph")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Generate scatter plot based on the training data
  output$scatter_plot <- renderPlot({
    ggplot(amazon_data, aes(x = RatingCount, y = Rating)) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Product Rating vs. Rating Count", x = "Rating Count", y = "Rating") +
      theme(plot.title = element_text(color = "darkblue", size = 16, face = "bold"),
            axis.title = element_text(color = "darkgreen", size = 12, face = "bold"))
  })
  
  # Generate bar chart
  output$bar_chart <- renderPlot({
    rating_counts <- table(amazon_data$Rating)
    bar_chart <- ggplot(data.frame(rating = names(rating_counts), count = as.numeric(rating_counts)),
                        aes(x = rating, y = count)) +
      geom_bar(stat = "identity", fill = "burlywood1") +
      labs(title = "Count of Ratings", x = "Rating", y = "Count") +
      theme(plot.title = element_text(color = "darkblue", size = 18, face = "bold"),
            axis.title = element_text(color = "darkgreen", size = 14, face = "bold"))
    print(bar_chart)
  })
  
  # Generating the pie chrart
  output$pie_chart <- renderPlotly({
    rating_counts <- table(amazon_data$Rating)
    pie_chart <- plot_ly(
      labels = names(rating_counts),
      values = as.numeric(rating_counts),
      type = "pie",
      hole = 0.2
    )
    pie_chart %>%
      layout(showlegend = FALSE, margin = list(b = 0)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Generate histogram
  output$histogram <- renderPlot({
    ggplot(amazon_data, aes(x = RatingCount)) +
      geom_histogram(binwidth = 100, color = "steelblue", fill = "steelblue", alpha = 0.7) +
      labs(title = "Rating Count Distribution", x = "Rating Count", y = "Frequency") +
      theme(plot.title = element_text(color = "purple", size = 18, face = "bold"),
            axis.title = element_text(color = "red", size = 14, face = "bold"))
  })
  
  # Predicting the rating based on user input
  output$prediction_result <- renderText({
    req(input$predict_btn)  # Wait for the button to be clicked
    new_data <- data.frame(RatingCount = as.numeric(input$rating_count))  # Convert to numeric
    predicted_rating <- predict(model, newdata = new_data)
    paste("Predicted Rating:", predicted_rating)
  })
  
  #Save plots as PNG
  observeEvent(input$save_plots_btn, {
    ggsave("scatter_plot.png", output$scatter_plot())
    ggsave("bar_chart.png", output$bar_chart())
    ggsave("histogram.png", output$histogram())
  }) 
}

# Running the Shiny App
shinyApp(ui = ui, server = server)