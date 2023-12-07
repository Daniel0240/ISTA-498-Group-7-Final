library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Read the dataset
df <- read.csv("/Users/lilmanchu/Documents/ISTA_498/shopping_trends_updated.csv", stringsAsFactors = FALSE)

# Clean column names by replacing spaces and special characters
names(df) <- gsub("[ )(]", "_", names(df))

# Assuming 'Discount Applied' is a factor with levels 'Yes' and 'No'
# and 'Review Rating' is numeric
df <- df %>%
  mutate(Discount.Factor = ifelse(Discount.Applied == 'Yes', 0.9, 1.0),
         Discounted.Price = Purchase.Amount..USD. * Discount.Factor)

# Define the UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "E-commerce Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Total Purchase Amount by Season", status = "primary", solidHeader = TRUE,
                    plotOutput("seasonalPlot", height = "300px", click = "seasonalPlot_click")),
                box(title = "Average Numeric Frequency of Purchases by Age and Gender", status = "warning", solidHeader = TRUE,
                    plotOutput("ageGenderPlot", height = "300px", click = "ageGenderPlot_click"))
              ),
              fluidRow(
                box(title = "Total Purchase Amount by Gender", status = "info", solidHeader = TRUE,
                    plotOutput("genderPlot", height = "300px", click = "genderPlot_click")),
                box(title = "Relationship Between Discounted Price and Purchase Amount", status = "danger", solidHeader = TRUE,
                    plotOutput("discountPlot", height = "300px", click = "discountPlot_click"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$seasonalPlot <- renderPlot({
    # Generate Total Purchase Amount by Season plot
    seasonal_sales <- aggregate(Purchase.Amount..USD. ~ Season, data = df, FUN = sum)
    ggplot(seasonal_sales, aes(x = Season, y = Purchase.Amount..USD., fill = Season)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c("springgreen", "lightcoral", "lightskyblue", "gold")) +
      geom_text(aes(label = round(Purchase.Amount..USD., digits = 2)),
                vjust = -0.3, size = 3.5, position = position_dodge(width = 0.9)) +
      labs(x = 'Season',
           y = 'Total Purchase Amount (USD)') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  observeEvent(input$seasonalPlot_click, {
    showModal(modalDialog(
      title = "Explanation: Total Purchase Amount by Season",
      "This bar chart illustrates the total spending in US dollars for four seasons. Each bar is color-coded—green for Fall, red for Spring, blue for Summer, and yellow for Winter—allowing for quick visual differentiation.

The chart reveals that Fall leads in spending with $60,018, followed by Spring and Winter, with Summer trailing behind. The near-equal spending in Spring and Winter suggests similar consumer behavior across these seasons, while the peak in Fall could indicate seasonal promotions or shopping habits that encourage higher sales.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$ageGenderPlot <- renderPlot({
    # Assuming df has a column Frequency.of.Purchases with values like 'Weekly', 'Monthly', etc.
    frequency_mapping <- c('Weekly' = 52, 'Fortnightly' = 26, 'Monthly' = 12,
                           'Quarterly' = 4, 'Annually' = 1)
    df$Numeric.Frequency.of.Purchases <- sapply(df$Frequency.of.Purchases, function(x) frequency_mapping[x])
    
    # Now, create the plot for Average Numeric Frequency of Purchases by Age and Gender
    age_gender_group <- df %>%
      group_by(Age, Gender) %>%
      summarise(Avg.Numeric.Frequency = mean(Numeric.Frequency.of.Purchases, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(age_gender_group, aes(x = Age, y = Avg.Numeric.Frequency, color = Gender)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = lm, aes(color = Gender, fill = Gender), se = FALSE) +
      labs(x = 'Age',
           y = 'Average Numeric Frequency of Purchases') +
      theme_minimal() +
      scale_color_manual(values = c('Female' = 'blue', 'Male' = 'orange')) +
      scale_fill_manual(values = c('Female' = 'blue', 'Male' = 'orange')) +
      theme(legend.position = "bottom")
  })
  observeEvent(input$ageGenderPlot_click, {
    showModal(modalDialog(
      title = "Explanation: Average Numeric Frequency of Purchases by Age and Gender",
      "This is a scatter plot that depicts the average number of purchases made by different age groups, separated by gender. Each dot on the plot represents an age group's average purchasing frequency, with the color blue signifying female and orange for male, allowing for an immediate visual distinction between the genders.

Trend lines, one for each gender, are overlaid on the scatter plot to indicate the overall direction of purchasing frequency as age increases. This visualization helps to identify patterns/trends in purchasing behavior across different ages and between genders.

In the chart, it can be seen that younger men typically have a higher average numeric frequency of purchases than younger women but as people get older it seems to flip, with older women buying more frequently than older men.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$genderPlot <- renderPlot({
    # Generate Total Purchase Amount by Gender plot
    total_purchase_by_gender <- aggregate(Purchase.Amount..USD. ~ Gender, data = df, FUN = sum)
    ggplot(total_purchase_by_gender, aes(x = Gender, y = Purchase.Amount..USD., fill = Gender)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = Purchase.Amount..USD.), vjust = -0.3, size = 4) +
      scale_fill_manual(values = c("Female" = "#FF69B4", "Male" = "#1E90FF")) +
      labs(x = "", y = "Total Purchase Amount (USD)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12),
            legend.title = element_blank(),
            legend.position = "bottom") +
      guides(fill = guide_legend(reverse = TRUE))
  })
  observeEvent(input$genderPlot_click, {
    showModal(modalDialog(
      title = "Explanation: Total Purchase Amount by Gender",
      "This graph is a vertical bar chart that compares the total purchase amounts made by females and males. The y-axis represents the total purchase amount in USD, while the x-axis distinguishes between the two genders.

In the chart, the pink bar represents females and the blue bar represents males. The numbers above the bars indicate the total purchase amounts for each gender: females have a total of 75,191 USD and males significantly more at 157,890 USD. This suggests that males contributed to a higher purchase volume within the dataset's context. This information could be important for making informed decisions regarding marketing strategies, product stocking, and sales targeting for different gender demographics.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$discountPlot <- renderPlot({
    # Generate Relationship between Discounted Price and Purchase Amount plot
    df$Discount.Factor <- ifelse(df$Discount.Applied == 'Yes', 0.9, 1.0)
    df$Discounted.Price <- df$Purchase.Amount..USD. * df$Discount.Factor
    ggplot(df, aes(x = Discounted.Price, y = Purchase.Amount..USD.)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = lm, color = 'red') +
      labs(x = 'Discounted Price (USD)',
           y = 'Purchase Amount (USD)') +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  observeEvent(input$discountPlot_click, {
    showModal(modalDialog(
      title = "Explanation: Relationship Between Discounted Price and Purchase Amount",
      "This graph is a scatterplot with a central trend line and two dotted lines parallel to the main trend line. These dotted lines create a channel around the central trend line, potentially representing a confidence interval or prediction interval. 

The area between the dotted lines captures the range within which a significant portion of the data points fall. This band indicates the variability of the purchase amounts relative to the discounted price; most purchase amounts are expected to lie within this range given a certain discounted price. The narrower this band, the more consistent the relationship between the two variables is.

Ultimately, the space between the dotted lines adds a layer of understanding to the strength and consistency of the relationship depicted by the trend line, giving us an idea of the reliability of the pattern observed in the data.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# Run the application
shinyApp(ui, server)
