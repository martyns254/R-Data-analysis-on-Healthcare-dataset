library(shiny)
library(randomForest)

# Define UI
ui_mortality <- fluidPage(
  titlePanel("Mortality Risk Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Patient Data"),
      numericInput("age", "Age:", value = 30, min = 0, max = 120),
      selectInput("gender", "Gender:", choices = c("Male", "Female")),
      selectInput("severity", "Severity:", choices = c("Mild", "Moderate", "Severe")),
      selectInput("admission_type", "Admission Type:", choices = unique(trainData$Admission.Type)),
      actionButton("predict", "Predict Mortality Risk")
    ),
    
    mainPanel(
      h4("Prediction Results"),
      verbatimTextOutput("prediction"),
      h4("Insights"),
      plotOutput("mortality_plot"),
      plotOutput("mortality_feature_importance")
    )
  )
)

# Define Server
server_mortality <- function(input, output) {
  
  # Make predictions based on input
  observeEvent(input$predict, {
    new_data <- data.frame(
      Age = input$age,
      Gender = input$gender,
      Severity = input$severity,
      Admission.Type = input$admission_type
    )
    
    prediction <- predict(rf_model_mortality, new_data)
    output$prediction <- renderText({
      paste("Predicted Mortality Risk: ", ifelse(prediction == 1, "High", "Low"))
    })
  })
  
  # Plot mortality data distribution
  output$mortality_plot <- renderPlot({
    hist(trainData$MortalityRisk, breaks = 20, main = "Distribution of Mortality Risk", 
         xlab = "Mortality Risk", col = "lightcoral")
  })
  
  # Feature importance plot
  output$mortality_feature_importance <- renderPlot({
    importance <- randomForest::importance(rf_model_mortality)
    barplot(importance[, 1], names.arg = rownames(importance), 
            main = "Feature Importance for Mortality Risk", las = 2, col = "salmon")
  })
}

# Train Random Forest Model for Mortality
rf_model_mortality <- randomForest(
  MortalityRisk ~ Age + Gender + Severity + Admission.Type,
  data = trainData, ntree = 100
)

# Run App for Mortality Risk
shinyApp(ui = ui_mortality, server = server_mortality)
