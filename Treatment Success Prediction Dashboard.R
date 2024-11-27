library(shiny)
library(randomForest)

# Define UI
ui_treatment <- fluidPage(
  titlePanel("Treatment Success Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Treatment Data"),
      numericInput("age", "Age:", value = 30, min = 0, max = 120),
      selectInput("medication", "Medication:", choices = unique(trainData$Medication)),
      selectInput("severity", "Severity:", choices = c("Mild", "Moderate", "Severe")),
      selectInput("hospital", "Hospital:", choices = unique(trainData$Hospital)),
      numericInput("length_of_stay", "Length of Stay (days):", value = 5, min = 1, max = 30),
      selectInput("medical_condition", "Medical Condition:", choices = unique(trainData$Medical.Condition)),
      actionButton("predict", "Predict Treatment Success")
    ),
    
    mainPanel(
      h4("Prediction Results"),
      verbatimTextOutput("prediction"),
      h4("Insights"),
      plotOutput("treatment_plot"),
      plotOutput("treatment_feature_importance")
    )
  )
)

# Define Server
server_treatment <- function(input, output) {
  
  # Make predictions based on input
  observeEvent(input$predict, {
    new_data <- data.frame(
      Age = input$age,
      Medication = input$medication,
      Severity = input$severity,
      Hospital = input$hospital,
      Length.of.Stay = input$length_of_stay,
      Medical.Condition = input$medical_condition
    )
    
    prediction <- predict(rf_model_treatment, new_data)
    output$prediction <- renderText({
      paste("Predicted Treatment Success: ", ifelse(prediction == 1, "Successful", "Unsuccessful"))
    })
  })
  
  # Plot treatment success data distribution
  output$treatment_plot <- renderPlot({
    hist(trainData$TreatmentSuccess, breaks = 20, main = "Distribution of Treatment Success", 
         xlab = "Treatment Success", col = "lightyellow")
  })
  
  # Feature importance plot
  output$treatment_feature_importance <- renderPlot({
    importance <- randomForest::importance(rf_model_treatment)
    barplot(importance[, 1], names.arg = rownames(importance), 
            main = "Feature Importance for Treatment Success", las = 2, col = "lightgreen")
  })
}

# Train Random Forest Model for Treatment Success
rf_model_treatment <- randomForest(
  TreatmentSuccess ~ Age + Medication + Severity + Hospital + Length.of.Stay + Medical.Condition,
  data = trainData, ntree = 100
)

# Run App for Treatment Success Prediction
shinyApp(ui = ui_treatment, server = server_treatment)
