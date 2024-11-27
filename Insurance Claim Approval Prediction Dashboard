library(shiny)
library(randomForest)

# Define UI
ui_claim <- fluidPage(
  titlePanel("Insurance Claim Approval Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Claim Data"),
      numericInput("billing_amount", "Billing Amount:", value = 1000, min = 0, max = 10000),
      selectInput("medical_condition", "Medical Condition:", choices = unique(trainData$Medical.Condition)),
      selectInput("hospital", "Hospital:", choices = unique(trainData$Hospital)),
      selectInput("admission_type", "Admission Type:", choices = unique(trainData$Admission.Type)),
      numericInput("test_results", "Test Results:", value = 50, min = 0, max = 100),
      actionButton("predict", "Predict Claim Approval")
    ),
    
    mainPanel(
      h4("Prediction Results"),
      verbatimTextOutput("prediction"),
      h4("Insights"),
      plotOutput("claim_plot"),
      plotOutput("claim_feature_importance")
    )
  )
)

# Define Server
server_claim <- function(input, output) {
  
  # Make predictions based on input
  observeEvent(input$predict, {
    new_data <- data.frame(
      Billing.Amount = input$billing_amount,
      Medical.Condition = input$medical_condition,
      Hospital = input$hospital,
      Admission.Type = input$admission_type,
      Test.Results = input$test_results
    )
    
    prediction <- predict(rf_model_claim, new_data)
    output$prediction <- renderText({
      paste("Predicted Claim Approval: ", ifelse(prediction == 1, "Approved", "Denied"))
    })
  })
  
  # Plot claim approval distribution
  output$claim_plot <- renderPlot({
    hist(trainData$ClaimApproval, breaks = 20, main = "Distribution of Claim Approvals", 
         xlab = "Claim Approval", col = "lightgreen")
  })
  
  # Feature importance plot
  output$claim_feature_importance <- renderPlot({
    importance <- randomForest::importance(rf_model_claim)
    barplot(importance[, 1], names.arg = rownames(importance), 
            main = "Feature Importance for Claim Approval", las = 2, col = "lightblue")
  })
}

# Train Random Forest Model for Claim Approval
rf_model_claim <- randomForest(
  ClaimApproval ~ Billing.Amount + Medical.Condition + Hospital + Admission.Type + Test.Results,
  data = trainData, ntree = 100
)

# Run App for Claim Approval Prediction
shinyApp(ui = ui_claim, server = server_claim)
