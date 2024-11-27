library(shiny)
library(randomForest)

# Define UI
ui_medication <- fluidPage(
  titlePanel("Medication Effectiveness Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Medication Data"),
      numericInput("age", "Age:", value = 30, min = 0, max = 120),
      selectInput("medical_condition", "Medical Condition:", choices = unique(trainData$Medical.Condition)),
      selectInput("severity", "Severity:", choices = c("Mild", "Moderate", "Severe")),
      selectInput("medication", "Medication:", choices = unique(trainData$Medication)),
      actionButton("predict", "Predict Medication Effectiveness")
    ),
    
    mainPanel(
      h4("Prediction Results"),
      verbatimTextOutput("prediction"),
      h4("Insights"),
      plotOutput("medication_plot"),
      plotOutput("medication_feature_importance")
    )
  )
)

# Define Server
server_medication <- function(input, output) {
  
  # Make predictions based on input
  observeEvent(input$predict, {
    new_data <- data.frame(
      Age = input$age,
      Medical.Condition = input$medical_condition,
      Severity = input$severity,
      Medication = input$medication
    )
    
    prediction <- predict(rf_model_medication, new_data)
    
    #Run app
    # Run the Billing Prediction App
    shinyApp(ui = ui_medication, server = server_medication)
