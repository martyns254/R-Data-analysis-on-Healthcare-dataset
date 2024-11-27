library(shiny)
library(randomForest)

# Define UI
ui <- fluidPage(
  titlePanel("Recovery Time Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Input Patient Data"),
      numericInput("age", "Age:", value = 30, min = 0, max = 120),
      selectInput("gender", "Gender:", choices = c("Male", "Female")),
      selectInput("blood_type", "Blood Type:", choices = c("A", "B", "AB", "O")),
      textInput("medical_condition", "Medical Condition:", value = "Hypertension"),
      selectizeInput("hospital", "Hospital:",
                     choices = unique(trainData$Hospital), # Dynamically load choices
                     options = list(maxOptions = 500)), # Optimize for large lists
      selectInput("admission_type", "Admission Type:", choices = unique(trainData$Admission.Type)),
      textInput("medication", "Medication:", value = "Drug A"),
      numericInput("test_results", "Test Results:", value = 50, min = 0, max = 100),
      selectInput("age_group", "Age Group:", choices = unique(trainData$Age.Group)),
      actionButton("predict", "Predict Recovery Time")
    ),
    
    mainPanel(
      h4("Prediction Results"),
      verbatimTextOutput("prediction"),
      h4("Insights"),
      plotOutput("recovery_plot"),
      plotOutput("feature_importance")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Make predictions based on input
  observeEvent(input$predict, {
    new_data <- data.frame(
      Age = input$age,
      Gender = input$gender,
      Blood.Type = input$blood_type,
      Medical.Condition = input$medical_condition,
      Hospital = input$hospital,
      Admission.Type = input$admission_type,
      Medication = input$medication,
      Test.Results = input$test_results,
      Age.Group = input$age_group
    )
    
    prediction <- predict(rf_model, new_data)
    output$prediction <- renderText({
      paste("Predicted Recovery Time:", round(prediction, 2), "days")
    })
  })
  
  # Plot recovery times
  output$recovery_plot <- renderPlot({
    hist(trainData$Recovery.Time, breaks = 20, main = "Distribution of Recovery Times", 
         xlab = "Recovery Time (days)", col = "skyblue")
  })
  
  # Feature importance plot
  output$feature_importance <- renderPlot({
    importance <- randomForest::importance(rf_model)
    barplot(importance[, 1], names.arg = rownames(importance), 
            main = "Feature Importance", las = 2, col = "orange")
  })
}

# Train Random Forest Model
rf_model <- randomForest(
  Recovery.Time ~ Age + Gender + Blood.Type + Medical.Condition + 
    Hospital + Admission.Type + Medication + Test.Results + 
    Age.Group + Admission.Month + Admission.DayOfWeek,
  data = trainData, ntree = 100
)

# Run App
shinyApp(ui = ui, server = server)
