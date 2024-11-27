data<-read.csv("C:/Users/smutu/OneDrive/Desktop/R project/healthcare_dataset.csv")


file.exists("C:/Users/smutu/OneDrive/Desktop/R project/healthcare_dataset.csv")

head(data)  # Look at the first few rows
summary(data)  # Get a summary of all columns

# Load necessary libraries
library(dplyr)
library(tidyr)

# Remove rows with any missing data
cleaned_data <- data %>%
  drop_na()

# View the cleaned data
head(cleaned_data)

# Remove unnecessary columns like 'Name', 'Doctor', and 'Room.Number'
cleaned_data <- cleaned_data %>%
  select(-Name, -Doctor, -Room.Number)

# View the cleaned data
head(cleaned_data)

# Convert 'Date.of.Admission' and 'Discharge.Date' to Date format
cleaned_data$Date.of.Admission <- as.Date(cleaned_data$Date.of.Admission, format="%m/%d/%Y")
cleaned_data$Discharge.Date <- as.Date(cleaned_data$Discharge.Date, format="%m/%d/%Y")

# View the cleaned data
head(cleaned_data)

# Check if there are any missing values left
summary(cleaned_data)

# Remove duplicates
cleaned_data <- distinct(cleaned_data)

# Create Age Groups
cleaned_data$Age.Group <- cut(cleaned_data$Age, breaks = c(0, 18, 30, 45, 60, 100), 
                              labels = c("0-18", "19-30", "31-45", "46-60", "61+"))

# Disease count by Age Group
disease_by_age <- cleaned_data %>%
  group_by(Age.Group, Medical.Condition) %>%
  summarise(Disease.Count = n()) %>%
  arrange(desc(Disease.Count))

# View the results
head(disease_by_age)

# Load necessary libraries
library(ggplot2)

# Create a bar plot to visualize the disease count across age groups
ggplot(disease_by_age, aes(x = Age.Group, y = Disease.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Disease Count by Age Group", x = "Age Group", y = "Disease Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Disease count by Blood Type
disease_by_blood <- cleaned_data %>%
  group_by(Blood.Type, Medical.Condition) %>%
  summarise(Disease.Count = n()) %>%
  arrange(desc(Disease.Count))

# View the results
head(disease_by_blood)

# Disease count by Gender
disease_by_gender <- cleaned_data %>%
  group_by(Gender, Medical.Condition) %>%
  summarise(Disease.Count = n()) %>%
  arrange(desc(Disease.Count))

# View the results
head(disease_by_gender)

# Disease count by Hospital
disease_by_hospital <- cleaned_data %>%
  group_by(Hospital, Medical.Condition) %>%
  summarise(Disease.Count = n()) %>%
  arrange(desc(Disease.Count))

# View the results
head(disease_by_hospital)

# Load ggplot2
library(ggplot2)

# Bar plot for Disease Count by Blood Type
ggplot(disease_by_blood, aes(x = Blood.Type, y = Disease.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Disease Count by Blood Type", x = "Blood Type", y = "Disease Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Bar plot for Disease Count by Gender
ggplot(disease_by_gender, aes(x = Gender, y = Disease.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Disease Count by Gender", x = "Gender", y = "Disease Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Bar plot for Disease Count by Hospital
ggplot(disease_by_hospital, aes(x = reorder(Hospital, Disease.Count), y = Disease.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Disease Count by Hospital", x = "Hospital", y = "Disease Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal()

ggplot(disease_by_blood, aes(x = Blood.Type, y = Disease.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Disease Count by Blood Type and Gender", x = "Blood Type", y = "Disease Count") +
  facet_wrap(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Hospital Impact: Disease count by Hospital
hospital_impact <- cleaned_data %>%
  group_by(Hospital, Medical.Condition) %>%
  summarise(Disease.Count = n()) %>%
  arrange(desc(Disease.Count))

# View the results
head(hospital_impact)

# Convert dates to Date format
cleaned_data$Date.of.Admission <- as.Date(cleaned_data$Date.of.Admission, format="%m/%d/%Y")
cleaned_data$Discharge.Date <- as.Date(cleaned_data$Discharge.Date, format="%m/%d/%Y")

# Calculate recovery time in days
cleaned_data$Recovery.Time <- as.numeric(difftime(cleaned_data$Discharge.Date, cleaned_data$Date.of.Admission, units = "days"))

# Medication and Recovery Time
medication_effectiveness <- cleaned_data %>%
  group_by(Medication) %>%
  summarise(Average.Recovery.Time = mean(Recovery.Time, na.rm = TRUE))

# View the results
head(medication_effectiveness)

# Medication Effectiveness for each Disease
medication_disease_effectiveness <- cleaned_data %>%
  group_by(Medication, Medical.Condition) %>%
  summarise(Average.Recovery.Time = mean(Recovery.Time, na.rm = TRUE)) %>%
  arrange(desc(Average.Recovery.Time))

# View the results
head(medication_disease_effectiveness)

# Convert Date.of.Admission to Date format (if not already in Date format)
cleaned_data$Date.of.Admission <- as.Date(cleaned_data$Date.of.Admission, format = "%m/%d/%Y")

# Now, perform the admission trends analysis
admission_trends <- cleaned_data %>%
  group_by(Year_Month = format(Date.of.Admission, "%Y-%m"), Medical.Condition) %>%
  summarise(Total.Admissions = n(), .groups = "drop") %>%  # Drop groups after summarizing
  arrange(Year_Month)

# View the results
head(admission_trends)

# Visualize the admission trends
ggplot(admission_trends, aes(x = Year_Month, y = Total.Admissions, color = Medical.Condition)) +
  geom_line() +
  labs(title = "Admission Trends Over Time by Disease", x = "Year-Month", y = "Total Admissions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# Filter data for severe diseases
high_risk_diseases <- cleaned_data %>%
  filter(Medical.Condition %in% c("Cancer", "Obesity", "Diabetes")) %>%
  group_by(Age.Group) %>%
  summarise(High.Risk.Count = n())

# View the results
head(high_risk_diseases)

# High-Risk Conditions (group by age group and disease)
high_risk_conditions <- cleaned_data %>%
  filter(Medical.Condition %in% c("Cancer", "Obesity", "Diabetes")) %>%
  group_by(Age.Group, Medical.Condition) %>%
  summarise(High.Risk.Count = n()) %>%
  arrange(desc(High.Risk.Count))

# View the results
head(high_risk_conditions)

# Visualize High-Risk Conditions by Age Group and Disease
ggplot(high_risk_conditions, aes(x = Age.Group, y = High.Risk.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "High-Risk Conditions by Age Group and Disease", 
       x = "Age Group", 
       y = "Count of High-Risk Conditions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# Visualize High-Risk Diseases by Age Group
ggplot(high_risk_diseases, aes(x = Age.Group, y = High.Risk.Count, fill = Medical.Condition)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "High-Risk Diseases by Age Group", 
       x = "Age Group", 
       y = "Total Count of High-Risk Diseases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Ensure the date columns are of Date type
cleaned_data$Recovery.Time <- as.numeric(cleaned_data$Discharge.Date - cleaned_data$Date.of.Admission)

# Check if the new column is created and populated correctly
summary(cleaned_data$Recovery.Time)
head(cleaned_data$Recovery.Time)


colnames(cleaned_data)

# Split data into training (80%) and testing (20%) sets
install.packages("caret")
library(caret)
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(cleaned_data$Recovery.Time, p = 0.8, list = FALSE)
trainData <-cleaned_data[trainIndex, ]
testData <- cleaned_data[-trainIndex, ]

# Load the randomForest package
install.packages("randomForest")
library(randomForest)

colnames(trainData)

# Extract Month and Day of the Week from the admission date
trainData$Admission.Month <- format(trainData$Date.of.Admission, "%m")
trainData$Admission.DayOfWeek <- weekdays(trainData$Date.of.Admission)

# Convert these to factors
trainData$Admission.Month <- factor(trainData$Admission.Month)
trainData$Admission.DayOfWeek <- factor(trainData$Admission.DayOfWeek)
# Run the random forest model
rf_model <- randomForest(Recovery.Time ~ Age + Gender + Blood.Type + Medical.Condition + 
                           Hospital + Insurance.Provider + Billing.Amount + Admission.Type + 
                           Medication + Test.Results + Age.Group + Admission.Month + Admission.DayOfWeek,
                         data = trainData, ntree = 100)

# Print model summary
print(rf_model)
# Apply the same feature creation steps for the test data
testData$Admission.Month <- format(testData$Date.of.Admission, "%m")
testData$Admission.DayOfWeek <- weekdays(testData$Date.of.Admission)

# Convert to factors
testData$Admission.Month <- factor(testData$Admission.Month)
testData$Admission.DayOfWeek <- factor(testData$Admission.DayOfWeek)
# Make predictions on the test data
predictions <- predict(rf_model, newdata = testData)

# Evaluate model performance (e.g., RMSE)
rmse <- sqrt(mean((predictions - testData$Recovery.Time)^2))
print(paste("RMSE: ", rmse))

install.packages("shiny")
library(shiny)
shiny::runApp("C:/Users/smutu/OneDrive/Documents/Recoverytime/app.R")

summary(cleaned_data$Recovery.Time)
unique(cleaned_data$Recovery.Time)
str(cleaned_data)  # Check the structure of the dataset
colnames(cleaned_data)  # Verify column names
head(cleaned_data)
