# Install necessary packages if you haven't already
#install.packages(c("caTools", "caret", "randomForest"))

# Load libraries
library(readr)
library(dplyr)
library(tidyverse)
library(caTools)
library(caret)
library(randomForest)
library(ggplot2)

#load the data
TelcoCustChurn <- read_csv("Downloads/Data Technician/R Notes/TelcoCustChurn .csv")
#Convert categorical variables to factors
data <- TelcoCustChurn
data$SeniorCitizen <-as.factor(data$SeniorCitizen)
data$Churn <- as.factor(data$Churn)

#Handle missing values if any
data <- data %>%
  mutate(TotalCharges = as.numeric(TotalCharges)) %>%
  drop_na()
# Convert 'No phone service' to 'No' in MultipleLines for consistency
data$MultipleLines <- ifelse(data$MultipleLines == "No phone service", "No", data$MultipleLines)

# Preview the cleaned data
str(data)

# Churn rate
churn_rate <- data %>%
  group_by(Churn) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
print(churn_rate)

# Distribution of tenure
ggplot(data, aes(x = tenure, fill = Churn)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Distribution of Tenure", x = "Tenure (months)", y = "Count")

####DEMOGRAPHIC ANALYSIS
# Churn rate by gender 
ggplot(data, aes(x = gender, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Gender Type", x = "Gender", y = "Proportion")

# Churn rate by age group 
ggplot(data, aes(x = SeniorCitizen, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate for Senior citizens and younger folks ", x = "SeniorCitizen", y = "Proportion")

# Churn rate by people with or without a partner  
ggplot(data, aes(x = Partner, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by people with or without a partner ", x = "Partner", y = "Proportion")

# Churn rate by people with or without dependents 
ggplot(data, aes(x = Partner, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by people with or without dependents", x = "Dependents", y = "Proportion")


####SERVICE USAGE ANALYSIS
# Churn rate by people who use phone service 
ggplot(data, aes(x = PhoneService, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate people who use phone service", x = "Phone service", y = "Proportion")

# Churn rate by people who use internet service 
ggplot(data, aes(x = InternetService, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate people who use internet service", x = "Internet service", y = "Proportion")

#### CONTRACT AND PAYMENT METHOD ANALYSIS 
# Churn rate by contract type
ggplot(data, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Contract Type", x = "Contract Type", y = "Proportion")

# Churn rate by Payment method
ggplot(data, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Payment Method", x = "Payment Method", y = "Proportion")

#### FINANCIAL ANALYSIS
# Churn rate by monthly charges
ggplot(data, aes(x = MonthlyCharges, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Monthly Charges", x = "Monthly Charges", y = "Proportion")

# Churn rate by monthly charges
ggplot(data, aes(x = tenure, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Monthly Charges", x = "Monthly Charges", y = "Proportion")

#### PREDICTIVE MODELLING WITH LOGISTIC REGRESSION
# 1. Feature selection using Recursive Feature Elimination (RFE) to recursively removes features and build a model on those that remain. 
#RFE ranks features by importance and selects the best subset.
# Define the control using a logistic regression function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Run the RFE algorithm
results <- rfe(data[, -which(names(data) == "Churn")], 
               data$Churn, 
               sizes = c(1:5), 
               rfeControl = control)

# List the chosen features
print(predictors(results))

# Plot the results
plot(results, type = c("g", "o"))

# Subset the data to only include selected features
selected_features <- c("tenure", "TotalCharges", "MonthlyCharges", "Contract", "OnlineSecurity", "TechSupport","Churn")
data_selected <- data[, selected_features]

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(data_selected$Churn, SplitRatio = 0.7)
train <- subset(data_selected, split == TRUE)
test <- subset(data_selected, split == FALSE)

# Logistic Regression Model
model <- glm(Churn ~ tenure + TotalCharges + MonthlyCharges + Contract + OnlineSecurity + TechSupport, 
             data = train, 
             family = binomial)

model <- glm(Churn ~ tenure + TotalCharges + MonthlyCharges, 
             data = train, 
             family = binomial)


# Summary of the model
summary(model)

# Predict on the test set
predictions <- predict(model, test, type = "response")
test$predicted <- ifelse(predictions > 0.5, "Yes", "No")

# Model evaluation
conf_matrix <- confusionMatrix(as.factor(test$predicted), test$Churn)
print(conf_matrix)

# Random Forest Model
rf_model <- randomForest(Churn ~ ., data = train, importance = TRUE)

# Feature importance plot
varImpPlot(rf_model)



