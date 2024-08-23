#-----Retail analysis using list ---------
#### Creating list for further analysis####
# Create a sample list
customer_data <- list(
  Customer_ID = c(1, 2, 3, 4, 5),
  Age = c(25, 34, 45, 29, 40),
  Gender = c("F", "M", "F", "M", "F"),
  Total_Spend = c(2500, 3400, 1500, 4200, 3000)
)

#### Analysing data in the vectors created #####
# Calculate and print summary statistics for numeric elements
age_summary <- summary(customer_data$Age)
print(age_summary)

total_spend_summary <- summary(customer_data$Total_Spend)
print(total_spend_summary)

# Apply mean function to all numeric elements
numeric_means <- sapply(customer_data, function(x) {
  if(is.numeric(x)) return(mean(x))
  return(NA)
})
print(numeric_means)

# Filter customers who spent more than 3000
high_spenders <- customer_data$Customer_ID[customer_data$Total_Spend > 3000]
print(high_spenders)

# Combine selected list elements into a data frame for analysis
customer_df <- data.frame(
  Customer_ID = customer_data$Customer_ID,
  Age = customer_data$Age,
  Total_Spend = customer_data$Total_Spend
)
# Analyze the combined data frame
summary(customer_df)

# Example custom function to calculate CLV based on Total Spend and Age
calculate_clv <- function(total_spend, age) {
  # Simplified example where CLV is a function of age and spend
  clv <- total_spend / (max(age) - age + 1)  # Age factor is for demo purposes
  return(clv)
}
# Apply the custom CLV function to the list elements
customer_clv <- calculate_clv(customer_data$Total_Spend, customer_data$Age)
print(customer_clv)

# Plotting
plot(customer_data$Age, customer_data$Total_Spend,
     main="Total Spend vs. Age",
     xlab="Age", ylab="Total Spend",
     pch=19, col="blue")

# Bar plot of Gender distribution
gender_counts <- table(customer_data$Gender)
barplot(gender_counts, 
        main="Gender Distribution", 
        xlab="Gender", 
        ylab="Count", 
        col=c("pink", "lightblue"))

# Bubble plot of Age vs. Total Spend with bubble size by Total Spend
symbols(customer_data$Age, customer_data$Total_Spend, 
        circles=customer_data$Total_Spend, inches=0.5, 
        main="Bubble Plot of Age vs. Total Spend",
        xlab="Age", ylab="Total Spend", bg="lightblue")





