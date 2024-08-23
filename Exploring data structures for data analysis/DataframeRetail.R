#-----Retail analysis using data frames ---------
install.packages("ggplot2")
library(ggplot2)

# Create a sample data frame
customer_df <- data.frame(
  Customer_ID = c(1, 2, 3, 4, 5),
  Age = c(25, 34, 45, 29, 40),
  Gender = c("F", "M", "F", "M", "F"),
  Total_Spend = c(2500, 3400, 1500, 4200, 3000)
)

# Print the data frame
print(customer_df)

# Add a new column for Customer Status based on Total Spend
customer_df$Status <- ifelse(customer_df$Total_Spend > 3000, "High", "Low")
print(customer_df)

# Add a new column for Gender
customer_df$Gender <- c(34, 25, 35, 45, 45)
print(customer_df)


#View the structure
str(customer_df)

#Summary statistics
summary(customer_df)

#Accessing data
#View and subsetting
head(customer_df)

# Subset the first three rows
subset_rows <- customer_df[1:3, ]
print(subset_rows)

# Subset a column
age_column <- customer_df$Age
print(age_column)

# Subset the 'Customer_ID' and 'Total_Spend' columns
subset_columns <- customer_df[, c("Customer_ID", "Total_Spend")]
print(subset_columns)

# Analysing the data
# Filter customers with Total_Spend greater than 3000
high_spenders <- customer_df[customer_df$Total_Spend > 3000, ]
print(high_spenders)

# Sort by Total_Spend in descending order
sorted_df <- customer_df[order(-customer_df$Total_Spend), ]
print(sorted_df)

# Aggregate total spend by Gender
aggregate_data <- aggregate(Total_Spend ~ Gender, data=customer_df, FUN=sum)
print(aggregate_data)

# Create another data frame for merging
region <- data.frame(
  Customer_ID = c(1, 2, 3, 4, 5),
  Region = c("North", "West", "South", "East", "Central")
)

# Merge the data frames by Customer_ID
merged_df <- merge(customer_df, region, by="Customer_ID")
print(merged_df)

#Visualising the data
# Bar plot of Total Spend by Gender
barplot(aggregate(Total_Spend ~ Gender, data=customer_df, FUN=sum)$Total_Spend,
        names.arg=aggregate(Total_Spend ~ Gender, data=customer_df, FUN=sum)$Gender,
        main="Total Spend by Gender",
        xlab="Gender", 
        ylab="Total Spend",
        col=c("lightblue", "lightgreen"))

# Bubble plot of Age vs Total Spend with bubble size by Customer_ID
ggplot(customer_df, aes(x=Age, y=Total_Spend, size=Customer_ID)) +
  geom_point(alpha=0.7) +
  labs(title="Bubble Plot of Age vs Total Spend", x="Age", y="Total Spend")

# Heatmap of correlation matrix
cor_matrix <- cor(customer_df[ ,sapply(customer_df, is.numeric)])
heatmap(cor_matrix, 
        main="Correlation Heatmap", 
        col=heat.colors(256))


