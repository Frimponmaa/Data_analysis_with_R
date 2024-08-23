#-----Retail analysis using vectors ---------
#### Creating vectors for further analysis####
# Customer vectors
Customer_ID <- c(1, 2, 3, 4, 5)
Age <- c(25, 34, 45, 29, 40)
Gender <- c("F", "M", "F", "M", "F")
Location <- c("New York", "California", "Texas", "Florida", "Illinois")
Account_Age <- c(2, 3, 1, 4, 5)  # in years
Total_Spend <- c(2500, 3400, 1500, 4200, 3000)
Number_of_Orders <- c(10, 15, 7, 20, 18)
Average_Order_Value <- Total_Spend / Number_of_Orders

# Order vectors
Order_ID <- c(1001, 1002, 1003, 1004, 1005)
Order_Customer_ID <- c(1, 2, 3, 4, 5)
Order_Date <- as.Date(c('2024-01-15', '2024-02-17', '2024-03-10', '2024-04-05', '2024-05-20'))
Order_Value <- c(250, 300, 150, 400, 350)
Number_of_Items <- c(2, 3, 1, 4, 3)
Payment_Method <- c("Credit Card", "PayPal", "Credit Card", "Credit Card", "PayPal")
Shipping_Method <- c("Standard", "Express", "Standard", "Express", "Standard")
Discount_Applied <- c(0, 20, 0, 30, 10)  # Discount in dollars

# Product vectors
Product_ID <- c(2001, 2002, 2003, 2004, 2005)
Product_Category <- c("Electronics", "Clothing", "Home", "Books", "Toys")
Product_Price <- c(100, 50, 30, 20, 25)
Stock_Quantity <- c(50, 100, 75, 30, 60)
Discount_Applied_Product <- c(10, 5, 0, 2, 0)  # Discount in dollars
Date_Added <- as.Date(c('2023-12-01', '2023-11-15', '2024-01-10', '2024-02-01', '2024-03-01'))

# Transaction vectors
Transaction_ID <- c(3001, 3002, 3003, 3004, 3005)
Transaction_Order_ID <- c(1001, 1002, 1003, 1004, 1005)
Transaction_Product_ID <- c(2001, 2002, 2003, 2004, 2005)
Quantity <- c(1, 2, 1, 3, 2)
Unit_Price <- c(100, 50, 30, 20, 25)
Total_Price <- Quantity * Unit_Price

#### Analysing data in the vectors created #####
# ------ Segment customers by total spend
spend_segment <- cut(Total_Spend, 
                     breaks = c(0, 2000, 4000, Inf), 
                     labels = c("Low", "Medium", "High"))
print(spend_segment)

# ------ Calculate total sales by product
total_sales_by_product <- tapply(Total_Price, Transaction_Product_ID, sum)
names(total_sales_by_product) <- Product_ID
print(total_sales_by_product)

#-----Monthly sales trends-------
# Extract month from Order_Date
order_months <- format(Order_Date, "%Y-%m")
unique_months <- unique(order_months)

# Calculate total orders and values per month
monthly_order_summary <- sapply(unique_months, function(month) {
  indices <- which(order_months == month)
  total_orders <- length(indices)
  total_value <- sum(Order_Value[indices])
  return(c(Total_Orders = total_orders, Total_Value = total_value))
})
colnames(monthly_order_summary) <- unique_months
print(monthly_order_summary)

# -------- Calculate average order value by customer
average_order_value_by_customer <- tapply(Order_Value, Order_Customer_ID, mean)
names(average_order_value_by_customer) <- Customer_ID
print(average_order_value_by_customer)

######Visualising the data in vectors
# 1.  Total sales by product (using vectors) using barplot
total_sales_by_product <- tapply(Total_Price, Transaction_Product_ID, sum)
names(total_sales_by_product) <- Product_Category
barplot(total_sales_by_product,
        main = "Total Sales by Product",
        xlab = "Product Category",
        ylab = "Total Sales",
        col = "skyblue",
        las = 2)  # Rotate x-axis labels

# 2. Histogram of total spend
hist(Total_Spend,
     main = "Distribution of Customer Total Spend",
     xlab = "Total Spend",
     col = "lightblue",
     border = "black")

#3.  Scatter plot
plot(Total_Spend, Average_Order_Value,
     main = "Average Order Value vs. Total Spend",
     xlab = "Total Spend",
     ylab = "Average Order Value",
     col = "blue",
     pch = 19)  # pch = 19 for filled circles

#### Add data to vectors
# Add new data to the customer vectors
Customer_ID <- c(Customer_ID, 6)
Age <- c(Age, 38)
Gender <- c(Gender, "M")
Location <- c(Location, "Nevada")
Account_Age <- c(Account_Age, 2)
Total_Spend <- c(Total_Spend, 2800)
Number_of_Orders <- c(Number_of_Orders, 12)
Average_Order_Value <- c(Average_Order_Value, 2800 / 12)

#### Accessing data in vectors
# Access the first element
first_customer <- Customer_ID[1]
print(first_customer)

# Access the last two elements
last_two_ages <- Age[4:5]
print(last_two_ages) 

# Access customers older than 30
customers_older_than_30 <- Customer_ID[Age > 30]
print(customers_older_than_30)

# Access age of customer with ID 3
# Named vector example
names(Age) <- Customer_ID
age_of_customer_3 <- Age["3"]
print(age_of_customer_3)  

# Access the 2nd and 5th ages
selected_ages <- Age[c(2, 5)]
print(selected_ages)

# Exclude the first element
without_first_customer <- Customer_ID[-1]
print(without_first_customer) 

# Access customer IDs with an age less than 35
young_customers <- Customer_ID[Age < 35]
print(young_customers)

# Get the number of customers
number_of_customers <- length(Customer_ID)
print(number_of_customers) 


#### Mathematical operations on Vectors
# Add 500 to each customer's total spend
increased_spend <- Total_Spend + 500
print(increased_spend)  

# Subtract 200 from each customer's total spend
reduced_spend <- Total_Spend - 200
print(reduced_spend)  

# Multiply each total spend by 1.1 (e.g., applying a 10% increase)
adjusted_spend <- Total_Spend * 1.1
print(adjusted_spend)  

# Divide each total spend by 2
half_spend <- Total_Spend / 2
print(half_spend)  

# Sum of all total spend
total_sum <- sum(Total_Spend)
print(total_sum)

# Mean of total spend
mean_spend <- mean(Total_Spend)
print(mean_spend)  

# Median of total spend
median_spend <- median(Total_Spend)
print(median_spend) 

# Minimum and maximum of total spend
min_spend <- min(Total_Spend)
max_spend <- max(Total_Spend)
print(min_spend)  
print(max_spend)  

# Cumulative sum of total spend
cumulative_spend <- cumsum(Total_Spend)
print(cumulative_spend)  

# Apply a discount of $200, $300, $150, $400, and $250 respectively
Discount <- c(200, 300, 150, 400, 250)
# Subtract the discount from the total spend
net_spend <- Total_Spend - Discount
print(net_spend)  

# Identify customers with total spend greater than 3000
high_spenders <- Total_Spend > 3000
print(high_spenders)  

# Round each total spend to the nearest hundred
rounded_spend <- round(Total_Spend, -2)
print(rounded_spend)  
