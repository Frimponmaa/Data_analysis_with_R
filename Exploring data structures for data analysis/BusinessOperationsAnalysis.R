# Create a vector of product names
product_names <- c("Laptop", "Smartphone", "Headphones", "Keyboard", "Mouse", "Monitor")

# Access the first element of a vector
first_product <- product_names[2]
print(first_product)  # Output: "Laptop"


# List for two customer orders
order1 <- list(
  customer_name = "John Doe",
  product_purchased = "Laptop",
  quantity = 1,
  total_price = 999.99
)
order2 <- list(
  customer_name = "John Smith",
  product_purchased = "Keyboard",
  quantity = 2,
  total_price = 799.99
)

# Access the customer name and total price
customer_name <- order1$customer_name
print(customer_name)  # Output: "John Doe"

total_price <- order2[["total_price"]]
print(total_price)  # Output: 799.99


# Create a data frame with customer orders
orders_df <- data.frame(
  customer_name = c("John Doe", "Jane Smith", "Eunice Johnson", "Suzzie Brown"),
  product = c("Laptop", "Headphones", "Smartphone", "Keyboard"),
  quantity = c(1, 2, 1, 1),
  region = c("North", "South", "East", "West"),
  stringsAsFactors = FALSE  # Avoid automatic conversion to factors for character columns
)

# Display the data frame
print(orders_df)

# Access the element in the first row and second column
first_order_product <- orders_df[1, 2]
print(first_order_product)  # Output: "Laptop"

# Access the product column
products <- orders_df$product
print(products)  # Output: "Laptop" "Headphones" "Smartphone" "Keyboard"

# Access the first row of the data frame
first_order <- orders_df[1, ]
print(first_order)

# Create a factor to represent the regions where the orders are being delivered.
region_factor <- factor(c("North", "South", "East", "West", "North", "East", "South"))

# Display the factor and its levels
print(region_factor)
print(levels(region_factor))

# Access the first element
first_region <- region_factor[1]
print(first_region)  # Output: "North"

# Access the levels of the factor
levels_region <- levels(region_factor)
print(levels_region)  # Output: "East" "North" "South" "West"


#Add more data
# Original vector of product names
#product_names <- c("Laptop", "Smartphone", "Headphones", "Keyboard", "Mouse", "Monitor")

# New products to add
new_products <- c("Webcam", "Printer", "Tablet")

# Use a loop to add new products
for (product in new_products) {
  product_names <- c(product_names, product)
}

# Print updated vector
print(product_names)

# Add more orders
# Create a list to store multiple orders
orders_list <- list()

# Define new customer orders
customer_orders <- list(
  list(customer_name = "Emily Davis", product_purchased = "Webcam", quantity = 1, total_price = 59.99),
  list(customer_name = "Michael Lee", product_purchased = "Printer", quantity = 1, total_price = 199.99),
  list(customer_name = "Sophia Green", product_purchased = "Tablet", quantity = 1, total_price = 299.99)
)

# Use a loop to add each new order to the list
for (order in customer_orders) {
  orders_list <- append(orders_list, list(order))
}

# Print updated list
print(orders_list)

# Add more data to orders to add
new_orders <- data.frame(
  customer_name = c("Emily Davis", "Michael Lee", "Sophia Green"),
  product = c("Webcam", "Printer", "Tablet"),
  quantity = c(1, 1, 1),
  region = c("North", "South", "East"),
  stringsAsFactors = FALSE
)

# Use a loop to add each new order to the data frame
for (i in 1:nrow(new_orders)) {
  orders_df <- rbind(orders_df, new_orders[i, ])
}

# Print updated data frame
print(orders_df)

#Add more regions
# Original factor for regions
#region_factor <- factor(c("North", "South", "East", "West", "North", "East", "South"))

# New regions to add
new_regions <- c("North", "South", "East", "West", "Central")

# Convert to character vector and append new regions
region_vector <- as.character(region_factor)
region_vector <- c(region_vector, new_regions)

# Create a new factor with updated levels
region_factor <- factor(region_vector, levels = unique(c(levels(region_factor), new_regions)))

# Print updated factor
print(region_factor)
print(levels(region_factor))


#--------------------------------- new code -----------------------------------
#----------------------- TOTAL SALES PER REGION-----------------------
# Original data frame with customer orders
orders_df <- data.frame(
  customer_name = c("John Doe", "Jane Smith", "Alice Johnson", "Bob Brown", "Emily Davis", "Michael Lee", "Sophia Green"),
  product = c("Laptop", "Headphones", "Smartphone", "Keyboard", "Webcam", "Printer", "Tablet"),
  quantity = c(1, 2, 1, 1, 1, 1, 1),
  total_price = c(999.99, 199.98, 299.99, 89.99, 59.99, 199.99, 299.99),
  region = c("North", "South", "East", "West", "North", "South", "East"),
  stringsAsFactors = FALSE
)

new_orders <- data.frame(
  customer_name = c("John Doe", "John Doe", "Alice Johnson"),
  product = c("Laptop", "Mouse", "Headphones"),
  quantity = c(1, 2, 3),
  total_price = c(1099.99, 39.98, 30.99),
  region = c("North", "North", "East"),
  stringsAsFactors = FALSE
)

# Combine the original data frame with the new orders
orders_df <- rbind(orders_df, new_orders)

# Print the updated data frame
print(orders_df)

# Add a date column
set.seed(123)  # For reproducibility
start_date <- as.Date("2024-01-01")
orders_df$date <- seq.Date(from = start_date, by = "day", length.out = nrow(orders_df))

# Print the updated orders_df
print(orders_df)

# Initialize a vector to store total sales for each region
regions <- unique(orders_df$region)
total_sales <- numeric(length(regions))
total_sales
# Calculate total sales for each region
for (i in seq_along(regions)) {
  region_sales <- sum(orders_df$total_price[orders_df$region == regions[i]])
  total_sales[i] <- region_sales
}

# Create a named vector for better readability
names(total_sales) <- regions
print(total_sales)

# HIGH VALUE ORDERS
# Initialize an empty list to store high-value orders
high_value_orders_list <- list()
index <- 1

# Identify high-value orders
for (i in 1:nrow(orders_df)) {
  if (orders_df$total_price[i] > 100) {
    high_value_orders_list[[index]] <- orders_df[i, ]
    index <- index + 1
  }
}

# ---------------------- HIGH VALUE ORDERS -----------------------
#Convert list to data frame
# Initialize an empty list to store high-value orders
high_value_orders_list <- list()
index <- 1

# Identify high-value orders
for (i in 1:nrow(orders_df)) {
  if (orders_df$total_price[i] > 100) {
    high_value_orders_list[[index]] <- orders_df[i, ]
    index <- index + 1
  }
}

# Convert list to data frame
high_value_orders_df <- do.call(rbind, high_value_orders_list)

# Order the data frame by total_price in descending order
high_value_orders_df <- high_value_orders_df[order(-high_value_orders_df$total_price), ]

print(high_value_orders_df)

#---------- CUSTOMER LOYALTY ----------------------------------

# Get unique customer names
unique_customers <- unique(orders_df$customer_name)

# Loop through each customer to check their order count
for (customer in unique_customers) {
  
  # Initialize counter for each customer
  order_count <- 0
  index <- 1
  
  # Count the number of orders for this customer
  while (index <= nrow(orders_df)) {
    if (orders_df$customer_name[index] == customer) {
      order_count <- order_count + 1
    }
    index <- index + 1
  }
  
  # Print message based on the number of orders
  if (order_count > 1) {
    print(paste(customer, "is a loyal customer with", order_count, "orders."))
  } else if (order_count == 1) {
    print(paste(customer, "has only one order."))
  } else {
    print(paste(customer, "has no orders."))
  }
}

#-------POPULAR PRODUCT---------
# Initialize a table to store product counts
product_counts <- table(orders_df$product)

# Find the most popular product
most_popular_product <- names(which.max(product_counts))

# Print product counts
print(product_counts)

# Print the most popular product along with the count
print(paste("The most popular product is", most_popular_product, "with", product_counts[most_popular_product], "purchases."))

#---------- MORE ANALYSIS ------------
# ----------- Calculate the average order value for each region
average_order_value_by_region <- tapply(orders_df$total_price, orders_df$region, mean)

# Print the average order values by region
print(average_order_value_by_region)

# Find the region with the highest average order value
highest_avg_region <- names(which.max(average_order_value_by_region))

# Find the region with the lowest average order value
lowest_avg_region <- names(which.min(average_order_value_by_region))

# Print the regions with the highest and lowest average order values
print(paste("The region with the highest average order value is", highest_avg_region, "with an average of", average_order_value_by_region[highest_avg_region]))
print(paste("The region with the lowest average order value is", lowest_avg_region, "with an average of", average_order_value_by_region[lowest_avg_region]))

# Recommendation for improving sales in the lowest-performing region
cat("Recommendation: For the", lowest_avg_region, "region, consider promotional offers, targeted marketing campaigns, or improving customer service to boost sales.\n")

#------Categorise customers by their spending habits
# Initialize a new column in orders_df to store customer categories
orders_df$customer_category <- NA

# Define spending thresholds for categories
gold_threshold <- 500
silver_threshold <- 200

# Calculate total spending for each customer
customer_spending <- tapply(orders_df$total_price, orders_df$customer_name, sum)

# Loop through each row and assign customer categories based on their spending
for (i in 1:nrow(orders_df)) {
  customer_total <- customer_spending[orders_df$customer_name[i]]
  
  if (customer_total > gold_threshold) {
    orders_df$customer_category[i] <- "Gold"
  } else if (customer_total > silver_threshold) {
    orders_df$customer_category[i] <- "Silver"
  } else {
    orders_df$customer_category[i] <- "Bronze"
  }
}

# Print the updated data frame with customer categories
print(orders_df)
high_value_orders_df <- high_value_orders_df[order(-high_value_orders_df$total_price), ]

print(high_value_orders_df)

# Bar plot of total sales by region
ggplot(data = orders_df, aes(x = region, y = total_price, fill = region)) +
  geom_bar(stat = "summary", fun = "sum") +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales ($)") +
  theme_minimal()

# Scatter plot of total price vs. quantity
ggplot(data = orders_df, aes(x = quantity, y = total_price, color = product)) +
  geom_point(size = 3) +
  labs(title = "Total Price vs. Quantity Ordered", x = "Quantity", y = "Total Price ($)") +
  theme_minimal()

# Add a date column for visualization
set.seed(123)  # For reproducibility
orders_df$date <- seq(as.Date("2024-01-01"), by = "day", length.out = nrow(orders_df))

# Line plot of total sales over time
ggplot(data = orders_df, aes(x = date, y = total_price, color = region)) +
  geom_line() +
  labs(title = "Total Sales Over Time", x = "Date", y = "Total Sales ($)") +
  theme_minimal()



