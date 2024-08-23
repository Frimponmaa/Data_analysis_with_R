# Analysing two dimensional data
install.packages("forecast")
library(forecast)
install.packages("cluster")
library(cluster)

# Sales matrix with products (rows) and regions (columns)
sales_matrix <- matrix(
  c(1000, 1500, 1200, 1300,
    1100, 1400, 1150, 1250,
    1050, 1600, 1300, 1400),
  nrow=3, ncol=4, byrow=TRUE,
  dimnames=list(c("Product A", "Product B", "Product C"),
                c("Region 1", "Region 2", "Region 3", "Region 4"))
)

print(sales_matrix)

# Create a sample matrix with sales data
sales_data <- matrix(
  c(100, 200, 300,
    110, 210, 320,
    120, 220, 330,
    130, 230, 340),
  nrow=4, ncol=3,
  dimnames=list(c("Month 1", "Month 2", "Month 3", "Month 4"),
                c("Product A", "Product B", "Product C"))
)
print(sales_data)
# Calculate and print correlation matrix
cor_matrix <- cor(sales_data)
print(cor_matrix)

# Perform PCA to reduce dimensional reduction and identify key patterns in data to inform business strategies and decision-making.
pca_result <- prcomp(sales_data, center=TRUE, scale=TRUE)

# Print PCA summary to show variability in the data
summary(pca_result)

# Print PCA loadings 
#Higher absolute values indicate a stronger contribution of the original variable to the principal component.
print(pca_result$rotation)

# Convert PCA scores to a data frame for plotting
pca_scores <- as.data.frame(pca_result$x)
pca_scores

# Plot PCA scores for the first two principal components
plot(pca_scores$PC1, pca_scores$PC2, 
     xlab="Principal Component 1", 
     ylab="Principal Component 2",
     main="PCA Scores Plot",
     pch=19, col="blue")

# Example customer data
customer_data <- matrix(
  c(10, 20, 15,
    30, 40, 35,
    25, 30, 20,
    40, 50, 45),
  nrow=4, ncol=3,
  dimnames=list(c("Customer 1", "Customer 2", "Customer 3", "Customer 4"),
                c("Order 1", "Order 2", "Order 3"))
)
customer_data
# Perform k-means clustering
kmeans_result <- kmeans(customer_data, centers=2)
print(kmeans_result$cluster)
