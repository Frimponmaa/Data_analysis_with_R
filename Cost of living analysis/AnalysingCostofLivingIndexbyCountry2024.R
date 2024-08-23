library(ggplot2)
library(dplyr)
library(tidyverse)
#library(summarytools)
library(readr)
library(corrplot)
library(ggcorrplot)
library(factoextra)
library(cluster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(pheatmap)

cost_of_living <- read_csv("Downloads/Data Technician/R Notes/Cost_of_Living_Index_by_Country_2024.csv")
View(cost_of_living)
#summary(cost_of_living)

#Calculate the average cost of living index
average_cost_living <- mean(cost_of_living$`Cost of Living Index`, na.rm = TRUE)
print(paste("The average cost of living index is:", round(average_cost_living,2)))

# Find the country with the highest cost of living index
highest_cost_country <- cost_of_living %>%
  filter(`Cost of Living Index` == max(`Cost of Living Index`, na.rm = TRUE)) %>%
  select(Country, `Cost of Living Index`)
print("Country with the highest cost of living index:")
print(highest_cost_country)

# Find the country with the lowest cost of living index
lowest_cost_country <- cost_of_living %>%
  filter(`Cost of Living Index` == min(`Cost of Living Index`, na.rm = TRUE)) %>%
  select(Country, `Cost of Living Index`)
print("Country with the lowest cost of living index:")
print(lowest_cost_country)

# Create a bar plot of the top 10 countries with the highest cost of living indices.
# 1. Select the top 10 countries with the highest cost of living index
top_10_countries <- cost_of_living %>%
  arrange(desc(`Cost of Living Index`)) %>%
  head(10)

# 2.  Plot the top 10 countries
ggplot(top_10_countries, aes(x = reorder(Country, `Cost of Living Index`), y = `Cost of Living Index`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries with Highest Cost of Living Index (2024)",
       x = "Country",
       y = "Cost of Living Index") +
  theme_minimal()

# Correlation analysis
# 1. Select only the numerical columns for correlation analysis
# Exclude the 'Country' column if present
numerical_data <- cost_of_living %>%
  select(-Country)

# 2. Calculate the correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# 3. Visualize the correlation matrix using corrplot
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("red", "white", "blue"), 
           title = "Correlation Matrix of Cost of Living Indices",
           ggtheme = theme_minimal())
# Cluster analysis
# 1. Select only the numerical columns for correlation analysis
# Exclude the 'Country' column if present
numerical_data <- cost_of_living %>%
  select(-Country)

# 2. Standardize the data to that ensures all variables contribute equally
scaled_data <- scale(numerical_data)

# 3. Determine the optimal number of clusters using the Elbow Method
set.seed(123)  # For reproducibility
fviz_nbclust(scaled_data, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Method for Determining Optimal Number of Clusters")

# 4. Perform K-means clustering with the chosen number of clusters (e.g., 4)
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)

# 5. Add the cluster assignments to the original data
cost_of_living$Cluster <- as.factor(kmeans_result$cluster)

# 6. Visualize the clusters
fviz_cluster(kmeans_result, data = scaled_data, 
             geom = "point", ellipse.type = "convex", 
             ggtheme = theme_minimal()) +
  labs(title = "K-means Clustering of Countries based on Cost of Living Indices")

# 7. Inspect the clusters
cluster_summary <- cost_of_living %>%
  group_by(Cluster) %>%
  summarise_all(mean)

print(cluster_summary)

# PCA
# Standardize the data for PCA
scaled_data <- scale(numerical_data)

# Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# Print summary of PCA result to see explained variance
summary(pca_result)

# Print PCA loadings 
#Higher absolute values indicate a stronger contribution of the original variable to the principal component.
print(pca_result$rotation)

# Convert PCA scores to a data frame for plotting
pcascores <- as.data.frame(pca_result$x)
pca_scores
# Visualize the variance explained by each principal component
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100)) +
  labs(title = "Scree Plot: Variance Explained by Principal Components")
# Visualize PCA (2D plot)
fviz_pca_biplot(pca_result, 
                geom = "point", 
                repel = TRUE, 
                title = "PCA Biplot of Cost of Living Data") +
  theme_minimal()

# Visualize the distribution of indices geographically.
# 1. Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Merge your data with the world map
map_data <- world %>%
  left_join(cost_of_living, by = c("name" = "Country"))

# 3. Plot the Cost of Living Index on the world map
ggplot(data = map_data) +
  geom_sf(aes(fill = `Cost of Living Index`), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Cost of Living Index") +
  theme_minimal() +
  labs(title = "Global Cost of Living Index", 
       subtitle = "Cost of Living Index by Country",
       caption = "Source: Your Data Source") +
  theme(legend.position = "bottom"
  )        
        
# Example for Rent Index
ggplot(data = map_data) +
  geom_sf(aes(fill = `Rent Index`), color = "black", size = 0.1) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", name = "Rent Index") +
  theme_minimal() +
  labs(title = "Global Rent Index", 
       subtitle = "Rent Index by Country",
       caption = "Source: Your Data Source") +
  theme(legend.position = "bottom")

# Reshape the data to long format
cost_of_living_long <- cost_of_living %>%
  select(`Cost of Living Index`, `Rent Index`, `Cost of Living Plus Rent Index`, 
         `Groceries Index`, `Restaurant Price Index`, `Local Purchasing Power Index`) %>%
  pivot_longer(cols = everything(), names_to = "Index", values_to = "Value")

# Plot boxplots for each index in a single plot
ggplot(cost_of_living_long, aes(x = Index, y = Value, fill = Index)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Cost of Living Index" = "skyblue", 
                               "Rent Index" = "lightgreen", 
                               "Cost of Living Plus Rent Index" = "lightcoral", 
                               "Groceries Index" = "lightyellow", 
                               "Restaurant Price Index" = "lightpink", 
                               "Local Purchasing Power Index" = "lightblue")) +
  theme_minimal() +
  labs(title = "Boxplots of Various Indices", 
       x = "Index", 
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

