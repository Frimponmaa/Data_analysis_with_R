library(readxl)
library(tidyverse)
library(readxl)
library(conflicted)
library(summarytools)
library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape2)
library(pivot)
library(tidyr)
Walmart <- read_excel("Downloads/Data Technician/R Notes/Walmart.xlsx", 
                      +     col_types = c("numeric", "date", "numeric", 
                                          +         "numeric", "numeric", "numeric", 
                                          +         "numeric", "numeric"))
df<-Walmart
# Summary of the data
summary(df)
summary(is.na(df))

# Calculate descriptive statistics by store
descriptive_stats_by_store <- df %>%
  group_by(Store) %>%
  summarise(
    Mean_Weekly_Sales = mean(Weekly_Sales, na.rm = TRUE),
    Median_Weekly_Sales = median(Weekly_Sales, na.rm = TRUE),
    SD_Weekly_Sales = sd(Weekly_Sales, na.rm = TRUE),
    Mean_Fuel_Price = mean(Fuel_Price, na.rm = TRUE),
    Median_Fuel_Price = median(Fuel_Price, na.rm = TRUE),
    SD_Fuel_Price = sd(Fuel_Price, na.rm = TRUE),
    Mean_CPI = mean(CPI, na.rm = TRUE),
    Median_CPI = median(CPI, na.rm = TRUE),
    SD_CPI = sd(CPI, na.rm = TRUE),
    Mean_Unemployment = mean(Unemployment, na.rm = TRUE),
    Median_Unemployment = median(Unemployment, na.rm = TRUE),
    SD_Unemployment = sd(Unemployment, na.rm = TRUE)
  )
head(descriptive_stats_by_store)
View(descriptive_stats_by_store)

#SALES DISTRIBUTION ACROSS STORES
# Boxplot of weekly sales by store
ggplot(df, aes(x=factor(Store), y=Weekly_Sales)) +
  geom_boxplot(fill="orange", outlier.color="red") +
  labs(x="Store Number", y="Weekly Sales", title="Distribution of Weekly Sales Across Stores") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#SALES TRENDS ON HOLIDAYS AND NON-HOLIDAYS
# Average sales by store during holiday and non-holiday weeks
store_holiday_sales <- df %>%
  group_by(Store, Holiday_Flag) %>%
  summarise(Avg_Weekly_Sales = mean(Weekly_Sales))

# Plot the sales
ggplot(store_holiday_sales, aes(x=factor(Store), y=Avg_Weekly_Sales, fill=factor(Holiday_Flag))) +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Store", y="Average Weekly Sales", fill="Holiday flag", title="Average Weekly Sales by Store for Holidays and Hon-holidays") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#SPECIFIC HOLIDAY IMPACT ON SALES
# Define the dates for each specific holiday
# If Date is not in Date format, convert it
df$Date <- as.Date(df$Date, format="%Y-%m-%d")

super_bowl <- as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08"))
labour_day <- as.Date(c("2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06"))
thanksgiving <- as.Date(c("2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29"))
christmas <- as.Date(c("2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27"))

# Add a new column for specific holidays
df$Specific_Holiday <- case_when(
  df$Date %in% super_bowl ~ "Super Bowl",
  df$Date %in% labour_day ~ "Labour Day",
  df$Date %in% thanksgiving ~ "Thanksgiving",
  df$Date %in% christmas ~ "Christmas",
  TRUE ~ "None"
)

head(df)

# Compare average sales between holiday and non-holiday weeks
avg_sales <- df %>%
  group_by(Holiday_Flag) %>%
  summarise(Avg_Weekly_Sales = mean(Weekly_Sales))

# Plot the average sales
ggplot(avg_sales, aes(x=factor(Holiday_Flag), y=Avg_Weekly_Sales)) +
  geom_bar(stat="identity", fill="skyblue") +
  labs(x="Holiday Week", y="Average Weekly Sales", title="Average Weekly Sales: Holiday vs Non-Holiday Weeks")

# Analyze sales for each specific holiday
holiday_sales <- df %>%
  filter(Specific_Holiday != "None") %>%
  group_by(Specific_Holiday) %>%
  summarise(Average_Sales = mean(Weekly_Sales))

# Plot the holiday sales
ggplot(holiday_sales, aes(x=Specific_Holiday, y=Average_Sales)) +
  geom_bar(stat="identity", fill="lightgreen") +
  labs(x="Holiday", y="Average Sales", title="Average Sales During Specific Holidays")

#SALES TRENDS OVER TIME
# Monthly sales trends
# Add a new column with the numeric month
df$Month <- as.numeric(format(df$Date, "%m"))

# Add a new column with the month name
df$Month_Name <- format(df$Date, "%B")

# Print the modified data frame
print(df$Month)

# Aggregate data by Month and Month_Name
monthly_data <- df %>%
  group_by(Month, Month_Name) %>%
  summarise(Average_Sales = mean(Weekly_Sales, na.rm = TRUE))

# Create a line plot of average monthly sales
ggplot(monthly_data, aes(x = Month, y = Average_Sales, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = 1:12, labels = month.name) +  # Use month names for x-axis
  labs(title = "Average Monthly Sales",
       x = "Month",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Calculate average weekly sales if you have multiple entries per week
# Assuming 'Date' is already a weekly date, otherwise, you might need to adjust the aggregation
sales_trend <- df %>%
  group_by(Date) %>%
  summarise(Average_Sales = mean(Weekly_Sales, na.rm = TRUE))

# Create line plot of average weekly sales over time
ggplot(sales_trend, aes(x = Date, y = Average_Sales)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +  # Smooth line to show trend
  labs(title = "Weekly Sales Trends Over Time",
       x = "Date", y = "Average Weekly Sales") +
  theme_minimal()


#CORRELATION ANALYSIS FOR KEY METRICS
# Calculate the correlation matrix and reshape it for plotting
cor_matrix <- cor(df[, c("Weekly_Sales", "Temperature", "Fuel_Price", "CPI", "Unemployment")])
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap with correlation coefficients
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name="Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + # Add correlation coefficients
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Correlation Heatmap", x="", y="")

#Prepare the data for plotting
df_long <- df %>%
  pivot_longer(cols = c(Temperature, Fuel_Price, CPI, Unemployment), 
               names_to = "Variable", 
               values_to = "Value")

# Create scatter plots with facet_wrap
ggplot(df_long, aes(x = Value, y = Weekly_Sales)) +
  geom_point(color = "blue", alpha = 0.6) +                 # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear trend line
  facet_wrap(~ Variable, scales = "free_x") +               # Facet by variable
  labs(title = "Weekly Sales vs Key metrics", y = "Weekly Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#negative and no correlations but there are clusters

# Convert Date column to Date format
df$Date <- ymd(df$Date)  # Use dmy() if the date format is day-month-year, otherwise use ymd() for year-month-day

# Extract year and month from Date
df <- df %>%
  mutate(Year = year(Date), Month = month(Date, label = TRUE))

# Aggregate CPI by year
yearly_cpi <- df %>%
  group_by(Year) %>%
  summarise(Average_CPI = mean(CPI, na.rm = TRUE))

# Plot CPI trend by Year
ggplot(yearly_cpi, aes(x = Year, y = Average_CPI)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average CPI by Year",
       x = "Year",
       y = "Average CPI") +
  theme_minimal()


# Aggregate CPI by month
monthly_cpi <- df %>%
  group_by(Month) %>%
  summarise(Average_CPI = mean(CPI, na.rm = TRUE))

# Plot CPI trend by Month
ggplot(monthly_cpi, aes(x = Month, y = Average_CPI, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Average CPI by Month",
       x = "Month",
       y = "Average CPI") +
  theme_minimal() +
  scale_x_discrete(limits = month.abb)  # Use month.abb to order months correctly

#Sales over time and highlight specific holiday impacts
# Ensure the Date column is in Date format
data <-df
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Initialise all as Non-Holiday
data$Holiday_Label <- "Non-Holiday"

# Assign specific labels for each holiday
data$Holiday_Label[data$Date %in% as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08"))] <- "Super Bowl"
data$Holiday_Label[data$Date %in% as.Date(c("2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06"))] <- "Labour Day"
data$Holiday_Label[data$Date %in% as.Date(c("2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29"))] <- "Thanksgiving"
data$Holiday_Label[data$Date %in% as.Date(c("2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27"))] <- "Christmas"

# Convert Holiday_Label to factor
data$Holiday_Label <- factor(data$Holiday_Label, 
                             levels = c("Non-Holiday", "Super Bowl", "Labour Day", "Thanksgiving", "Christmas"))

# Check if labels were correctly assigned
table(data$Holiday_Label)

# Compute weekly average sales including holiday labels
weekly_avg <- data %>%
  group_by(Date, Holiday_Label) %>%
  summarize(Average_Sales = mean(Weekly_Sales, na.rm = TRUE))

View(weekly_avg)
# Create line plot with holiday labels
ggplot(weekly_avg, aes(x = Date, y = Average_Sales, color = Holiday_Label)) +
  geom_line(size = 1) +
  labs(title = "Average Weekly Sales Over Time by Holiday Label", 
       x = "Date", 
       y = "Average Sales") +
  scale_color_manual(values = c("Non-Holiday" = "lightblue", 
                                "Super Bowl" = "pink", 
                                "Labour Day" = "green", 
                                "Thanksgiving" = "orange", 
                                "Christmas" = "purple")) +
  theme_minimal() +
  theme(legend.title = element_blank())



