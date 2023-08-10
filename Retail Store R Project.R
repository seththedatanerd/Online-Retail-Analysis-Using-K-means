## Stage 1 - data landscape and pre-processing
install.packages("readxl")
install.packages("tidyverse")
library(readxl)
library(tidyverse)
data <- read_excel("Online Retail.xlsx", sheet = 1)

head(data)

summary(data)


colSums(is.na(data))

data <- na.omit(data)

sum(is.na(data))

str(data)

unique(data$InvoiceNo[grepl("^c", data$InvoiceNo)])





## Stage 2 Exploratory Data Analysis

data$InvoiceDate <- as.POSIXct(data$InvoiceDate, format = "%d/%m/%Y")

# Customer purchase patterns
transactions_per_customer <- data %>%
  group_by(CustomerID) %>%
  summarise(Transactions = n_distinct(InvoiceNo)) %>% 
  arrange(desc(Transactions))

head(transactions_per_customer)





# Popular products - most purchased items
popular_products <- data %>%
  group_by(Description) %>%
  summarise(NumPurchases = sum(Quantity)) %>%
  arrange(desc(NumPurchases)) %>%
  top_n(10, NumPurchases)



# Sales trends over time
sales_trends <- data %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(Quantity * UnitPrice)) %>% filter(TotalSales > 0) %>%
  arrange(InvoiceDate)



# Geographic distribution of customers
geographic_distribution <- data %>%
  group_by(Country) %>%
  summarise(NumCustomers = n_distinct(CustomerID)) %>%
  arrange(desc(NumCustomers)) %>% top_n(10, Country)

# Let's visualise all of the data we have created so far 

# Visualise the geographic distribution of customers
ggplot(data = geographic_distribution, aes(x = reorder(Country, -NumCustomers), y = NumCustomers)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Geographic Spread of Customers",
       x = "Country",
       y = "Number of Customers") + coord_flip()


# distribution of transactions per customer
ggplot(data = transactions_per_customer, aes(x = Transactions)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Transactions per Customer",
       x = "Number of Transactions",
       y = "Frequency") 

# lemme zoom into the histogram in a little more
ggplot(data = transactions_per_customer, aes(x = Transactions)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Transactions per Customer",
       x = "Number of Transactions",
       y = "Frequency") +
  scale_x_continuous(limits = c(1, 50))

# line graph of sales trends over time
ggplot(data = sales_trends, aes(x = InvoiceDate, y = TotalSales)) +
  geom_line(color = "black") +
  labs(title = "Sales Trends Over Time",
       x = "Invoice Date",
       y = "Total Sales (£)",
       subtitle = "Note: Time period - 01/12/2010 to 09/12/2011")

# top 10 most purchased products
ggplot(data = popular_products, aes(x = NumPurchases, y = Description)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  labs(title = "Top 10 Most Purchased Products",
       x = "Product Description",
       y = "Number of Purchases")


### Stage 3: we'll perform some k-means clustering to segment certain customers 
# based on their spending and purchases

spending_by_customer <- data %>% group_by(CustomerID) %>% summarise(TotalSpend = sum(Quantity*UnitPrice))

cluster_data <- merge(spending_by_customer, transactions_per_customer, by = "CustomerID")

head(cluster_data)

cluster_data[, c("TotalSpend", "Transactions")] = scale(cluster_data[, c("TotalSpend", "Transactions")])


selected_features <- cluster_data %>%
  select(TotalSpend, Transactions)



install.packages("factoextra")
library(factoextra)

# best number of clusters using the elbow method
set.seed(123) 
wss <- numeric(10)  # Within-cluster sum of squares
for (i in 1:10) {
  kmeans_model <- kmeans(selected_features, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow plot
plot(1:10, wss, type = "b", xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares")
# Elbow point here is 3 shown by our graph
optimal_k <- 3

# Perform k-means clustering with the optimal k value (in our case, 3)
kmeans_result <- kmeans(selected_features, centers = optimal_k)

# Add cluster assignments to the original dataset
cluster_data$Cluster <- kmeans_result$cluster

# Clusters on scatter plot
ggplot(data = cluster_data, aes(x = TotalSpend, y = Transactions, color = factor(Cluster))) +
  geom_point() +
  labs(title = "Customer Segmentation using K-Means Clustering",
       x = "Total Spending",
       y = "Number of Purchases",
       color = "Cluster")

# insights from the clusters
for (i in 1:optimal_k) {
  cluster_mean <- cluster_data %>%
    filter(Cluster == i) %>%
    summarise(avg_spending = mean(TotalSpend),
              avg_purchases = mean(Transactions))
  
  cat("Cluster", i, "– Average Spending:", cluster_mean$avg_spending,
      "- Average Purchases:", cluster_mean$avg_purchases, "\n")
}

