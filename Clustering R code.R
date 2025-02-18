# Load necessary library
library(cluster)

# Read the dataset
rfm_data <- read.csv("dataset_online_retail.csv")

# Scale the RFM scores (if not already normalized)
rfm_scaled <- scale(rfm_data[, c("REC.SCR.normalized", "FRQ.SCR.normallized", "MON.SCR.normalized")])

# Remove rows with NA, NaN, or Inf values
rfm_scaled_clean <- na.omit(rfm_scaled)  # Removes rows with NA values
rfm_scaled_clean <- rfm_scaled_clean[!is.nan(rfm_scaled_clean)]  # Removes NaN values
rfm_scaled_clean <- rfm_scaled_clean[!is.infinite(rfm_scaled_clean)]  # Removes Inf values

# Determine the optimal number of clusters using the Elbow Method
wss <- sapply(1:10, function(k) {
  kmeans(rfm_scaled_clean, k, nstart=25)$tot.withinss
})

# Plot the Elbow Method graph
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within-Cluster Sum of Squares")

# Choose the optimal number of clusters (e.g., k=4 based on the elbow point)
set.seed(123)  # For reproducibility
k <- 4  # Replace with the optimal number of clusters
kmeans_result <- kmeans(rfm_scaled_clean, k, nstart=25)

# Add cluster labels to the original dataset
rfm_data_clean <- rfm_data[complete.cases(rfm_data), ]  # Remove rows with NA values in the original data
rfm_data_clean$Cluster <- kmeans_result$cluster

# View the clustered data
print(rfm_data_clean)

# Save the clustered data to a new CSV file
write.csv(rfm_data_clean, "rfm_clustered_clean.csv", row.names=FALSE)