# Load necessary library
library(mvtnorm)

# Set seed for reproducibility
set.seed(123)

# Define parameters for clusters
n_clusters = 2
n_dimensions = 5
n_points = 1000

# Create means for each cluster
# cluster_means = matrix(rnorm(n_clusters * n_dimensions, mean = 0, sd = 10), n_clusters, n_dimensions)
cluster_means = matrix(c(0,0,0,0,0, 0.5,0.5,0.5,0.5,0.5), n_clusters, n_dimensions, byrow = TRUE)

# Create covariance matrices for each cluster (identity matrix for simplicity)
cluster_covariances = lapply(1:n_clusters, function(i) diag(n_dimensions))

# Generate data for each cluster
cluster_data = lapply(1:n_clusters, function(i) {
  rmvnorm(n_points, mean = cluster_means[i, ], sigma = cluster_covariances[[i]])
})

# Combine all clusters into a single data frame
data = do.call(rbind, cluster_data)

# Convert to data frame and add cluster labels
data = as.data.frame(data)
data$cluster = rep(1:n_clusters, each = n_points)

# View the first few rows of the data
head(data)
plot(data[,1:5], col=data[,6])
colnames(data) = c("F1", "F2", "F3", "F4", "F5", "class")
write.csv(data, "Data.5d.2c.E5.csv")
