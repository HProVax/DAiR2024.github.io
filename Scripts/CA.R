library(ggplot2)
# Generate a sample dataset
set.seed(123)
n <- 100
p <- 2
X <- data.frame(matrix(rnorm(n * p), ncol = p))
colnames(X) <- c("Feature1", "Feature2")

# Perform K-means clustering
k <- 2
kmeans_result <- kmeans(X, centers = k, nstart = 25)

# Print the clustering result
print(kmeans_result)

# Add the cluster assignment to the dataset
X$Cluster <- as.factor(kmeans_result$cluster)

# Plot the clusters
ggplot(X, aes(x = Feature1, y = Feature2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = paste("K-means Clustering with", k, "Clusters"),
       x = "Feature 1", y = "Feature 2") +
  theme_minimal()

# Generate a sample dataset
set.seed(123)
n <- 100
p <- 2
X <- data.frame(matrix(rnorm(n * p), ncol = p))
colnames(X) <- c("Feature1", "Feature2")

# Calculate the distance matrix
dist_matrix <- dist(X)

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hclust_result, labels = FALSE, main = "Hierarchical Clustering Dendrogram")


# library(ggplot2)
library(cluster)
library(factoextra)

# Generate a sample dataset
set.seed(123)
n <- 100
p <- 2
X <- data.frame(matrix(rnorm(n * p), ncol = p))
colnames(X) <- c("Feature1", "Feature2")

# Elbow method to find the optimal number of clusters
wcss <- numeric(15)
for (k in 1:15) {
  kmeans_result <- kmeans(X, centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}

# Plot the WCSS for the elbow method
plot(1:15, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Method for Optimal Number of Clusters")

# Silhouette method to find the optimal number of clusters
avg_sil_width <- numeric(15)
for (k in 2:15) {
  kmeans_result <- kmeans(X, centers = k, nstart = 25)
  sil <- silhouette(kmeans_result$cluster, dist(X))
  avg_sil_width[k] <- mean(sil[, 3])
}

# Plot the average silhouette width for the silhouette method
plot(2:15, avg_sil_width[2:15], type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Average Silhouette Width",
     main = "Silhouette Method for Optimal Number of Clusters")

# Use factoextra to determine the optimal number of clusters using the elbow method
fviz_nbclust(X, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

# Use factoextra to determine the optimal number of clusters using the silhouette method
fviz_nbclust(X, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

library(MASS)
Mean1 = c(1,1)
Sigma1 = matrix(c(1, 0.5, 0.5, 2), nrow=2)
class = rep(1, 1000)
class1 = cbind(mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1), class)

Mean2 = c(5,5)
Sigma2 = matrix(c(1, -0.5, -0.5, 2), nrow=2)
class = rep(2, 1000)
class2 = cbind(mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2), class)

Mean3 = c(10,10)
Sigma3 = matrix(c(1, 0.5, 0.5, 2), nrow=2)
class = rep(3, 1000)
class3 = cbind(mvrnorm(n=1000, mu=Mean3, Sigma = Sigma3), class)

# When using real data, may need to remove na by na.omit(data)
myData = data.frame(rbind(class1, class2, class3))
colnames(myData) = c("feature1", "feature2", "classification")
summary(myData)
library(plotly)
myPlot = plot_ly(myData, type="scatter", mode="markers",
                 x=~feature1, y=~feature2, color=~classification)
myPlot = myPlot %>% layout(title = 'Patient Response')
myPlot

wcss <- numeric(15)
for (k in 1:15) {
  kmeans_result = kmeans(myData[,-3], centers = k, nstart = 50)
  wcss[k] = kmeans_result$tot.withinss
}

# Plot the WCSS for the elbow method
plot(1:15, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Method for Optimal Number of Clusters")

kmeans_result = kmeans(myData[,-3], centers = 3, nstart = 25)
myData$cluster <- factor(kmeans_result$cluster)
centers <- as.data.frame(kmeans_result$centers)
centers$cluster <- factor(1:nrow(centers))
myPlot = plot_ly(myData, type="scatter", mode="markers",
                 x=~feature1, y=~feature2, color=~cluster)

myPlot = myPlot %>% add_trace(
  data = centers,
  x = ~feature1,
  y = ~feature2,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 5, symbol = 'x', color = 'red'),
  name = 'Cluster Centers'
)
myPlot = myPlot %>% layout(title = 'Clusters')
myPlot

