#Load the needed libraries
library(MASS)
library(mvtnorm)
library(ggplot2)

Mean1 = c(15,80)
Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(1, 1000)
class1 = cbind(mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1), class)

Mean2 = c(30,120)
Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(2, 1000)
class2 = cbind(mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2), class)

# When using real data, may need to remove na by na.omit(data)
myData = data.frame(rbind(class1, class2))
colnames(myData) = c("patientAge", "patientWeight", "classification")
myData$classification=factor(myData$classification)
write.csv(myData, "Example1.csv")


# Set seed for reproducibility
set.seed(123)

# Define the means and covariance matrices for the two clusters
mean1 <- c(0, 0)
mean2 <- c(-5, -5)
cov_matrix1 <- matrix(c(1, -0.5, -0.5, 1), ncol = 2)
cov_matrix2 <- matrix(c(1, -0.5, -0.5, 1), ncol = 2)


# Generate random points for the two clusters
n <- 1000
cluster1 <- rmvnorm(n, mean = mean1, sigma = cov_matrix1)
cluster2 <- rmvnorm(n, mean = mean2, sigma = cov_matrix2)

# Combine the clusters into a single data frame
data <- rbind(cluster1, cluster2)
colnames(data) <- c("x", "y")
data <- as.data.frame(data)
data$Cluster <- factor(rep(c("Cluster 1", "Cluster 2"), each = n))

# Plot the clusters
ggplot(data, aes(x = x, y = y, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Randomly Generated Clusters",
       x = "X-axis",
       y = "Y-axis") +
  theme_minimal()

