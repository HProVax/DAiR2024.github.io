# Load required package
library(class)

# Generate a sample dataset
set.seed(123)
n <- 100
p <- 2
X <- matrix(rnorm(n * p), ncol = p)
y <- factor(sample(c('A', 'B'), n, replace = TRUE))

# Split data into training and test sets
train_idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

# Perform k-NN analysis
k <- 3
y_pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)

# Evaluate the performance
confusion_matrix <- table(y_test, y_pred)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / length(y_test)
print(paste("Accuracy:", round(accuracy, 2)))

library(MASS)
Mean1 = c(15,80)
Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(1, 1000)
class1 = cbind(mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1), class)

Mean2 = c(35,130)
Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(2, 1000)
class2 = cbind(mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2), class)

# When using real data, may need to remove na by na.omit(data)
myData = data.frame(rbind(class1, class2))
colnames(myData) = c("feature1", "feature2", "classification")

summary(myData)
plot(myData, col=myData$classification)
# library(plotly)
# myPlot = plot_ly(myData, type="scatter", mode="markers",
#                  x=~feature1, y=~feature2, color=~classification)
# myPlot = myPlot %>% add_trace(X_train, type="scatter", mode="markers", symbol='+',
#                               x=~feature1, y=~feature2, color=~classification)
# myPlot = myPlot %>% layout(title = 'Patient Response')
# myPlot

train_idx = sample(1:2000, size = round(0.7 * 2000), replace = FALSE)
X_train = myData[train_idx, -3]
y_train = myData[train_idx, 3]
X_test = myData[-train_idx, -3]
y_test = myData[-train_idx, 3]
plot(X_train, col=y_train, pch=16)
points(X_test, col=blues9, pch = 3)

k=5
y_pred = knn(train = X_train, test = X_test, cl = y_train, k = k)

# Evaluate the performance
confusion_matrix = table(y_test, y_pred)
print(confusion_matrix)
accuracy = sum(diag(confusion_matrix)) / length(y_test)
print(paste("Accuracy:", round(accuracy, 2)))

