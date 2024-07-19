# Load necessary package
library(nnet)

# Generate a sample dataset
set.seed(123)
n <- 100
p <- 2
X <- data.frame(matrix(rnorm(n * p), ncol = p))
colnames(X) <- c("Feature1", "Feature2")
y <- factor(ifelse(X$Feature1 + X$Feature2 + rnorm(n) > 0, 1, 0))
data <- data.frame(X, Label = y)

# Split data into training and test sets
train_idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# Normalize the data
maxs <- apply(train_data, 2, max)
mins <- apply(train_data, 2, min)
scaled_train_data <- as.data.frame(scale(train_data, center = mins, scale = maxs - mins))
scaled_test_data <- as.data.frame(scale(test_data, center = mins, scale = maxs - mins))

# Train the neural network
nn_model <- nnet(Label ~ ., data = train_data, size = 3, maxit = 200, linout = FALSE)
print(nn_model)
# Print the model summary
summary(nn_model)

# Predict on the test set
nn_predictions <- predict(nn_model, scaled_test_data, type = "class")

# Evaluate the performance
confusion_matrix <- table(test_data$Label, nn_predictions)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / length(test_data$Label)
print(paste("Accuracy:", round(accuracy, 2)))
