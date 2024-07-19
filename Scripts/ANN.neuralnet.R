# install.packages("neuralnet")
library(neuralnet)

# Generate a sample dataset
set.seed(123)
n <- 100
p <- 2
X <- data.frame(matrix(rnorm(n * p), ncol = p))
colnames(X) <- c("Feature1", "Feature2")
y <- ifelse(X$Feature1 + X$Feature2 + rnorm(n) > 0, 1, 0)
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
# formula <- Label ~ Feature1 + Feature2
nn_model <- neuralnet(Label ~ ., data = scaled_train_data, hidden = c(3), linear.output = FALSE)

print(nn_model$error)
# Plot the neural network
plot(nn_model)

# Predict on the test set
nn_predictions <- compute(nn_model, scaled_test_data[, -3])
predicted_labels <- ifelse(nn_predictions$net.result > 0.5, 1, 0)

library(ggplot2)
# Extract the error at each step
error_df <- data.frame(Step = 1:length(nn_model$net.result[[1]]), Error = nn_model$net.result[[1]])
sum(error_df)
ggplot(error_df, aes(x = Step, y = Error)) +
  geom_line(color = "blue") +
  labs(title = "Neural Network Training Loss", x = "Step", y = "Error") +
  theme_minimal()

# Evaluate the performance
confusion_matrix <- table(test_data$Label, predicted_labels)
confusion_matrix = confusionMatrix(test_data$Label, predicted_labels)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / length(test_data$Label)
print(paste("Accuracy:", round(accuracy, 2)))

