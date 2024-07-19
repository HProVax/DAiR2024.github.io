library(neuralnet)
Input = Data_5d_2c_E3[,2:7]
dOutput = Data_5d_2c_E3[,7]
Input$class = Input$class -1

# Can perform data scaling

# Check to see if your data is balanced
# If not, use SMOTE to balance the dataset
# balanced_data = SMOTE(class ~ ., data, perc.over = 200, perc.under = 200)
hist(as.numeric(Input$class))


# Split data into training and test sets
train_idx = sample(1:2000, size = round(0.7 * 2000), replace = FALSE)
train_data = Input[train_idx, ]
test_data = Input[-train_idx, ]

# Check to see if your data is balanced
# If not, use SMOTE to balance the dataset
# balanced_data = SMOTE(class ~ ., data, perc.over = 200, perc.under = 200)


# Train the neural network
# formula <- Label ~ Feature1 + Feature2
nn_model <- neuralnet(class ~ ., data = train_data, hidden = c(3), linear.output = FALSE)

# Plot the neural network
plot(nn_model)

# Predict on the test set
nn_predictions <- compute(nn_model, test_data[,-6])
predicted_labels <- ifelse(nn_predictions$net.result > 0.5, 1, 0)

library(caret)
confusion_matrix = confusionMatrix(as.factor(test_data$class),
                                   as.factor(predicted_labels))     
confusion_matrix
