library(rpart)
library(rpart.plot)
Input = Data_5d_2c_E5[,2:7]
dOutput = Data_5d_2c_E5[,7]
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
param = rpart.control(maxdepth = 2, minsplit = 2, cp = 0.01)
tree_model = rpart(class ~ ., data = train_data, method = "class",
                   control=param)
# Visualize the tree
rpart.plot(tree_model)
#rpart.rules(tree_model, cover=TRUE)

# Predict on the test set
predicted_labels = predict(tree_model, test_data, type = "class")


library(caret)
confusion_matrix = confusionMatrix(as.factor(test_data$class),
                                   as.factor(predicted_labels))     
confusion_matrix

