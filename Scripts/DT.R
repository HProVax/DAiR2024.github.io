# Load required package
library(rpart)
library(rpart.plot)
# Generate a sample dataset
set.seed(123)
# n = 100
# p = 2
# X = data.frame(matrix(rnorm(n * p), ncol = p))
# colnames(X) = c("Feature1", "Feature2")
# y = factor(sample(c('A', 'B'), n, replace = TRUE))
# data = data.frame(X, Label = y)
# 
# # Split data into training and test sets
# train_idx = sample(1:n, size = round(0.7 * n), replace = FALSE)
# train_data = data[train_idx, ]
# test_data = data[-train_idx, ]
# write.csv(train_data, "train_data.csv")
# 
# 
# # Create a decision tree model
# tree_model = rpart(Label ~ ., data = train_data, method = "class")
# 
# # Print the decision tree model
# print(tree_model)
# 
# # Plot the decision tree
# # plot(tree_model)
# # text(tree_model, pretty = 5)
# 
# library(rpart.plot)
# rpart.plot(tree_model)
# rpart.rules(tree_model, cover=TRUE)
# 
# # Predict on the training set
# y_pred = predict(tree_model, train_data, type = "class")
# 
# # Evaluate the performance
# confusion_matrix = table(train_data$Label, y_pred)
# print(confusion_matrix)
# accuracy = sum(diag(confusion_matrix)) / length(train_data$Label)
# print(paste("Accuracy:", round(accuracy, 2)))
# 
# # Predict on the test set
# y_pred = predict(tree_model, test_data, type = "class")
# 
# # Evaluate the performance
# confusion_matrix = table(test_data$Label, y_pred)
# print(confusion_matrix)
# accuracy = sum(diag(confusion_matrix)) / length(test_data$Label)
# print(paste("Accuracy:", round(accuracy, 2)))
# 
# # library(rattle)
# # library(RColorBrewer)
# # fancyRpartPlot(tree_model, main="Fancy DT")

library(MASS)
Mean1 = c(15,80)
Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(1, 1000)
class1 = cbind(mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1), class)

Mean2 = c(15,110)
Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(2, 1000)
class2 = cbind(mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2), class)

# When using real data, may need to remove na by na.omit(data)
myData = data.frame(rbind(class1, class2))
colnames(myData) = c("feature1", "feature2", "classification")
plot(myData, col=myData$classification)
write.csv(myData, "myData.csv")

summary(myData)
library(plotly)
# myPlot = plot_ly(myData, type="scatter", mode="markers",
#                  x=~feature1, y=~feature2, color=~classification)
# myPlot = myPlot %>% add_trace(myData, type="scatter", mode="markers", symbol='+',
#                               x=~feature1, y=~feature2, color=~classification)
# myPlot = myPlot %>% layout(title = 'Patient Response')
# myPlot

train_idx = sample(1:2000, size = round(0.7 * 2000), replace = FALSE)
train_data = myData[train_idx, ]
test_data = myData[-train_idx, ]
plot(train_data[,1:2], col=train_data[,3], pch=16)
points(test_data[,1:2], col=blues9, pch = 3)

param = rpart.control(maxdepth = 30, minsplit = 2, cp = 0.01)
tree_model = rpart(classification ~ ., data = train_data, method = "class", 
                   control=param)
rpart.plot(tree_model)
rpart.rules(tree_model, cover=TRUE)

# Evaluate the performance on the training set
y_pred = predict(tree_model, train_data, type = "class")
confusion_matrix = table(train_data$classification, y_pred)
print(confusion_matrix)
accuracy = sum(diag(confusion_matrix)) / length(train_data$classification)
print(paste("Accuracy:", round(accuracy, 2)))

# Evaluate the performance on the testing set
y_pred = predict(tree_model, test_data, type = "class")
confusion_matrix = table(test_data$classification, y_pred)
print(confusion_matrix)
accuracy = sum(diag(confusion_matrix)) / length(test_data$classification)
print(paste("Accuracy:", round(accuracy, 2)))

library(factoextra)
pcaData = prcomp(myData, center = TRUE, scale. = TRUE)
param = rpart.control(maxdepth = 30, minsplit = 2, cp = 0.01)
tree_model = rpart(classification ~ ., data = data.frame(pcaData$x), method = "class", 
                   control=param)
rpart.plot(tree_model)
rpart.rules(tree_model, cover=TRUE)
