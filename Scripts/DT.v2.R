# Load required package
library(rpart)
library(rpart.plot)
library(MASS)

# Generate a sample dataset
set.seed(123)

# Generate the data fro Class 1
Mean1 = c(15,80)
Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(1, 1000)
class1 = cbind(mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1), class)

# Generate the data fro Class 2
Mean2 = c(15,110)
Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
class = rep(2, 1000)
class2 = cbind(mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2), class)

# Merge the data into one data frame and name the columns
myData = data.frame(rbind(class1, class2))
colnames(myData) = c("feature1", "feature2", "classification")

# Visual inspection of the data and capture into a file
summary(myData)
plot(myData[,1:2], col=myData$classification)
write.csv(myData, "myData.csv")
# From here on, we can use read.csv to load the data

# This is too pretty to delete
# library(plotly)
# myPlot = plot_ly(myData, type="scatter", mode="markers",
#                  x=~feature1, y=~feature2, color=~classification)
# myPlot = myPlot %>% add_trace(myData, type="scatter", mode="markers", symbol='+',
#                               x=~feature1, y=~feature2, color=~classification)
# myPlot = myPlot %>% layout(title = 'Patient Response')
# myPlot

# Create the training and testing set by a 70/30 split rule
train_idx = sample(1:2000, size = round(0.7 * 2000), replace = FALSE)
train_data = myData[train_idx, ]
test_data = myData[-train_idx, ]
plot(train_data[,1:2], col=train_data[,3], pch=16)
points(test_data[,1:2], col=blues9, pch = 3)

# Some example DT hyper parameters
param = rpart.control(maxdepth = 2, minsplit = 2, cp = 0.0001)
# Train a classification DT using the parameters from above
tree_model = rpart(classification ~ ., data = train_data, method = "class", 
                   control=param)
# Visualize the tree
rpart.plot(tree_model)
#rpart.rules(tree_model, cover=TRUE)

# Evaluate the performance on the training set (should be high)
y_pred = predict(tree_model, train_data, type = "class")
confusion_matrix = table(train_data$classification, y_pred)
print(confusion_matrix)
accuracy = sum(diag(confusion_matrix)) / length(train_data$classification)
print(paste("Accuracy:", round(accuracy, 2)))

# Evaluate the performance on the testing set (should be comparable to the training)
y_pred = predict(tree_model, test_data, type = "class")
confusion_matrix = table(test_data$classification, y_pred)
print(confusion_matrix)
accuracy = sum(diag(confusion_matrix)) / length(test_data$classification)
print(paste("Accuracy:", round(accuracy, 2)))

# Question for the curious minds: what happens if we repeat this exercise on the
# PCA transformation of the same data?
library(factoextra)
pcaData = prcomp(myData, center = TRUE, scale. = TRUE)

