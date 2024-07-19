# Load necessary packages
library(randomForest)
library(randomForestExplainer)
library(ggplot2)
library(MASS)

# Generate a sample dataset
set.seed(123)

# Generate the data fro Class 1
Mean1 = c(15,80)
Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
class = factor(rep(1, 1000))
class1 = cbind(mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1), factor(class, levels=(c(1,2))))

# Generate the data fro Class 2
Mean2 = c(15,110)
Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
class = factor(rep(2, 1000))
class2 = cbind(mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2), factor(class, levels=(c(1,2))))

# Merge the data into one data frame and name the columns
myData = data.frame(rbind(class1, class2))
colnames(myData) = c("feature1", "feature2", "class")
myData$class = factor(myData$class, levels=(c(1,2)))

# Visual inspection of the data and capture into a file
summary(myData)
plot(myData[,1:2], col=myData[,3], pch=16)
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

# Create a random forest model
rf_model <- randomForest(class ~ ., data = train_data, ntree=100)

# Print the random forest model summary
print(rf_model)

# Plot the error rate as the number of trees increases
plot(rf_model, main = "Random Forest Error Rates")

# Visualize variable importance
var_imp <- importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance Plot")

# Use randomForestExplainer to get more insights
min_depth_frame <- min_depth_distribution(rf_model)
plot_min_depth_distribution(min_depth_frame)

importance_frame <- measure_importance(rf_model)
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

