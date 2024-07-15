#A simulated case
#Simulate 1000 samples of the independent variable, parameter, variable, or predictor between 0 and 10
simX = runif(1000, min=0, max=10)
#Simulate 1000 instances of observations with added uniform noise between -4 and 4
simY = 3*simX + 5 + runif(1000, min=-1, max=1)

#Scatter plot using base plotting function
plot(simX, simY, "p")
#Perform the linear regression fitness
myFit = lm(simY ~ simX)
#Computer the outcomes using the linear model
comY = simX*myFit$coefficients[2] + myFit$coefficients[1]
difY = simY - comY
#Plot the best linear fit
lines(simX, myFit$fitted.values, col="red")

#Using experimental data
myData = read.csv("./heart.data.csv")
View(myData)
typeof(myData)
#Just confirming it is a dataframe
class(myData)
#Get the column names
names(myData)
#Remove the column X
myData$X = NULL
#Confirming the column X is eliminated
colnames(myData)

#Making sure there are no NAs - removing NAs
which(is.na(myData))
workingData = myData[complete.cases(myData),]

#Visual inspection of linearity for each dimensi on/predictor individually
plot(myData$biking, myData$heart.disease, "p")
plot(myData$smoking, myData$heart.disease, "p")

#Perfrom the linear regression
myFit <-lm(heart.disease ~ biking + smoking, data = myData)
myFit$coefficients
myFit$residuals
#comY = myFit$coefficients[1] + myFit$coefficients[2]*myData$biking + myFit$coefficients[3]*myData$smoking

#Creating interactive plots using Plotly
# library(ggplot2)
# library(plotly)
myPlot = plot_ly(myData, type="scatter3d", mode="markers",  
                 x=~biking, y=~smoking, z=~heart.disease, size=I(10))
myPlot = myPlot %>% layout(title = 'Daily habits')

myPlot

myFit <-glm(heart.disease ~ biking + smoking, data = myData, family = gaussian())



