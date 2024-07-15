#Load the needed libraries
library(MASS)
library(ggplot2)
library(plotly)

x = seq(-pi,pi,0.1)
plot(x, sin(x), col="green")
points(x, cos(x))
lines(x, cos(x))

par(mfrow=c(2,2))
plot(x, sin(x), "l", col="red")
plot(x, cos(x), "b", col="blue")

par(mfrow=c(1,1))
x = rnorm(1000)
y = x + rnorm(1000)
plot(x, y)
abline(lm(y ~ x), col = "red") #Add linear model


#Generate data from a multivariate normal distribution with strong correlation. 
#The final response is a linear combination of the predictors.
ID = 1:1000
myMean = c(47,160)
mySigma = matrix(c(40, 35, 35, 100), nrow=2)
myData = mvrnorm(n=1000, mu=myMean, Sigma = mySigma)
colnames(myData) = c("patientAge", "patientWeight")
patientResponse = 0.3*(myData[,"patientAge"]-47)^2 - 0.5*(myData[,"patientWeight"]-160)^2 + 45 + rnorm(1000, mean=0, sd=50)
patientData = data.frame(ID,myData, patientResponse)
# plot(-30:30, 100:160, type="n")
plot(x=patientData$patientAge[1:500], y=patientData$patientWeight[1:500],
     main="An Example Plot", sub="Subtitle", xlab="Age", ylab="Weight", "p", col="red",
     pch=20)
points(x=patientData$patientAge[501:1000], y=patientData$patientWeight[501:1000], 
     main="An Example Plot", sub="Subtitle", xlab="Age", ylab="Response", "p", col="red",
     pch=23)
legend("topleft",                              # Add legend to plot
       legend = c("Group 1", "Group 2"),
       col = "red",
       pch = c(20,23))

pairs(patientData)

smoothScatter(patientData$patientAge, y=patientData$patientWeight)

#Histogram
hist(patientData$patientAge, breaks=20, col="darkmagenta", border="pink", freq=FALSE)

#Density plot
myDensity = density(patientData$patientAge)
plot(myDensity, col="blue")
polygon(myDensity, col = "magenta")

# The following segment of code copied from https://r-charts.com/distribution/fill-area-density/
# Data
set.seed(3)
x <- rnorm(200)
y <- rnorm(150, mean = 1)

# Density estimations
denx <- density(x)
deny <- density(y)

# Plot
plot(denx,
     ylim = c(0, max(c(denx$y, deny$y))),
     xlim = c(min(c(denx$x, deny$x)),
              max(c(denx$x, deny$x))))
lines(deny)

# Fill the areas
polygon(denx, col = rgb(0.78, 0.89, 1, alpha = 0.6))
polygon(deny, col = rgb(0.51, 0.44, 1, alpha = 0.6)) 

# Simple Pie Chart
slices = c(10, 12,4, 16, 8)
myLabels = c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = myLabels, main="Pie Chart of Countries")

# Plotting with Plotly
myPlot = plot_ly(data=patientData, x=~patientAge, y=~patientWeight, type="scatter")
myPlot

myPlot = plot_ly(data=patientData, x=~patientAge, y=~patientWeight, type="scatter", 
                 marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', 
                               line = list(color = 'rgba(152, 0, 0, .8)', width = 1)))
myPlot
myPlot %>% layout(title = "My modified plot")

myPlot2 = plot_ly(patientData, x=~ID)
myPlot2
myPlot2 = myPlot2 %>% add_trace(y = ~patientAge, name="Patient Age", mode = 'lines')
myPlot2
myPlot2 = myPlot2 %>% add_trace(y = ~patientWeight, name="Patient Weight", mode = 'lines+markers')
myPlot2

# myPlot = plot_ly(patientData, type="scatter3d", mode="markers",  
#                  x=~patientAge, y=~patientWeight, z=~patientResponse, size=I(10))
# myPlot = myPlot %>% layout(title = 'Patient Response')
# myPlot
