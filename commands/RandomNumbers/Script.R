uData = runif(1000, min=1, max=10)
nData = rnorm(1000, mean=0, sd = 2)

myData=cbind(uData, nData)
write.csv(myData, "myData.csv")


summary(uData)
summary(nData)

hist(uData)
hist(nData)

hist(nData, breaks=20)

hist(nData, main="Some Other Title", xlab = "My X labels", ylab = "My Y Labels", col = "pink")
