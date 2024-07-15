#Load the needed libraries
library(MASS)
library(ggplot2)
library(factoextra)

Mean1 = c(15,80)
Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
cluster1 = mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1)

Mean2 = c(60,160)
Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
cluster2 = mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2)

# When using real data, may need to remove na by na.omit(data)
myData = data.frame(rbind(cluster1, cluster2))
colnames(myData) = c("patientAge", "patientWeight")
pairs(myData)

#Explain nstart and the returned components of kmeans (e.g. totss, within ss)
#What should happen to totss, etc. as clusters move?
#What should happen to % as #centers increase?
#Although not performed here, generally it is a good idea to scale data with scale(data)
clusters2 = kmeans(myData, centers=2, nstart=10) 
summary(clusters2)
print(clusters2) #Note WcSS and between_SS/total_SS line
plot(myData, col=clusters2$cluster)
fviz_cluster(clusters2, myData, ellipse = TRUE, ellipse.type = "norm", labelsize=0)

clusters5 = kmeans(myData, centers=20, nstart=10) #Explain nstart
summary(clusters5)
print(clusters5) #Note WcSS and between_SS/total_SS line
plot(myData, col=clusters5$cluster)
fviz_cluster(clusters5, myData)

fviz_nbclust(myData, kmeans, method="wss")
fviz_nbclust(myData, kmeans, method="silhouette")
# fviz_nbclust(myData, kmeans, method="gap_stat")

myDistances=get_dist(myData)
fviz_dist(myDistances)

hClusters = hclust(myDistances, method="average")
plot(hClusters)
