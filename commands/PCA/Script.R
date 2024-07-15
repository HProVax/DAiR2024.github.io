#Load the needed libraries
library(MASS)
library(ggplot2)
library(factoextra)

Mean1 = c(0,0)
Sigma1 = matrix(c(10, 0, 0, 10), nrow=2)
tempData = mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1)
Data1 = data.frame(tempData)
colnames(Data1) = c("Var1", "Var2")
# pairs(Data1, xlim=c(-10, 10), ylim=c(-10,10))

Mean1 = c(0,0)
Sigma2 = matrix(c(10, 9, 9, 10), nrow=2)
tempData = mvrnorm(n=1000, mu=Mean1, Sigma = Sigma2)
Data2 = data.frame(tempData)
colnames(Data2) = c("Var1", "Var2")
# pairs(Data2, xlim=c(-10, 10), ylim=c(-10,10))

par(mfrow=c(2,2))
stripchart(Data1$Var1)
stripchart(Data2$Var1)
stripchart(Data1$Var2)
stripchart(Data2$Var2)

par(mfrow=c(2,2))
hist(Data1$Var1)
hist(Data1$Var2)
hist(Data2$Var1)
hist(Data2$Var2)

# Mean1 = c(15,80)
# Sigma1 = matrix(c(40, 45, 45, 100), nrow=2)
# cluster1 = mvrnorm(n=1000, mu=Mean1, Sigma = Sigma1)
# 
# Mean2 = c(25,135)
# Sigma2 = matrix(c(40, 45, 45, 100), nrow=2)
# cluster2 = mvrnorm(n=1000, mu=Mean2, Sigma = Sigma2)
# 
# myData = data.frame(rbind(cluster1, cluster2))
# colnames(myData) = c("patientAge", "patientWeight")
# pairs(myData)
# stripchart(myData$patientAge)
# stripchart(myData$patientWeight)

myPCA1 <- prcomp(Data1, center = FALSE, scale = FALSE)
summary(myPCA1)
# str(myPCA)
fviz_eig(myPCA1)
# https://www.datacamp.com/community/tutorials/pca-analysis-r
# fviz_pca_ind(myPCA1,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# fviz_pca_var(myPCA1,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
eigVal1 = get_eigenvalue(myPCA1)
transData1 = get_pca_var(myPCA1)
transIndData1 = get_pca_ind(myPCA1)
pairs(Data1, , xlim=c(-10, 10), ylim=c(-10,10))
pairs(transIndData1$coord, xlim=c(-10, 10), ylim=c(-10,10))

myPCA2 <- prcomp(Data2, center = FALSE, scale = FALSE)
summary(myPCA2)
# str(myPCA2)
fviz_eig(myPCA2)
# fviz_pca_ind(myPCA2,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# fviz_pca_var(myPCA2,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
eigVal2 = get_eigenvalue(myPCA2)
transData2 = get_pca_var(myPCA2)
transIndData2 = get_pca_ind(myPCA2)
pairs(Data2, xlim=c(-10, 10), ylim=c(-10,10))
pairs(transIndData2$coord, xlim=c(-10, 10), ylim=c(-10,10))
hist(transIndData2$coord[,1])
hist(transIndData2$coord[,2])
