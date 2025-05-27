install.packages("factoextra")
library("factoextra")

mydata=read.csv("dataset/USArrest.csv")
head(mydata)
tail(mydata)
str(mydata)
mydata$rownames

usdata=scale(mydata[,-1])
usdata

kmeans.cluster_2=kmeans(usdata,centers=2,nstart=20)
kmeans.cluster_2
kmeans.cluster_3=kmeans(usdata,centers=3,nstart=20)
kmeans.cluster_3
kmeans.cluster_4=kmeans(usdata,centers=4,nstart=20)
kmeans.cluster_4
kmeans.cluster_5=kmeans(usdata,centers=5,nstart=20)
kmeans.cluster_5

kmeans.cluster_2$cluster
kmeans.cluster_3$cluster
kmeans.cluster_4$cluster
kmeans.cluster_5$cluster

fviz_cluster(kmeans.cluster_2,data=usdata)
fviz_cluster(kmeans.cluster_3,data=usdata)
fviz_cluster(kmeans.cluster_4,data=usdata)
fviz_cluster(kmeans.cluster_5,data=usdata)

fviz_nbclust(usdata,FUN=kmeans,method="silhouette")
