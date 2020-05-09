# Xinyi Zhao  10451738
# CS513 HW8

rm(list = ls())

filename<-file.choose()
BreastCancer<-read.csv(filename)
#remove the ID
data<-BreastCancer[,-1]

#remove rows with any missing value(s) first.
data1<-na.omit(data)

#hclust
data_dist<-dist(data1[,-1])
hclust_resutls<-hclust(data_dist)
plot(hclust_resutls)
hclust_2<-cutree(hclust_resutls,2)
hclust_2
table(hclust_2,data1[,1])

#kmeans
kmeans_2<- kmeans(data1[,-1],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,data1[,1])
