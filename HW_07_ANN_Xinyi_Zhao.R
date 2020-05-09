# Xinyi Zhao  10451738
# CS513 HW7

rm(list = ls())

filename<-file.choose()
BreastCancer<-read.csv(filename)
#remove the ID
data<-BreastCancer[,-1]
data1<-na.omit(data)
diagnosis<-ifelse(data$diagnosis=='M',1,0)
data2<-data.frame(diagnosis,data1[,-1])
str(data2)

#training and test
index<-seq(1,nrow(data2),by=5)
test<-data2[index,]
training<-data2[-index,]

library("neuralnet")
net_data<-neuralnet(diagnosis~.,training,hidden = 5,threshold = 0.01)
plot(net_data)

#only the input column
ann<-compute(net_data,test[,-1])
ann$net.result

ann_cat<-ifelse(ann$net.result<0.2,0,1)
table(Predict=ann_cat,Actual=test$diagnosis)

#error rate
wrong<-(test$diagnosis!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate
