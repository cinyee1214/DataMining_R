# Xinyi Zhao  10451738
# CS513 HW6

rm(list = ls())

# Important: make sure your categories are represented by the “factor” data type in R and DO NOT replace the missing values.  
BreastCancer<-read.csv("/Users/cinyee/Desktop/Stevens/2020spring/513/DataMiningHW/breast-cancer-wisconsin.data.csv")

#remove the ID
data<-BreastCancer[,-1]
str(data)

#convert all the integer data types to factor data type
data[sapply(data, is.integer)]<-lapply(data[sapply(data, is.integer)],as.factor)

#test and training
set.seed(123)
index<-sort(sample(nrow(data),round(.30*nrow(data))))
training<-data[-index,]
test<-data[index,]

library(randomForest)
fit<-randomForest(Class~.,data=training, importance=TRUE,ntree=1000)
importance(fit)
varImpPlot(fit)
prediction<-predict(fit,test)
table(actual=test[,10],prediction)
wrong<- (test[,10]!=Prediction )
errorRate<-sum(wrong,na.rm = TRUE)/length(wrong)
errorRate