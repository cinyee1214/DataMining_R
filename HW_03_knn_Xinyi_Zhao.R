# Xinyi Zhao  10451738
# CS513 HW3


rm(list = ls())

setwd("/Users/cinyee/Desktop/Stevens/2020spring/513/HW")

data <- read.csv("breast-cancer-wisconsin.data.csv")
View(data)

# Use the knn methodology (k=3,5 and 10) to develop a classification models for the Diagnosis. 

# Make sure your categories are represented by the “factor” data type in R 
# and delete the rows with missing value. 
data[data == "?"] <- NA
data <- na.omit(data)
data <- data[ , -1]
data$F6 <- as.integer(as.character(data$F6))
data$Class<-as.factor(data$Class)
levels(data$Class)<-c("benign","malignant")
View(data)

summary(data)

# Use 30% test 70% training data. 
selected_index <- sample(nrow(data), as.integer(nrow(data) * 0.7))
training_data <- data[selected_index,]
test_data <- data[-selected_index,]

# Use the knn methodology (k=3,5 and 10) to develop a classification models for the Diagnosis. 
library(kknn)
library(e1071)
library(caret)
# k = 3
knn_k3 <- kknn(formula = Class ~ ., training_data, test_data, k = 3, kernel = "rectangular")
fit <- fitted(knn_k3)
conf_matrix <- table(test_data$Class, fit)
confusionMatrix(conf_matrix)

# k = 5
knn_k5 <- kknn(formula = Class ~ ., training_data, test_data, k = 5, kernel = "rectangular")
fit <- fitted(knn_k5)
conf_matrix <- table(test_data$Class, fit)
confusionMatrix(conf_matrix)

# k = 10
knn_k10 <- kknn(formula = Class ~ ., training_data, test_data, k = 10, kernel = "rectangular")
fit <- fitted(knn_k10)
conf_matrix <- table(test_data$Class, fit)
confusionMatrix(conf_matrix)








