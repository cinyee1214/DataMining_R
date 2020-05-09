# Xinyi Zhao  10451738
# CS513 HW4

rm(list = ls())

setwd("/Users/cinyee/Desktop/Stevens/2020spring/513/DataMiningHW")

data <- read.csv("breast-cancer-wisconsin.data.csv")
# Delete the rows with missing value and ID.
data[data == "?"] <- NA
data <- na.omit(data)
data <- data[ , -1]
View(data)

# Use the Naïve Bayes methodology to develop a classification model for the Diagnosis.
data$Class <- as.factor(data$Class)
levels(data$Class) <- c("benign","malignant")
View(data)

summary(data)
prop.table(table(data$Class))

# Use 30% test 70% training data.
selected_index <- sample(nrow(data), as.integer(nrow(data) * 0.7))
training_data <- data[selected_index, ]
test_data <- data[-selected_index, ]

library(e1071)
library(caret)

# Naïve Bayeswith training and test
model <- naiveBayes(Class~., data = training_data)
predict_class <- predict(model,newdata = test_data)

# Confusion matrix for finding accuracy
conf_matrix <- table(predict_class, test_data$Class)
confusionMatrix(conf_matrix)

# Fitting the Naive Bayes model with all data
nBayes_class <- naiveBayes(Class~., data = data)

# Print the model summary
nBayes_class

# Prediction on the dataset
category_class <- predict(nBayes_class, data)
table(NBayes = category_class, Diagnosis = data$Class)

NB_wrong <- sum(category_class != data$Class)

NB_error_rate <- NB_wrong / length(category_class)
