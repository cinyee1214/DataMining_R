# Xinyi Zhao  10451738
# CS513 HW5

rm(list = ls())

# Important: make sure your categories are represented by the “factor” data type in R 
# and DO NOT replace the missing values.  

file <- file.choose()
data <-  read.csv(file,
                na.strings = "/Users/cinyee/Desktop/Stevens/2020spring/513/DataMiningHW/breast-cancer-wisconsin.data.csv",
                colClasses=c("Sample"="character",
                             "F1"="factor","F2"="factor","F3"="factor",
                             "F4"="factor","F5"="factor","F6"="factor",
                             "F7"="factor","F8"="factor","F9"="factor",
                             "Class"="factor"))


#install.packages("rpart")
#install.packages("rpart.plot")     
#install.packages("rattle")         
#install.packages("RColorBrewer")    
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

index <- sort(sample(nrow(data), round(.30 * nrow(data))))
training <- data[-index, ]
test <- data[index, ]

?rpart()
#Grow the tree 

CART_class <- rpart(Class~., data = training[ ,-1])
rpart.plot(CART_class)
CART_predict2 <- predict(CART_class, test, type = "class")
df <- as.data.frame(cbind(test, CART_predict2))
table(Actual = test[ , "Class"], CART = CART_predict2)

CART_wrong <- sum(test[ , "Class"] != CART_predict2)

error_rate = CART_wrong / length(test$Class)

dev.off()