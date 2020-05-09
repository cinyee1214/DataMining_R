# Xinyi Zhao  10451738
# CS513 HW2


rm(list = ls())

# 1-Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the EDA analysis by:

setwd("/Users/cinyee/Desktop/Stevens/2020spring/513/HW")

data <- read.csv("breast-cancer-wisconsin.data.csv")
View(data)

# I.Summarizing each column (e.g. min, max, mean )
summary(data)

# II.Identifying missing values
# only F6 has missing value
data$F6 <- as.numeric(as.character(data$F6))
data[is.na(data$F6),]

summary(data)

# III.Replacing the missing values with the “mean” of the column.
data$F6[is.na(data$F6)] <- mean(data$F6, na.rm = TRUE)

summary(data)

# IV.Displaying the frequency table of “Class” vs. F6
table(data$Class, data$F6)

# V.Displaying the scatter plot of F1 to F6, one pair at a time
for(i in 2:6){
  for(j in (i + 1):7){
    plot(data[ , i],
         data[ , j],
         main = paste0("Scatter plot of F", i - 1, " and F", j - 1),
         xlab = paste0("F", i - 1),
         ylab = paste0("F", j - 1))
  }
}

# VI.Show histogram and box plot for columns F7 to F9
for(i in 8:10){
  hist(data[ , i],
       main = paste0("Frequency Histogram of F", i - 1),
       xlab = paste0("F", i - 1),
       xlim = c(0,10),
       ylim = c(0,400 + (i - 8) * 150))
  
  boxplot(data[ , i],
          main = paste0("Box plot for F", i - 1),
          xlab = paste0("F", i - 1))
}

# 2- Delete all the objects from your R- environment. Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
# Remove any row with a missing value in any of the columns.
rm(list = ls())
data <- read.csv("breast-cancer-wisconsin.data.csv")
View(data)

data[data == "?"] <- NA
data <- na.omit(data)
View(data)
summary(data)


