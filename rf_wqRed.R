library(tidyverse)
library(randomForest)
library(caret)

#Get Data
wqRed_data <- read_delim('winequality-red.csv', delim=";")
wqRed_data$quality <- as.factor(wqRed_data$quality)
wqRed_data <- wqRed_data[complete.cases(wqRed_data), ] #Remove incomplete rows
names(wqRed_data) <- make.names(names(wqRed_data)) #Convert column names to legal format

#Divide into train and test
set.seed(2)
id <- sample(2, nrow(wqRed_data), prob = c(0.7, 0.3), replace = TRUE)
wqRed_train <- wqRed_data[id == 1, ]
wqRed_test <- wqRed_data[id == 2, ]

#Create Model
bestmtry <- tuneRF(wqRed_train, wqRed_train$quality, stepFactor = 1.2, improve = 0.01, trace = T, plot = T) #To use this, need installed.packages("randomForest")
wqRed_forest <- randomForest(quality~., data = wqRed_train)
wqRed_forest
wqRed_forest$importance
varImpPlot(wqRed_forest)

#Validation
pred1_wqRed <- predict(wqRed_forest, newdata = wqRed_test, type = "class") #All of our predictions
confusionMatrix(table(pred1_wqRed, wqRed_test$quality)) #Need install "caret" and install.packages('e1071', dependencies=TRUE)

#wqRed_data[!complete.cases(wqRed_data),] to find partial rows
#Column: Actual
#Row: Predicted
#Source: https://www.youtube.com/watch?v=gmmV4drPTS4