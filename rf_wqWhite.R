library(tidyverse)
library(randomForest)
library(caret)

#Get Data
wqWhite_data <- read_delim('winequality-white.csv', delim=";")
wqWhite_data$quality <- as.factor(wqWhite_data$quality)
wqWhite_data <- wqWhite_data[complete.cases(wqWhite_data), ] #Remove incomplete rows
names(wqWhite_data) <- make.names(names(wqWhite_data)) #Convert column names to legal format

#Divide into train and test
set.seed(2)
id <- sample(2, nrow(wqWhite_data), prob = c(0.7, 0.3), replace = TRUE)
wqWhite_train <- wqWhite_data[id == 1, ]
wqWhite_test <- wqWhite_data[id == 2, ]

#Create Model
bestmtry <- tuneRF(wqWhite_train, wqWhite_train$quality, stepFactor = 1.2, improve = 0.01, trace = T, plot = T) #To use this, need installed.packages("randomForest")
wqWhite_forest <- randomForest(quality~., data = wqWhite_train)
wqWhite_forest
wqWhite_forest$importance
varImpPlot(wqWhite_forest)

#Validation
pred1_wqWhite <- predict(wqWhite_forest, newdata = wqWhite_test, type = "class") #All of our pWhiteictions
confusionMatrix(table(pred1_wqWhite, wqWhite_test$quality)) #Need install "caret" and install.packages('e1071', dependencies=TRUE)

#wqWhite_data[!complete.cases(wqWhite_data),] to find partial rows
#Column: Actual
#Row: PWhiteicted
#Source: https://www.youtube.com/watch?v=gmmV4drPTS4