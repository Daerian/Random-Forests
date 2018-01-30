" 
Hyperparameters
================
m - Number of predictors to use for constructing trees
B - Number of trees chosen for each bootstrap sample
"

library(tidyverse)
<<<<<<< HEAD
<<<<<<< HEAD
library(randomForest)
library(caret)

#Get Data
wq_data <- read_delim('winequality-red.csv', delim=";")
wq_data$quality <- as.factor(wq_data$quality)
wq_data <- wq_data[complete.cases(wq_data), ] #Remove incomplete rows
names(wq_data) <- make.names(names(wq_data)) #Convert column names to legal format

#Divide into train and test
set.seed(2)
id <- sample(2, nrow(wq_data), prob = c(0.7, 0.3), replace = TRUE)
wq_train <- wq_data[id == 1, ]
wq_test <- wq_data[id == 2, ]

#Create Model
bestmtry <- tuneRF(wq_train, wq_train$quality, stepFactor = 1.2, improve = 0.01, trace = T, plot = T) #To use this, need installed.packages("randomForest")
wq_forest <- randomForest(quality~., data = wq_train)
wq_forest
wq_forest$importance
varImpPlot(wq_forest)

#Validation
pred1_wq <- predict(wq_forest, newdata = wq_test, type = "class") #All of our predictions
confusionMatrix(table(pred1_wq, wq_test$quality)) #Need install "caret" and install.packages('e1071', dependencies=TRUE)

#wq_data[!complete.cases(wq_data),] to find partial rows
#Column: Actual
#Row: Predicted
#Source: https://www.youtube.com/watch?v=gmmV4drPTS4
=======
=======
>>>>>>> kevin
library(rattle)
library(rpart.plot)
library(RColorBrewer)

RandForest <- function (Z, labels, m, B) {
  bootstrap <- BSSample(sample(Z, m, replace=FALSE))
}

BSSample <- function(Z) {
  bssamp <- sample_n(Z, nrow(Z), replace=TRUE)
  return(bssamp)
}

CompPredictErr <- function() {
<<<<<<< HEAD
  # TODO: Implement loss function
}

>>>>>>> kevin
=======
  # TODO: Loss function
}
>>>>>>> kevin
