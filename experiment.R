source("randomforest.R")
library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)

# Read red wine and white wine data from their respective filess
redWineData = read_delim("winequality-red.csv", delim = ";")
whiteWineData = read_delim("winequality-white.csv", delim = ";")

# Remove NA values
redWineData = (redWineData[complete.cases(redWineData),])
whiteWineData = (whiteWineData[complete.cases(whiteWineData),])

# Prep Sets for merging library
redWineData = redWineData %>% mutate(Type = "Red")
whiteWineData = whiteWineData %>% mutate (Type = "White")

# Merge Data and get Final Dataset
wineData = rbind(redWineData,whiteWineData)

# Remove spaces from column names
names(wineData) = gsub(" ","_", names(wineData))

# Convert columns ot factor as needed and remove not needed columns
wineData$quality  = as.factor(wineData$quality)
wineData$Type = as.factor(wineData$Type)
wineData = wineData[,-ncol(wineData)]

seed = 0
M = 0
R2 = 0

for (i in 1:100) {
  set.seed(i)

  # Split data into training data and testing data, 50% and 50% respectively
  split = sample(2, nrow(wineData), prob=c(0.75,0.25), replace=TRUE)
  training_set = wineData[split==1,]
  testing_set = wineData[split==2,]
  
  # Acquire labels for the training set
  training_labels = as.numeric(as.character(training_set$quality))
  
  # Acquire labels for the testing set
  testing_labels = as.numeric(as.character(testing_set$quality))
  
  training_set = training_set[,-ncol(training_set)]
  testing_set = testing_set[,-ncol(testing_set)]
  
  
  for (m in 1:5) {
  # This is the number of trees we want to create
  B = 500
  
  f = PerformRegression(training_set,training_labels,testing_set,testing_labels,B,m)
    if (f[[2]] > R2 & (M != m | seed != i)) {
      R2 = f[[2]]
      M = m
      seed = i
    }
  }
}