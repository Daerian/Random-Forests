" 
Hyperparameters
================
m - Number of predictors to use for constructing trees
B - Number of trees chosen for each bootstrap sample
"


################################# LIBRARIES AND MISCELLANEOUS ######################################
rm(list = ls())

require(tidyverse)
require(rpart)
require(rpart.plot)

library(tidyverse)
library(rpart)
library(rpart.plot)


################################### READ AND CLEAN DATA ############################################

redWineData = read_delim("winequality-red.csv", delim = ";")
whiteWineData = read_delim("winequality-white.csv", delim = ";")

# remove NA values
redWineData = (redWineData[complete.cases(redWineData),])
whiteWineData = (whiteWineData[complete.cases(whiteWineData),])
# Prep Sets for merginglibrary
redWineData = redWineData %>% mutate(Type = "Red")
whiteWineData = whiteWineData %>% mutate (Type = "White")

# Merge Data and get Final Dataset
wineData = rbind(redWineData,whiteWineData)
# Remove spaces from column names
names(wineData) = gsub(" ","_", names(wineData))



##################################### BUILD A TREE ##############################################

bt = function() {
  sample.wineData = sample_n(wineData, 1000, replace=TRUE)
  sample.p.wineData = sample(sample.wineData[,c(-12,-13)], 4, replace=FALSE)
  param = paste("quality ~", paste(names(sample.p.wineData), collapse=" + "))
  sample.p.wineData$quality = sample.wineData$quality
  column.names = colnames(sample.p.wineData)
  trees=rpart(formula=param, data=sample.p.wineData, method='class')
  #rpart.plot(trees)
  return(trees)
}


# Forest Algo
forest = function(num_trees){
  forest=list()
  for (i in 1:num_trees){
    forest[[i]] = bt()
  }
  return (forest)
}

# 
trees2 = forest(3)
rpart.plot(trees2[[2]])


