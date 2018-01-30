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

=======

library(tidyverse)
library(rpart)
library(rpart.plot)

=======

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

# Glimpse of the Data Set
glimpse(wineData)

names(wineData) = gsub(" ","_", names(wineData))

sample.wineData = sample_n(wineData, 1000, replace=TRUE)
sample.p.wineData = sample(sample.wineData[,c(-12,-13)], 4, replace=FALSE)
sample.p.wineData$quality = sample.wineData$quality
column.names = colnames(sample.p.wineData)
param = paste(names(sample.p.wineData)[5],"~", paste(names(sample.p.wineData)[-1], collapse=" + "))
trees=rpart(formula=param, data=sample.p.wineData, method='class')
plot(trees)
text(trees, use.n=TRUE)

