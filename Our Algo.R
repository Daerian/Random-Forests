" 
Hyperparameters
================
m - Number of predictors to use for constructing trees
B - Number of trees chosen for each bootstrap sample
"


################################# LIBRARIES AND MISCELLANEOUS ######################################
rm(list = ls())

libraries = c('tidyverse','rpart','rpart.plot','robustbase','dplyr')
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  } else if (!(lib %in% (.packages()))){
    library(lib, character.only = TRUE)
  }
}

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
wineData$quality  = as.factor(wineData$quality)
wineData$Type = as.factor(wineData$Type)


##################################### SUMMARY STATISTICS ########################################

wineData %>% group_by(Type) %>% summarize(n=n())
wineData %>% group_by(quality) %>% summarize(n=n())
hist.chlorides = hist(wineData$chlorides)
hist.ph = hist(wineData$pH)
hist.citricac = hist(wineData$citric_acid)
hist.ressugar = hist(wineData$residual_sugar)

##################################### BUILD A TREE ##############################################

bt = function() {
  sample.wineData = sample_n(wineData, nrow(wineData), replace=TRUE)
  sample.p.wineData = sample(sample.wineData[,c(-12,-13)], 4, replace=FALSE)
  param = paste("quality ~", paste(names(sample.p.wineData), collapse=" + "))
  sample.p.wineData$quality = sample.wineData$quality
  column.names = colnames(sample.p.wineData)
  trees=rpart(formula=param, data=sample.p.wineData, method='class')
  rpart.plot(trees)
  ret = list()
  ret[[1]] = trees
  ret[[2]] = sample.p.wineData
  ret[[3]] = anti_join(wineData, sample.wineData)
  return(ret)
}

forest = list()
for (i in 1:10) {
  forest[[i]] = bt()
}
