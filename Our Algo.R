" 
Hyperparameters
================
m - Number of predictors to use for constructing trees
B - Number of trees chosen for each bootstrap sample
"

################################# LIBRARIES AND MISCELLANEOUS ######################################
rm(list = ls())

"
Please add all required libraries to the 'libraries' vector of strings below. All libraries in the
vector of strings will be installed and/or loaded on runtime if not already installed/loaded.
"
libraries = c('tidyverse','rpart','rpart.plot','robustbase','dplyr', 'dbplyr')

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

"
'bt' function returns a list of objects. The objects are returned in the following order:
  [1] Classification tree
  [2] Sampled parameters combined with bootstrap sample
  [3] All observations NOT used in building the tree (i.e. all observations not in [2])

To access these objects, here is a sample code:
bt.return = bt()
# To access the classification tree
bt.return[[1]]
# To access the sampled predictors & bootstrapped sample
bt.return[[2]]
# To access all observations not used in sampled predictors & bootstrapped sample
bt.return[[3]]
"
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
  
  # TODO Confirm if this outputs the correct data frame with all observations that are NOT used
  #      in generating the classification tree for a specific bootstrap sample.
  ret[[2]] = anti_join(wineData, sample.wineData)
  ret[[3]] = trees[["variable.importance"]]
  return(ret)
}

forest = list()
obs = list()
pars = list()
for (i in 1:10) {
  forest[[i]] = bt()
  obs[[i]] = forest[[i]][2]
}

ind = 0
for (parameter in param){
  ind = ind+1
  tot = 0
  mag = 0
  for (tree in forest){
    for (i in 1:length(tree[[3]])){
      if (names(tree[[3]][i]) == parameter){
        tot = tot + tree[[3]][[i]]
        mag = mag + 1
      }
    }
  }
  
  est[[ind]]= tot/mag
}

names(est) = param