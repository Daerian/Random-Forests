################################# LIBRARIES AND MISCELLANEOUS ######################################
rm(list = ls())

"
All required libraries to the 'libraries' vector of strings below. All libraries in the
vector of strings will be installed and/or loaded on runtime if not already installed/loaded.
"
libraries = c('tidyverse','rpart','rpart.plot','robustbase','dplyr', 'dbplyr', 'AppliedPredictiveModeling',
              'datasets','mlbench')

for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  } else if (!(lib %in% (.packages()))){
    library(lib, character.only = TRUE)
  }
}

######################################### CONSTANTS #############################################

# A function to set the constants needed for our algorithm in other places,
# dat - the data frame
Constant_Set = function(dat) {
  m = ncol(dat) / 2
  col_nam = colnames(dat)
  ret = list()
  ret[[1]] = m
  ret[[2]] = col_nam
  return (ret)
}


##################################### BUILD A TREE / FOREST ######################################

"
REQUIREMENTS
==============
1. (a) dat - Data frame that contains the predictors and the classes in the last column
(b) p - Number of parameters to be sampled


DESCRIPTION
============
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
BT_Tree = function(dat, labels, p, tree.print=FALSE) {
  dat[,colnames(labels)] = labels
  last_col = ncol(dat)
  pred_name = paste(colnames(dat)[ncol(dat)],paste(" ~"))
  sample.dat = sample_n(dat, nrow(dat), replace=TRUE)
  sample.p.dat = sample(sample.dat[,-last_col], p, replace=FALSE)
  param = paste(pred_name, paste(names(sample.p.dat), collapse = " + "))
  sample.p.dat[,colnames(dat)[ncol(dat)]] = sample.dat[,last_col]
  trees = rpart(formula=param, data=sample.p.dat, method='class')
  if (tree.print) {
    rpart.plot(trees)
  }
  ret = list()
  ret[[1]] = trees
  ret[[2]] = anti_join(dat, sample.dat, by=names(dat))
  ret[[3]] = trees[["variable.importance"]]
  return(ret)
}


# Returns a forest
Get_Forest = function(dat, labels, B, p){
  forest = list()
  for (i in 1:B) {
    forest[[i]] = BT_Tree(dat, labels, p)
  }
  return(forest)
}

############################################ CLASSIFICATION ######################################

"
This function takes as input:
forest = a random Forest, ie a list of trees which have been trained via a regression model.
obs - dataframe an observation to predict  -- predictors only.

The function will take the predictor values for this observation, and return to the user 
the classification for the random forest provided in the input as 'forest' 
"
Classify = function(forest, obs){
  predictions = 0 # will add all the predictions, then divide by numTrees to get average
  numTrees = length(forest)
  obs = as.data.frame(obs)
  i = 0
  # This for loop will add the predictions of every tree together, so they can be aggregated
  for (i in 1:numTrees){
    predictions = predict(forest[[i]][[1]], obs, type = "class")
  }
  return (predictions)
}

Loss = function(predicts, labels){
  loss =  1 - as.numeric(predicts == labels)
  error = sum(loss) / length(labels)
  return (error)
}


####################################### REGRESSION ##############################################

Regress = function(forest,obs){
  predictions = 0 # will add all the predictions, then divide by numTrees to get average
  numTrees = length(forest)
  obs = as.data.frame(obs)
  i = 0
  #This for loop will add the predictions of every tree together, so they can be aggregated
  for (i in 1:numTrees){
    predictions = predictions + predict(forest[[i]][[1]], obs, type = "vector")
  }
  
  predictions = predictions/numTrees
  #predictions = predictions + predict(forest[[1]][[1]], obs, type = "vector")
  return (predictions)
}

"
A function that calculates accuracy for regresison functions
"
Accuracy = function(predicts, labels){
  avg = mean(labels)
  tot  = sum((predicts - labels)^2)
  relative_tot = tot/(length(predicts) - 2)
  return (relative_tot)
}

RSquared = function(predicts, labels){
  avg = mean(labels)
  upper = sum((predicts - labels)^2)
  lower = sum((labels - avg)^2)
  r2 = upper/lower
  R2 = 1-r2
  return (R2)
}



####################################### FUNCTION CALLS ###############################################

# Training set, Training labels, Testing set, Testing labels, # of trees, # of params / tree
PerformClassification = function(Df, labels, Df2, labels2, num_trees, num_vars,Data) {
  # Set Constants
  time = proc.time()
  Const = Constant_Set(Data)
  m = Const[[1]]
  col_nam = Const[[2]]
  fo=Get_Forest(Df, labels, num_trees, num_vars)
  predictions = Classify(fo,Df2)
  Loss = Loss(predictions,labels2)
  print("Results:")
  print (paste (c("Loss = ", Loss), collapse = ""))
  print("Timings: ")
  print(proc.time() - time)
  return (predictions)
}

# Training set, Training labels, Testing set, Testing labels, # of trees, # of params / tree
PerformRegression = function(Df, labels, Df2, labels2, num_trees, num_vars) {
  # Set Constants
  time = proc.time()
  Const = Constant_Set(wineData)
  m = Const[[1]]
  col_nam = Const[[2]]
  fo=Get_Forest(Df, labels, num_trees, num_vars)
  predictions = Regress(fo,Df2)
  MSE = Accuracy(predictions, labels2)
  R2 = RSquared(predictions, labels2)
  print("Results:")
  print (paste (c("MSE = ", MSE), collapse = ""))
  print (paste (c("R2 = ", R2), collapse = ""))
  print("Timings: ")
  print(proc.time() - time)
  return (fo)
}
