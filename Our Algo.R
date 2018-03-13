" 
Hyperparameters
================
p - Number of predictors to use for constructing trees
B - Number of trees chosen for each bootstrap sample
"

################################# LIBRARIES AND MISCELLANEOUS ######################################
rm(list = ls())

"
Please add all required libraries to the 'libraries' vector of strings below. All libraries in the
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

################################### READ AND CLEAN DATA ############################################
##################################### Wine Data ##################################################
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
wineData = wineData[,-ncol(wineData)]
##################################################################################################

############################## Breast Cancer Data ################################################
#read data into
data(BreastCancer)
#remove Nas
BreastCancer = as.data.frame(na.omit(BreastCancer))
#change the benign to 0 and malignant to 1 for the classes
BC=within(BreastCancer, Class <- factor(Class, labels = c(0,1)))
#get rid of the id
BC = BC[,-1]
#make the class into factors if already not done
BC[, 1:9] <- sapply(BC[, 1:9], as.numeric)





#############################################################################################



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

##################################### SUMMARY STATISTICS ########################################

"wineData %>% group_by(Type) %>% summarize(n=n())
wineData %>% group_by(quality) %>% summarize(n=n())
hist.chlorides = hist(wineData$chlorides)
hist.ph = hist(wineData$pH)
hist.citricac = hist(wineData$citric_acid)
hist.ressugar = hist(wineData$residual_sugar)"

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
  trees = rpart(formula=param, data=sample.p.dat, method='regress')
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

ClassLoss = function(predicts, labels){
  loss =  1 - as.numeric(predicts == labels)
  error = sum(loss) / length(labels)
  return (error)
}

####################################### REGRESSION ##############################################

RegClass = function(forest,obs){
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
RegnAcc = function(predicts, labels){
  avg = mean(labels)
  tot  = sum((predicts - labels)^2)
  relative_tot = tot/(length(predicts) - 2)
  return (relative_tot)
}

RegR2 = function(predicts, labels){
  avg = mean(labels)
  upper = sum((predicts - labels)^2)
  lower = sum((labels - avg)^2)
  r2 = upper/lower
  R2 = 1-r2
  return (R2)
}
"
# REGN CLASSIFIER
Regn_Predicts = function(){
  ind = 0
  predicts = list()
  forest=Get_Forest()
  for (n in 1:length(col_nam) ){
    parameter = col_nam[n]
    ind = ind+1
    tot = 0
    mag = 0
    for (t in 1:length(forest)){
      tree=forest[[t]]
      for (i in 1:length(tree[[3]])){
        if (names(tree[[3]][i]) == parameter){
          tot = tot + tree[[3]][[i]]
          mag = mag + 1
        }
      }
    }
    predicts[[ind]]= tot/mag
  }
  names(predicts) = col_nam
  return(predicts)
}
"

################################## FUNCTION CALLS ####################################################

#fo=Get_Forest(wineData, 10, 4)
"preds = Regn_Predicts()
preds = preds[]"

################################## EXAMPLE CALLS ####################################################

# Training set, Training labels, Testing set, Testing labels, # of trees, # of params / tree
Perform = function(Df, labels, Df2, labels2, num_trees, num_vars,Data) {
  # Set Constants
  time = proc.time()
  Const = Constant_Set(Data)
  m = Const[[1]]
  col_nam = Const[[2]]
  fo=Get_Forest(Df, labels, num_trees, num_vars)
  predictions = Regress(fo,Df2)
  Acc = Accuracy(predictions,labels2)
  R2 = RegR2(predictions,labels2)
  print("Results:")
  print (paste (c("Acc = ", Acc), collapse = ""))
  print (paste (c("R2 = ", R2), collapse = ""))
  print("Timings: ")
  print(proc.time() - time)
  return (predictions)
}

w1 = sample_n(wineData, nrow(wineData)/2, replace=FALSE)
w2 = anti_join(wineData,w1)
labs = w1[ncol(w1)]
labels2 = as.numeric(unlist(w2[ncol(w2)]))
w1 = w1[,-ncol(w1)]
w2 = w2[,-ncol(w2)]
B = 600
M = 4
pred = Perform(w1,labs,w2,labels2,B,M,wineData)


#fo=Get_Forest(w1, labels, B, M)
#predictions = RegClass(fo,w2)


if (FALSE) {
  v1 = sample_n(BC, nrow(BC)/2, replace=FALSE)
  v2 = anti_join(BC,v1)
  Labels = v1[ncol(v1)]
  Labels2 = as.numeric(unlist(v2[ncol(v2)]))
  v1 = v1[,-ncol(v1)]
  v2 = v2[,-ncol(v2)]
  B2= 50
  M2 = 1
  f2 = Perform(v1,Labels,v2,Labels2,B2,M2,BC)
  
  #fo2 = Get_Forest(v1,Labels,B2,M2)
  #predictions = RegClass(fo2,v2)
}
