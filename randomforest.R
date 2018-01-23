" 
Hyperparameters
================
m - Number of predictors to use for constructing trees
B - Number of trees chosen for each bootstrap sample
"

library(tidyverse)
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
  # TODO: Implement loss function
}

