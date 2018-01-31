# STAD94: Random Forest
## Introduction
This project is aimed at creating and implementing an algorithm in order to do Random Forest learning and prediction using R. Definitions below.
## Algorithm
Included are datasets along with the algorithm in an R file.

 -  Let Z be the data set, 
 -  Let its elements zi = {yi, xi1,xi2,...,xip}, with p being number of predictors
 -  Let `B`,`m`,`qual` be hyper-parameters, where:
        `B` is the number of boostraps, 
        `m` the number of variables to subsample from the feature spacem m ~= sqrt(p),
        and `qual` is a boolean to change the model from regression to classification.
        
The algorithm follows the following steps:

### Step 1:
```
For (j = 0; j < B; j ++) {
  
  1. Get a Bootstrap sample Bj
  2. Randomly sample m of p predictor variables (without replacement)
  3. Train a regression tree, Tj, on Bj over the m variables selected in step 2
  
}
```
Let the set {T} be the set of trees that was trained above.
Be sure to track which data points are used in each tree

### Step 2:
```
Create an empty list of trees L.
For (zi in Z) {
  1. Find all trees from the set of trees {T} that do NOT include zi
  2. Calculate prediction Error of theese trees.
  3. Choose the tree T with the lowest prediction error and add it to L
}
```
### Step 3:
If the model was quantative:
- Aggregate the coefficients for each parameter that appear in the trees in L to estimate the
  final coefficients, then sum them to create a regression model.

If the model qualitative:
- Find the tree in L that appears the most and choose it to be the model

### Step 4:
Return the final model as chosen in Step 3.

## Definitions

**Bootstrap** Given a data set of size n, a bootstrap is a random sample WITH replacement such that the sample size is also n. Ideally used when model creation is done repeatedly then aggregated.

**Prediction Error** A loss function which calculates the error between the true y and the predicted value f(x) = f(x1,...,xp). This is generally done using a function that doesn't discriminate between underflow and overflow, and considers both equally. ex: (y - f(x))^2

**Tree** A representation of regression functions over a feature space based on seperating the space into smaller regions and assigning each region it's associated mean estimate.
