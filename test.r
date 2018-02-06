param = names(wine)
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

for (t in 1:length(forest)){
  tree=forest[[t]]
  for (i in 1:length(tree[[3]])){
    if (names(tree[[3]][i]) == parameter){
      tot = tot + tree[[3]][[i]]
      mag = mag + 1
    }
  }
}



ind = 0
predicts = list()
for (n in 1:length(col_nam)){
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