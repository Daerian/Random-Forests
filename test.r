ind = 0
end
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