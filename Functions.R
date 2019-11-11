SMOTE_data = function(train_data, class_size = 500){
  af_sm1 = class_size
  class1 = unname(table(train_data$SuiHx)[2])
  p.over = 100*(af_sm1-class1)/class1
  p.under = 100*af_sm1/(af_sm1-class1)
  library(DMwR)
  smoted = SMOTE(SuiHx~., train_data, perc.over = p.over, perc.under = p.under)
  return(smoted)
}


Divide_folds = function(n, k){
  shuffle = sample(1:n, n, replace = F)
  folds = list()
  ele = ceiling(n/k)
  for(i in 1:(k-1)){
    folds = c(folds, list(shuffle[1:ele]))
    shuffle = shuffle[-c(1:ele)]
  }
  folds = c(folds, list(shuffle))
  return(folds)
}
