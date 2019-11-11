###QDA
QDA_chloe = function(datalist, k = 56){
  library(MASS)
  library(progress)
  set.seed(1)
  cv_idx = createFolds(sample(1:56, 56, replace = F), k)
  result = data.frame(data = NULL, ACC = NULL)
  bar = progress_bar$new(
    format = "  Training [:bar] :percent", clear = FALSE, width= 60, total = length(cv_idx)*length(datalist))
  for(d in 1:length(datalist)){
    data = datalist[[d]]
    data$SuiHx = as.factor(data$SuiHx)
    cv.performance = NULL
    for(i in 1:length(cv_idx)){
      train = data[-cv_idx[[i]],]
      train = SMOTE_data(train,500)
      test = data[cv_idx[[i]],]
      mod = qda(SuiHx~., data = train, cv = F)
      pred = predict(mod, test)$class
      cv.performance[i] = mean(pred == test$SuiHx)
      bar$tick()
    }
    temp = data.frame(data = names(datalist[d]), ACC = mean(cv.performance))
    result = rbind(result, temp)
  }
  cat('\n')
  return(result)
}

### LDA
LDA_chloe = function(datalist, k = 56){
  library(MASS)
  library(progress)
  set.seed(1)
  cv_idx = createFolds(sample(1:56, 56, replace = F), k)
  result = data.frame(data = NULL, ACC = NULL)
  bar = progress_bar$new(
    format = "  Training [:bar] :percent", clear = FALSE, width= 60, total = length(cv_idx)*length(datalist))
  for(d in 1:length(datalist)){
    data = datalist[[d]]
    data$SuiHx = as.factor(data$SuiHx)
    cv.performance = NULL
    for(i in 1:length(cv_idx)){
      train = data[-cv_idx[[i]],]
      train = SMOTE_data(train, 500)
      test = data[cv_idx[[i]],]
      mod = lda(SuiHx~., data = train, cv = FALSE)
      pred = predict(mod, test)$class
      cv.performance[i] = mean(pred == test$SuiHx)
      bar$tick()
    }
    temp = data.frame(data = names(datalist[d]), ACC = mean(cv.performance))
    result = rbind(result, temp)
  }
  cat('\n')
  return(result)
}

### KNN
KNN_chloe = function(datalist, k.fold = 56, k.neighbor = 20){
  
  library(e1071)
  library(progress)
  
  result = data.frame(data = NULL, k = NULL, ACC = NULL)
  bar = progress_bar$new(
    format = "  Training [:bar] :percent", clear = FALSE, width= 60, total = length(datalist))
  set.seed(1)
  for(d in 1:length(datalist)){
    data = datalist[[d]]
    data$SuiHx = as.factor(data$SuiHx)
    mod = tune.knn(x = data[,-1], y = as.factor(data[,1]), k = 1:k.neighbor,
                   tunecontrol = tune.control(cross = k.fold), tunetype = "c")
    temp = data.frame(data = names(datalist)[d], k = as.numeric(mod$best.parameters), ACC = 1-mod$best.performance)
    result = rbind(result, temp)
    bar$tick()
  }
  return(result)
  
}


BAG_chloe = function(datalist, k=56, RF = F){
  library(randomForest)
  library(progress)
  set.seed(1)
  cv_idx = createFolds(sample(1:56, 56, replace = F), k)
  result = data.frame(data = NULL, ACC = NULL)
  bar = progress_bar$new(
    format = "  Training [:bar] :percent", clear = FALSE, width= 60, total = length(cv_idx)*length(datalist))
  for(d in 1:length(datalist)){
    data = datalist[[d]]
    data$SuiHx = as.factor(data$SuiHx)
    m.try = ifelse(RF==T, round(sqrt(NCOL(data)-1)), NCOL(data)-1)
    data$SuiHx = as.factor(data$SuiHx)
    cv.performance = NULL
    for(i in 1:length(cv_idx)){
      train = data[-cv_idx[[i]],]
      train = SMOTE_data(train, 500)
      test = data[cv_idx[[i]],]
      mod = randomForest(SuiHx~.,data=train, mtry=m.try,importance=TRUE)
      pred = predict(mod, test[,-1])
      cv.performance[i] = mean(pred == test$SuiHx)
      bar$tick()
    }
    temp = data.frame(data=names(datalist)[d], ACC = mean(cv.performance))
    result = rbind(result, temp)
  }
  return(result)
}


##### SVM
SVM_chloe = function(datalist, k = 56, 
                     costs = c(0.001,0.01,0.1,1,5,10,100),
                     degrees = c(0.5,1,2,3,4),
                     gammas = c(0.5, 1,2,3,4,10)){
  library(e1071)
  library(dplyr)
  library(progress)
  set.seed(1)
  result = data.frame(data = NULL, kernel = NULL, cost = NULL, gamma = NULL, degree = NULL, ACC = NULL)
  bar = progress_bar$new(
    format = "  Training [:bar] :percent", clear = FALSE, width= 60, total = 3*length(names(datalist)))
  for(i in names(datalist)){
    m.l = tune(svm, SuiHx~., data=datalist[[i]], kernel='linear', ranges=list(cost = costs),
               tunecontrol = tune.control(cross = k), tunetype = "c")
    bar$tick()
    m.r = tune(svm, SuiHx~., data = datalist[[i]], kernel='radial', ranges=list(cost = costs, gamma = gammas),
                      tunecontrol = tune.control(cross = k), tunetype = "c")
    bar$tick()
    m.p = tune(svm, SuiHx~., data = datalist[[i]], kernel='polynomial', ranges=list(cost = costs, gamma = gammas, degree = degrees),
                      tunecontrol = tune.control(cross = k), tunetype = "c")
    bar$tick()
    temp = data.frame(data = i, kernel = c('linear', 'radial','polynomial'), 
                      cost = c(m.l$best.parameters[['cost']], m.r$best.parameters[['cost']], m.p$best.parameters[['cost']]),
                      gamma = c(NA, m.r$best.parameters[['gamma']], m.p$best.parameters[['gamma']]),
                      degree = c(NA, NA, m.p$best.parameters[['degree']]),
                      ACC = 1-c(m.l$best.performance, m.r$best.performance, m.p$best.performance)
                        )
    result = rbind(result, temp)
  }
  cat('\n')
  return(result)
  
}
