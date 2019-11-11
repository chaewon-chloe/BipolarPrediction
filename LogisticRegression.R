##### Logistic Regression (from scratch)

# Functions for optimization
logit = function(x){1/(1+exp(-x))}

cost = function(theta, X, y){
  h = logit(X%*%theta)
  return((t(-y)%*%log(h)-t(1-y)%*%log(1-h))/length(y))
}

grd = function(theta, X, y){
  h = logit(X%*%theta)
  return((t(X)%*%(h - y))/length(y))
}

# Logistic Regression and Predict function
Logit_reg = function(X, y){
  inputs = na.omit(cbind(y, X))
  X = as.matrix(cbind(bias=rep(1,nrow(inputs)), inputs[,-1]))
  y = as.matrix(as.numeric(inputs[, 1])-1)
  theta = matrix(rep(0, ncol(X)), nrow = ncol(X))
  return(optim(matrix(rep(0, ncol(X)), nrow = ncol(X)), cost, grd, X=X, y=y))
}

predict.log = function(mod, test, respond.class=F){
  coeff = mod$par
  test.X = cbind(1,test[,-1])
  output = as.matrix(test.X)%*%as.vector(coeff)
  output = as.vector(output)
  output = as.matrix(logit(output))
  if(respond.class==T){
    output = round(output)
  }
  return(output)
}

### Logistic Regression
LGS_chole = function(datalist, k=56){

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
      mod = Logit_reg(train[,-1], train[,1])
      pred = predict.log(mod, test, respond.class = T)
      cv.performance[i] = mean(pred == test$SuiHx)
      bar$tick()
    }
    temp = data.frame(data=names(datalist)[d], ACC = mean(cv.performance))
    result = rbind(result, temp)
  }
  return(result)
}
