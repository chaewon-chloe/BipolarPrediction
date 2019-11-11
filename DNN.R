DNN_chloe = function(data, d_name, k = 56, epoch = 100){
  library(keras)
  library(tensorflow)
  library(progress)
  set.seed(1)
  cv_idx = Divide_folds(nrow(data), k)
  bar = progress_bar$new(
    format = "  Training :percent", clear = FALSE, width= 60, total = length(cv_idx))

  data$SuiHx = as.factor(data$SuiHx)
 
  dl_mod = keras_model_sequential()
  dl_mod %>% 
    layer_dense(units = 256, activation = "relu", input_shape = c(ncol(data)-1)) %>% layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = "relu") %>% layer_dropout(rate = 0.3) %>%
    layer_dense(units = 64, activation = "relu") %>% layer_dropout(rate = 0.2) %>%
    layer_dense(units = 32, activation = "relu") %>% layer_dropout(rate = 0.1) %>%
    layer_dense(units = 2, activation = "softmax")
  
  dl_mod %>% compile(
    loss = "categorical_crossentropy", optimizer = optimizer_sgd(), metrics = c("accuracy"))
  
  cv.performance = NULL
  for(i in 1:length(cv_idx)){
    idx = cv_idx[[i]]
    smoted_train = SMOTE_data(data[-idx,], 500)
    test = data[idx,]
    
    # Split training & test set
    test_y = to_categorical(test[,1],2)
    test_x = as.matrix(test[,-1])
    train_y = to_categorical(smoted_train[,1],2)
    train_x = as.matrix(smoted_train[,-1])
    d = length(idx)
    dim(test_x) = c(d,ncol(test_x)); dim(test_y) = c(d,2)
    
    fit_mod = dl_mod %>% fit( train_x, train_y, 
                              epochs = epoch, batch_size = 55, validation_split = 0, verbose = 0)
    
    cv.performance[i] = evaluate(dl_mod, x = test_x, y = test_y, verbose = 0)$acc
    bar$tick()
  }
  result = data.frame(data = d_name, ACC = mean(cv.performance), k=k, epoch = epoch)
  return(result)
}
