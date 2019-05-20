
run_model_regression <- function(dataTrain, algorithm_method, number_of_folds=1, repeats=0, cores=4, seed=123, unbalanced = F, balance = NULL) {
  if(Sys.info()["sysname"] == "Windows") {
    cl <- makeCluster(cores)
    registerDoParallel(cl);
  } else {
    registerDoMC(cores = cores)
  }
  
  set.seed(seed)
  fitControl <- trainControl(method = "repeatedcv",
                             number = number_of_folds,
                             repeats = repeats,
                             search = "random")
  
  if(!is.null(balance)) {
    fitControl$sampling = switch(balance, 
                                 "upsample" = "up",
                                 "downsample" = "down",
                                 "rose" = "rose")
  }
  
  set.seed(seed)
  this_fit <- train(ResultVariable ~ ., data = dataTrain, 
                    method = algorithm_method,
                    trControl = fitControl,
                    importance = T)
  
  if(Sys.info()["sysname"] == "Windows") {
    stopCluster(cl);
    registerDoSEQ();
  }
  return(this_fit)
}

