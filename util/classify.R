get_tune_grid <- function(algorithm_method, dataTrain)  {
  if(algorithm_method == "gbm") {
    return(expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = (1:30)*50,
                       shrinkage = 0.1,
                       n.minobsinnode = 20))
  } else if(algorithm_method == "rf") {
    return(expand.grid(mtry = seq(1, sqrt(ncol(dataTrain)))))
  } 
  return(NULL)
}

run_model_classification <- function(dataTrain, algorithm_method, number_of_folds=1, repeats=0, cores=4, seed=123, balance=NULL, method_for_training = "repeatedcv", search_method="random") {
  tuneGrid <- get_tune_grid(algorithm_method = algorithm_method, dataTrain = dataTrain)
  
  set.seed(seed)
  if(method_for_training != "none") {
    fitControl <- trainControl(algorithm_method,
                               method = method_for_training,
                               number = number_of_folds,
                               repeats = repeats,
                               classProbs = T, # for the auc
                               summaryFunction = twoClassSummary, # for the auc
                               search = search_method)
  } else {
    fitControl <- trainControl(classProbs = T, # for the auc
                               summaryFunction = twoClassSummary, # for the auc
                               search = search_method)
  }
  
  if(!is.null(balance)) {
    fitControl$sampling = switch(balance, 
                                 "upsample" = "up",
                                 "downsample" = "down",
                                 "rose" = "rose")
  }
  
  set.seed(seed)
  if(!is.null(tuneGrid)) {
    this_fit <- train(ResultVariable ~ ., data = dataTrain,
                      method = algorithm_method,
                      trControl = fitControl,
                      tuneLength = 50,
                      metric="ROC",
                      tuneGrid = tuneGrid
    )
  } else {
    this_fit <- train(ResultVariable ~ ., data = dataTrain,
                      method = algorithm_method,
                      trControl = fitControl, # for the normal grid/random search
                      tuneLength = 50, # for the normal grid/random search
                      metric="ROC"
    )
  }
  set.seed(seed)
  return(this_fit)
}

score_auc <- function(method_name = "", fitted_object, eval_dataset, expected_result, ignore_columns, plot=T) {
  PredProbs <- predict(fitted_object, newdata = eval_dataset, type = "prob")
  PredClass <- predict(fitted_object, eval_dataset[, setdiff(colnames(eval_dataset), ignore_columns)])
  
  result <- list(sens = sensitivity(data = PredClass, reference = eval_dataset$ResultVariable),
                 spec = specificity(data = PredClass, reference = eval_dataset$ResultVariable),
                 roc = myAUC <- pROC::auc(expected_result, predictor = PredProbs[[levels(expected_result)[2]]], levels=c("X1", "X0")))
  log("Final results:")
  log(result)
  log("Confusion matrix:")
  log(confusionMatrix(data = PredClass, reference = eval_dataset$ResultVariable, positive = "X1"))
  log("AUC:")
  log(ci(expected_result, predictor = PredProbs$X1))
  
  if(plot == T) {
    options(bitmapType='cairo')
    png(paste("result_for_", method_name, ".png", sep=""), width=1000, height=1000)
    myAUC <- pROC::plot.roc(x = expected_result, predictor = PredProbs$X1, levels=c("X1", "X0"))
    print(myAUC)
    myAUC <- myAUC$auc
    dev.off()
  } else {
    myAUC <- pROC::auc(expected_result, predictor = PredProbs$X1, levels=c("X1", "X0"))
  }
  return(myAUC)
}
