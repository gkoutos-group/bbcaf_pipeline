library(caret)
library(mlbench)
library(Hmisc)

set.seed(123) # for initialization

ga_feature_selection <- function(data, method="rf", repeats=5, number_of_folds = 10, seed = 123, number_of_iterations = 50) {
  set.seed(seed)
  ctrl <- gafsControl(functions = caretGA,
                      method = "repeatedcv",
                      number = number_of_folds,
                      repeats = repeats)
  
  obj <- gafs(x = data[, setdiff(colnames(data), c("ResultVariable"))], 
              y = data$ResultVariable,
              iters = number_of_iterations,
              gafsControl = ctrl,
              ## Now pass options to `train`
              method = "rf",
              allowParallel = T)
  
  return(obj)
}

rf_feature_selection <- function(data, 
                                 repeats = 5, 
                                 cores = 4, 
                                 number_of_folds = 10, 
                                 seed = 123, 
                                 step_size = 5, 
                                 plot = T, 
                                 plot_name = "plot.png",
                                 functions = "rfFuncs",
                                 method = NULL,
                                 method_for_training = "repeatedcv") {
  set.seed(seed)
  
  print(sprintf("Functions: %s", functions))
  # 
  # vv <- c(lmFuncs, rfFuncs, nbFuncs, treebagFuncs, caretFuncs)
  # vn <- c("lmFuncs", "rfFuncs", "nbFuncs", "treebagFuncs", "caretFuncs")
  # if(functions %in% vn) {
  #   functions <- vv[which(vn == functions)]
  # } else {
  #   print(sprintf("ERROR ON FEATURE SELECTION, INVALID METHOD. Use one of: %s", vn))
  #   return(-1)
  # }
  
  functions <- switch(functions,
                      "rfFuncs" = rfFuncs,
                      "nbFuncs" = nbFuncs,
                      "treebagFuncs" = treebagFuncs,
                      "caretFuncs" = caretFuncs)
  if(is.null(functions)) {
    print(sprintf("ERROR ON FEATURE SELECTION, INVALID METHOD. Use one of: %s", c("rfFuncs", "nbFuncs", "treebagFuncs", "caretFuncs")))
  }

  functions$summary <- twoClassSummary
  
  # print(ncol(data))
  # print(colnames(data))
  if(ncol(data)-1 <= 10) {
    subsets <- 1:ncol(data)-1
  } else {
    subsets <- union(union(c(1:10), which(1:(ncol(data)-1) %% step_size ==0)), ncol(data)-1)
  }
  # print(subsets)
  
  seeds <- vector(mode = "list", length = repeats*number_of_folds + 1)
  for(i in 1:(repeats*number_of_folds))
    seeds[[i]] <- rep(seed, length(subsets) + 1)
  seeds[[repeats*number_of_folds + 1]] <- seed
  # print(seeds)
  
  set.seed(seed)
  if(method_for_training != "none") {
    ctrl <- rfeControl(functions=functions, 
                       method = method_for_training,
                       repeats = repeats, 
                       number = number_of_folds,
                       verbose = F,
                       allowParallel=T,
                       returnResamp = "final",
                       seeds=seeds)
  } else {
    ctrl <- rfeControl(functions=functions,
                       repeats = repeats, 
                       verbose = F,
                       allowParallel=T,
                       returnResamp = "final",
                       seeds=seeds)
  }
  
  tr_ctrl <- trainControl(method = "none", classProbs = T)

  if(!is.null(method)) {
    print(sprintf("Feature selection is using method: %s", method))
    set.seed(seed)
    rfProfile <- rfe(data[, setdiff(colnames(data), c("ResultVariable"))],
                   data$ResultVariable,
                   sizes = subsets,
                   rfeControl = ctrl,
                   trControl = tr_ctrl,
                   metric = "ROC",
                   
                   method = method)
  } else {
    print("Running with functions")
    set.seed(seed)
    rfProfile <- rfe(data[, setdiff(colnames(data), c("ResultVariable"))],
                     data$ResultVariable,
                     sizes = subsets,
                     rfeControl = ctrl,
                     trControl = tr_ctrl,
                     metric = "ROC")
  }

  if(plot) {
    options(bitmapType='cairo')
    print(sprintf("Image to %s", plot_name))
    png(paste0("output/img/", plot_name), width=1920, height=1080)
    # p <- plot(fs) + theme_bw()
    plot(rfProfile) #, type = c("g", "o")
    # plot(p)
    dev.off();
  }
  
  print(rfProfile)
  cat("Variable importance in general:")
  print(varImp(rfProfile, scale=T))
  cat("Variable importance for fit:")
  print(varImp(rfProfile$fit, scale=T))
  # saveRDS(rfProfile, "rfProfile.RDS")
  print(sprintf("Predictors are: %s", predictors(rfProfile)))
  return(predictors(rfProfile))
}
