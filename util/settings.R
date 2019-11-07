
library(caret)

algos_classification <- c("svmLinear2", "gbm", "rf", "kernelpls", "stepLDA", "AdaBoost.M1", "adaboost", "rocc", "RRF", "svmLinearWeights2", "rpart", "glm", "nnet", "knn", "kknn", "logreg", "earth", "pcaNNet", "pls", "plsRgml", "dnn", "lda", "glmnet", "svmRadial", "nb", "C5.0", "treebag", "xgbLinear", "plr", "rpart", "xgbTree")
algos_classification_fast <- c("svmLinear2", "gbm", "rf", "kernelpls", "rocc", "RRF", "svmLinearWeights2", "rpart", "glm", "nnet", "knn", "kknn", "logreg", "earth", "pcaNNet", "dnn", "lda", "glmnet", "svmRadial", "nb", "C5.0", "treebag", "plr") #"plsRgml"
algos_classification_bbcaf <- c("svmLinear2", "gbm", "rf", "glmnet", "rpart")

settings <- list(
  #the name of the test
  test_name = "test",
  
  #wether the test set will be split or not and the ratio for the test set
  split_test = T,
  split_ratio = 0.8,
  
  #verbose will print more information when running
  verbose = T,
  
  #random seed
  seed_key = 123,
  
  #cv, repeatedcv, boot or none
  method_for_training = "repeatedcv",
  
  #number of folds when training
  number_of_folds = 10,
  
  #number of repeats when training
  number_of_repeats = 5,
  
  #number of threads
  number_of_cores = 4,
  
  #fraction to keep NA values on columns (0.7 will keep all columns that have up to 70% NA)
  fraction_to_keep_col = 0.5,
  
  #fraction to keep NA values on rows (0.5 will keep all rows that have up to 50% NA)
  fraction_to_keep_row = 0.5,
  
  #maximum amount of iterations on mice
  mice_max_it = 5,
  
  #a wide selection of algorithms to try different forms of classification
  algos = c("rf"),

  #strategies to balance the dataset, options are: upsample, downsample, rose
  balance_with_sample = NULL,
  
  #to save model
  save_models = F,
  
  #save the datasets
  save_datasets = F,
  
  #save the environment,
  save_env = F,

  #save predictions
  save_pred = F,
  
  #to load from the saved models
  load_and_run_from_models = F,
  
  #used to return predictions just after predicting
  return_predictions = F,
  
  #remove columns by near_zero_variance
  do_nzv = T,
  
  #center and scale the values
  do_center_scale = T,
  
  #filter the variables before creating the models
  feature_selection = F,
  
  #functions could be: lmFuncs, rfFuncs, nbFuncs, treebagFuncs or caretFuncs to use with the others in method | lmFuncs is for linear
  feature_selection_funcs = "rfFuncs",
  
  feature_selection_method = NULL,
  
  feature_selection_current_method = F,
  
  feature_selection_iters = 10,
  
  #whether to run the models or only do the feature selection
  stop_after_feature_selection = F,
  
  #converts factors (binary category variable) to numerical 1 or 0
  factor_to_hot_encoded = T,
  
  #the preprocess methods should be run individually or with a preprocessed created model from train set?
  preprocess_individually = F,
  
  #the search method for the parameter optimization
  search_method = "random",
  
  #if we are to plot the auc for the split results
  plot_auc_test = F,
  
  #if we are to plot from the feature selection
  plot_feature_selection = F
)

