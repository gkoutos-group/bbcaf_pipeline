###
# CHANGE THE WD DIRECTORY: on linux it can be done using pwd; I normally leave it as a another sub-directory in the same where common_pipeline is
# CHANGE THE SETTINGS: more information on settings can be found on common_pipeline/util/settings.R
###

source("bbcaf_pipeline/util/settings.R")

library(caret)
library(parallel)
settings$number_of_cores <- detectCores()
settings$split_test <- T;
settings$algos <- algos_classification_fast;
settings$mice_max_it <- 5;
settings$fraction_to_keep_col <- 0.9;
settings$fraction_to_keep_row <- 0.9;
settings$number_of_folds <- 5;
settings$number_of_repeats <- 1;
settings$split_ratio = 0.8

settings$print_results <- T
settings$balance_with_sample <- "rose";
settings$do_nzv <- T;
settings$seed_key <- 123456;

source("bbcaf_pipeline/pipe.R", chdir=T)

load_set <- function() {
  ###
  # load the dataset into the variable df the variable being optimized should be called ResultVariable, you can do that as in:
  #df$ResultVariable <- df$outcome
  #df$outcome <- NULL
  ###
  return(list(data=df))
}

settings$feature_selection_iters <- 50

if(T) {
  settings$algos <- c("glmnet");
  
  settings$feature_selection <- T
  settings$feature_selection_funcs <- "rfFuncs"
  settings$feature_selection_method <- NULL
  settings$feature_selection_current_method <- F
  settings$search_method = "random"
  settings$save_models <- T
  settings$save_datasets <- T
  settings$balance_with_sample <- "rose"
  
  settings$method_for_training <- "repeatedcv"
  
  print(settings$feature_selection_funcs)
  r1 <- data.frame(pipe(database = load_set, settings=settings))
  print(r1)
  
  # settings$return_predictions <- T
  # r2 <- data.frame(pipe(database = load_set, settings=settings))
  # print(r2)
}
