
# V Roth Cardoso - 2017/06/30
# script to test different algorithms and conditions
# run as
# Rscript pipe.R > terminal_output
# and watch it
# watch tail -n 20 terminal_output

VERSION = "3"
options(show.error.locations=TRUE)
set.seed(123) # for initialization
# this value will be reset inside

install.packages("here")
library(here)

library(caret)
library(AppliedPredictiveModeling)
library(pROC)
library(mice)

load_packages <- function() {
  source(paste0(here(), "/util/log.R"));
  source(paste0(here(), "/util/load_base.R"));
  source(paste0(here(), "/util/preprocess.R"));
  source(paste0(here(), "/util/classify.R"));
  source(paste0(here(), "/util/regression.R"));
  source(paste0(here(), "/util/feature_selection.R"));
}

load_packages();

pipe <- function(database=load_base, settings=settings) {
  if(settings$save_env) {
    save.image(file = paste0(here(), "/output/env/env_at_", strftime(Sys.time(),"%Y-%m-%d_%H_%M_%S_%Z"), Sys.info()['nodename'], ".RData"));
  }

  # if(settings$return_predictions == T & settings$split_test == T) {
  #   log(sprintf("It will not return predictions for split test! Select either settings$split_test == F or settings$return_predictions == F"))
  #   return(-1)
  # }
  
  # if(!is.null(settings$balance_with_sample) & settings$split_test == F) {
  #   log(sprintf("settings$balance_with_sample is only to be used with the splitted test."))
  #   settings$balance_with_sample = NULL
  # }
  if(length(intersect(settings$balance_with_sample, c("upsample", "downsample", "rose"))) != 1 & !is.null(settings$balance_with_sample)) {
    log(sprintf("Invalid settings for settings$balance_with_sample: %s", settings$balance_with_sample))
    log(sprintf("Valid options are: '%s'", c("upsample", "downsample", "rose")))
    return(-1)
  }
  log(sprintf("Setting: %s", paste(names(settings), settings, sep=" ")))
  if(Sys.info()["sysname"] == "Windows") {
    cl <- makeCluster(cores)
    registerDoParallel(cl);
  } else {
    registerDoMC(cores = settings$number_of_cores)
  }
  
  ########################## load the dataset
  data <- database()$data;
  if(length(data) == 0) {
    s <- sprintf("Dataset is invalid? Please check config");
    print(s)
    return(-1);
  }
  
  if(is.null(settings$regression) & class(data$ResultVariable) == "factor") {
    settings$regression <- F;
    log("Running classification");
  } else {
    settings$regression <- T;
    log("Running regression");
  }
  ########################## load the dataset
  
  #remove the columns without a certain amount of values
  if(settings$fraction_to_keep_col > 0) {
    data <- set_preprocess_remove_columns_na(data, fraction_to_keep=settings$fraction_to_keep_col)$data;
  }
  #remove the rows without a certain amount of values
  if(settings$fraction_to_keep_row > 0) {
    data <- set_preprocess_remove_rows_na(data, fraction_to_keep=settings$fraction_to_keep_row)$data;
  }
  
  if(settings$split_test) { #splitted case
    ########################## split the sets
    print("Splitting the data")
    ret <- set_preprocess(data = data, 
                          split_ratio = settings$split_ratio,
                          preprocess_methods = NULL, 
                          seed = settings$seed_key, 
                          remove_na_rows = F,
                          do_nzv = settings$do_nzv);
    dataTrain <- ret$dataTrain;
    dataTest <- ret$dataTest;
    rm(data);
    rm(ret);
    ########################## split the sets
    
    ########################## transform the factor variables
    if(settings$factor_to_hot_encoded) {
      nDataTrain <- set_preprocess_transform_factor(dataTrain)$data; #gonna get the new columns to place back after center and scale
      nDataTest <- set_preprocess_transform_factor(dataTest)$data; #gonna get the new columns to place back after center and scale
      
      nDataTrainCols <- setdiff(colnames(nDataTrain), colnames(dataTrain));
      nDataTestCols <- setdiff(colnames(nDataTest), colnames(dataTest));
    } else {
      nDataTrain <- dataTrain
      nDataTest <- dataTest
    }
    
    ########################## center and scale variables
    if(settings$do_center_scale) {
      ret <- set_preprocess_already_splitted(dataTrain = nDataTrain,
                                             dataTest = nDataTest,
                                             preprocess_methods = c("center", "scale"), ##################
                                             seed = settings$seed_key,
                                             remove_na_rows = F,
                                             preprocess_individually = settings$preprocess_individually);
    } else {
      ret <- set_preprocess_already_splitted(dataTrain = nDataTrain,
                                             dataTest = nDataTest,
                                             preprocess_methods = NULL,
                                             seed = settings$seed_key,
                                             remove_na_rows = F,
                                             preprocess_individually = settings$preprocess_individually);
    }
    dataTrain <- ret$dataTrain;
    dataTest <- ret$dataTest;
    rm(ret);
    ########################## center and scale variables
    
    if(settings$factor_to_hot_encoded) {
      dataTrain[, nDataTrainCols] <- nDataTrain[, nDataTrainCols];
      dataTest[, nDataTestCols] <- nDataTest[, nDataTestCols];
      
      rm(nDataTrainCols);
      rm(nDataTestCols);
    }
    rm(nDataTrain);
    rm(nDataTest);
    ########################## transform the factor variables
    
    ##########################
    if(settings$verbose) {
      print("Missing values on training")
      print(colSums(is.na(dataTrain)))
      
      print("Missing values on test")
      print(colSums(is.na(dataTest)))
    }
    
    ########################## imputation
    if(settings$mice_max_it > 0) {
      temp <- dataTrain$ResultVariable; #doing separately
      dataTrain$ResultVariable <- NULL;
      tryCatch({
        imputation <- mice(dataTrain, m=1, maxit = settings$mice_max_it, method = 'pmm', seed = settings$seed_key);
        dataTrain <- complete(imputation, 1);
      },
      error = function(e) {
        print(e);
      })
      dataTrain$ResultVariable <- temp;
      
      temp <- dataTest$ResultVariable;
      dataTest$ResultVariable <- NULL;
      tryCatch({
        imputation <- mice(dataTest, m=1, maxit = settings$mice_max_it, method = 'pmm', seed = settings$seed_key);
        dataTest <- complete(imputation, 1);
      },
      error = function(e) {
        print(e)
      })
      dataTest$ResultVariable <- temp;
      rm(temp);
    }
    
    #if mice fails to get some, try using knn
    if(sum(is.na(dataTrain)) + sum(is.na(dataTest)) > 0) {
      ret <- set_preprocess_already_splitted(dataTrain = dataTrain,
                                             dataTest = dataTest,
                                             preprocess_methods=c("knnImpute"),
                                             seed = settings$seed_key,
                                             remove_na_rows = T,
                                             preprocess_individually = settings$preprocess_individually);
      dataTrain <- ret$dataTrain;
      dataTest <- ret$dataTest;
      rm(ret)
    }
    ########################## imputation
    
    if(ncol(dataTrain) != ncol(dataTest)) {
      namesDataTrain <- colnames(dataTrain);
      namesDataTest <- colnames(dataTest);
      
      finalCols <- intersect(namesDataTrain, namesDataTest);
      
      removed_dataTrain <- setdiff(namesDataTrain, finalCols);
      removed_dataTest <- setdiff(namesDataTest, finalCols);
      
      log(sprintf("Removed column on train (because of difference between sets): %s", removed_dataTrain))
      log(sprintf("Removed column on test (because of difference between sets): %s", removed_dataTest))
      
      dataTrain <- dataTrain[, finalCols];
      dataTrain <- dataTest[, finalCols];
    }
    
    log(sprintf("Data set has size: %05d (dataTrain), %05d (dataTest)", nrow(dataTrain), nrow(dataTest)))
    log(sprintf("Train set has distribution:"));
    log(table(dataTrain$ResultVariable));
    log(sprintf("Test set has distribution:"));
    log(table(dataTest$ResultVariable));
    log(sprintf("The data-set has column: %s", colnames(dataTrain)));
    
    if(settings$save_datasets) {
      dir.create("run_datasets")
      train_file <- paste(here(), "/output/run_datasets/", strftime(Sys.time(),"%Y-%m-%d_%H_%M_%S_%Z"), settings$test_name, "_split_train.csv", sep="")
      print(train_file)
      write.csv(dataTrain, train_file)
      test_file <- paste(here(), "/output/run_datasets/", strftime(Sys.time(),"%Y-%m-%d_%H_%M_%S_%Z"), settings$test_name, "_split_test.csv", sep="")
      print(test_file)
      write.csv(dataTest, test_file)
    }
  } else { #non-splitted test data -- folding results evaluation
    ########################## transform the factor variables
    nData <- set_preprocess_transform_factor(data)$data; #gonna get the new columns to place back after center and scale
    nDataCols <- setdiff(colnames(nData), colnames(data))
    data <- set_preprocess_one_set(nData, preprocess_methods=c("center", "scale"), 
                                   do_nzv=settings$do_nzv)$data;
    data[, nDataCols] <- nData[, nDataCols];
    rm(nData);
    ########################## transform the factor variables
    
    ########################## mice impute
    if(settings$mice_max_it > 0) {
      temp <- data$ResultVariable;
      data$ResultVariable <- NULL;
      imputation <- mice(data, m=1, 
                         maxit = settings$mice_max_it, 
                         method = 'pmm', 
                         seed = settings$seed_key);
      data <- complete(imputation, 1);
      data$ResultVariable <- temp;
      rm(temp);
    }
    
    #if mice fails get the variable through knn
    data <- set_preprocess_one_set(data,
                                   preprocess_methods=c("knnImpute"),
                                   do_nzv = settings$do_nzv)$data;
    ########################## imputation
    
    log(sprintf("Data set has size: %05d", nrow(data)));
    log(sprintf("Data set has distribution:"));
    log(table(data$ResultVariable));
    log(sprintf("The data-set has column: %s", colnames(data)));
    
    if(settings$save_datasets) {
      write.csv(data, paste(here(), "/output/run_datasets/", strftime(Sys.time(),"%Y-%m-%d_%H_%M_%S_%Z"), settings$test_name, "_nonsplit_data.csv", sep=""))
    }
  }
  
  ########################### run mode
  if(settings$regression == T) {
    run_model <- run_model_regression;
  } else {
    run_model <- run_model_classification;
  }
  ########################### run mode
  
  ########################### results variables
  if(settings$regression) {
    results_rmse <- list()
    results_r2 <- list()
    if(settings$split_test) {
      results_split_rmse <- list()
      results_split_r2 <- list()
    }
  } else {
    results_roc <- list()
    if(settings$split_test) {
      results_split_roc <- list()
    }
  }
  ########################### results variables
  
  if(settings$split_test) {
    data <- dataTrain;
  }
  
  ########################### feature selection
  if(!is.null(settings$feature_selection) && settings$feature_selection == T && 
     ( (settings$feature_selection_funcs == "caretFuncs" && !settings$feature_selection_current_method) || (settings$feature_selection == T && settings$feature_selection_funcs != "caretFuncs" )) ) {
    if(settings$feature_selection_funcs == "caretFuncs" && !settings$feature_selection_current_method && is.null(settings$feature_selection_method)) {
      print("There should be a method for feature selection when the current is not being used.")
      return(-1);
    }
    
    i_funcs <- settings$feature_selection_funcs
    i_method <- settings$feature_selection_method
    
    log(sprintf("Feature selection properties: %s | %s", settings$feature_selection_funcs, i_method))
    fs <- rf_feature_selection(data,
                               number_of_folds = settings$number_of_folds,
                               repeats = settings$number_of_repeats,
                               cores = settings$number_of_cores,
                               seed = settings$seed_key,
                               # balance = settings$balance_with_sample,
                               plot = settings$plot_feature_selection,
                               plot_name = paste0(here(), '/output/img/', settings$test_name, i_method, "feature_selection.png"),
                               functions = i_funcs,
                               method = i_method)
    print(sprintf("Variables selected after feature selection are: %s", fs))
    dataTrain <- dataTrain[, union(fs, c("ResultVariable"))]
    dataTest <- dataTest[, union(fs, c("ResultVariable"))]
    data <- data[, union(fs, c("ResultVariable"))]
    
    if(settings$stop_after_feature_selection) {
      return(list(variables_selection=fs))
    }
  }
  ########################### feature selection
  
  ########################### loop training different models
  data_o <- data
  for(i in settings$algos) {
    data <- data
    result_name <- i
    log(sprintf("Training algorithm: %s", result_name));
    this_result <- NULL
    tryCatch({
      if(settings$load_and_run_from_models) {
        print("LOADING MODEL")
        dataTest <- data
        this_result <- readRDS(paste0(here(), "/output/models/", strftime(Sys.time(),"%Y-%m-%d_%H_%M_%S_%Z"), settings$test_name, "_model_", i))
      } else {
        tryCatch( {
          if(!is.null(settings$feature_selection) && settings$feature_selection == T && 
             (settings$feature_selection_current_method == T)) {
            log("RUNNING FEATURE SELECTION INSIDE")
            i_method <- i
            i_funcs <- "caretFuncs"

            log(sprintf("Feature selection properties: %s | %s", settings$feature_selection_funcs, i_method))
            fs <- rf_feature_selection(data,
                                       number_of_folds = settings$number_of_folds,
                                       repeats = settings$number_of_repeats,
                                       cores = settings$number_of_cores,
                                       seed=settings$seed_key,
                                       # balance = settings$balance_with_sample,
                                       plot = settings$plot_feature_selection,
                                       plot_name = paste0(here(), "/output/img", settings$test_name, i, "feature_selection.png"),
                                       functions = i_funcs,
                                       method = i_method)
            print(sprintf("Variables selected after feature selection are: %s", fs))
            data <- data[, union(fs, c("ResultVariable"))]
          }
          
          log("CREATING MODEL")
          this_result <- run_model(data,
                                   i,
                                   number_of_folds=settings$number_of_folds,
                                   repeats=settings$number_of_repeats,
                                   cores=settings$number_of_cores,
                                   seed=settings$seed_key,
                                   balance = settings$balance_with_sample,
                                   method = settings$method_for_training,
                                   search_method = settings$search_method);
          
          if(settings$save_models) {
            print("SAVING MODEL")
            saveRDS(this_result, paste0(here(), "/output/models/", strftime(Sys.time(),"%Y-%m-%d_%H_%M_%S_%Z"), settings$test_name, "_model_", i))
          }
          
          log(sprintf("Training algorithm: %s", result_name));
          log(varImp(this_result, scale=T));
          log(sprintf("Results were:"))
          log(this_result$results);
          log(sprintf("Whole model information:"))
          log(this_result)
        }, error = function(e) {
          log(e);
          log(colSums(is.na(data)));
        })
      }
      if(settings$regression) {
        # results_rmse[[result_name]] <- min(this_result$results$RMSE); ############################################################ TODO: CHANGE - FIX TO GET THE ONE USED
        if(!is.na(max(this_result$results$Rsquared))) {
          results_r2[[result_name]] <- max(this_result$results$Rsquared);
        }
      } else {
        if(!is.na(max(this_result$results$ROC)) & max(this_result$results$ROC) >= 0 & max(this_result$results$ROC) <= 1) {
          results_roc[[result_name]] <- max(this_result$results$ROC);
        }
      }
      
      if(settings$return_predictions) {
        pred_vals <- predict(this_result, dataTest[, setdiff(colnames(dataTest), c("ResultVariable"))])
        pred_vals_prob <- predict(this_result, dataTest[, setdiff(colnames(dataTest), c("ResultVariable"))], type="prob")
        if(Sys.info()["sysname"] == "Windows") {
          stopCluster(cl);
          registerDoSEQ();
        }
        return(data.frame(real_value = dataTest$ResultVariable, predicted = pred_vals, probs = pred_vals_prob))
      }
      
      if(settings$split_test) {
        if(settings$regression) {
          pred_val <- predict(this_result, dataTest[, setdiff(colnames(dataTest), c("ResultVariable"))])
          results <- postResample(pred = pred_val, obs = dataTest$ResultVariable);
          results_split_rmse[[result_name]] <- results["RMSE"];
          results_split_r2[[result_name]] <- results["Rsquared"];
          
          if(settings$print_results) {
            print("Values obtained")
            print(data.frame(obs = dataTest$ResultVariable, pred = pred_val))
          }
        } else {
          results_split_roc[[result_name]] <- score_auc(method_name = result_name,
                                              fitted_object = this_result,
                                              eval_dataset = dataTest,
                                              expected_result = dataTest$ResultVariable,
                                              ignore_columns = c("ResultVariable"),
                                              plot = settings$plot_auc_test);
        }
      }
    },
    error=function(e){
      s <- sprintf("Error running algorithm: %s. %s", result_name, e);
      print(s);

      if(settings$regression) {
        results_rmse[[result_name]] <- 0;
        results_r2[[result_name]] <- 0;

        if(settings$split_test) {
          results_split_rmse[[result_name]] <- 0;
          results_split_r2[[result_name]] <- 0;
        }
      } else {
        results_roc[[result_name]] <- 0;

        if(settings$split_test) {
          results_split_roc[[result_name]] <- 0;
        }
      }
    })
  }
  ########################### loop training different models
  
  
  if(settings$regression) {
    results_mapping <- data.frame(names(results_rmse));
    names(results_mapping) <- c("algorithm");
    results_mapping$rmse <- results_rmse;
    results_mapping$r2 <- results_r2;
    
    if(settings$split_test) {
      if(length(results_split_rmse) > 0) {
        results_mapping_split <- data.frame(names(results_split_rmse));
        names(results_mapping_split) <- c("algorithm");
        results_mapping_split$rmse <- results_split_rmse;
        results_mapping_split$r2 <- results_split_r2;
      } else {
        results_mapping_split <- data.frame()
      }
    }
  } else {
    results_mapping <- data.frame(names(results_roc));
    if(length(results_mapping) > 0) {
      names(results_mapping) <- c("algorithm");
      results_mapping$roc <- results_roc;
    }
    
    if(settings$split_test) {
      if(length(results_split_roc) > 0) {
        results_mapping_split <- data.frame(names(results_split_roc));
        if(length(results_mapping_split) > 0) {
          names(results_mapping_split) <- c("algorithm");
          results_mapping_split$roc <- results_split_roc;
        }
      } else {
        results_mapping_split <- data.frame()
      }
    }
  }
  
  # print the results
  print("Folding results");
  print(results_mapping);
  
  if(settings$split_test) {
    print("Split results");
    print(data.frame(results_mapping_split))
  }
  
  if(Sys.info()["sysname"] == "Windows") {
    stopCluster(cl);
    registerDoSEQ();
  }
  
  cat("##########################################\n")
  cat("##########################################\n")
  cat("##########################################\n")
  cat("##########################################\n")
  cat("DO NOT USE ONLY THE BEST FINAL RESULT. THE CRITERIA IS TO ALWAYS SELECT THE BEST FROM THE FIRST STAGE, THEN SEE HOW WELL IT DID IN THE FINAL SCORE!\n")
  cat("##########################################\n")
  cat("##########################################\n")
  cat("##########################################\n")
  cat("##########################################\n")
  return(list(first_stage=results_mapping, final=results_mapping_split))
}
