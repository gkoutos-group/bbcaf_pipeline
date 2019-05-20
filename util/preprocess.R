
library(caret)
library(ROSE)

cluster_variables_20171025 <- function(ndf, cut_groups = NULL, cut_height=NULL, method="SUM") {
  if(method != "SUM" & method != "FIRST") {
    print("INVALID METHOD FOR cluster_variables_20171025!")
    return(-1);
  }
  set.seed(seed);
  preProcValues <- preProcess(ndf, method = c("center", "scale"));
  ndf <- predict(preProcValues, ndf);
  
  hr <- hclust(dist(abs(cor(na.omit(ndf)))))
  groups <- data.frame(cutree(hr, k=cut_groups, h=cut_height))
  colnames(groups) <- c("group")
  groups$names <- rownames(groups)
  
  print("The groups are:")
  print(groups)
  
  for(i in groups$names) {
    group_n <- groups[groups$names == i, ]$group;
    n_var <- paste("VAR", group_n, sep="")
    if(sum(n_var %in% colnames(ndf)) == 0) {
      ndf[[n_var]] <- 0
      if(method == "FIRST") {
        ndf[[n_var]] <- ndf[[i]] + ndf[[n_var]]
      }
    }
    if(method == "SUM") {
      ndf[[n_var]] <- ndf[[i]] + ndf[[n_var]]
    }
    
    ndf[[i]] <- NULL
  }
  
  return(ndf)
}

set_preprocess <- function(data, 
                           split_ratio, 
                           preprocess_methods=c("center", "scale", "knnImpute"), 
                           seed=123, 
                           remove_na_rows=T, 
                           do_nzv = T,
                           preprocess_individually = F) {
    log(sprintf("Preprocessing methods are:"));
    log(sprintf("%s", preprocess_methods));
    
    if(do_nzv) {
      nzv <- nearZeroVar(data);
      if(length(nzv) > 0) {
        if(length(intersect(colnames(data)[nzv], c("ResultVariable"))) == 1) {
            log(sprintf("nzv tried to remove ResultVariable!"))
        }
        vals <- setdiff(colnames(data)[nzv], "ResultVariable")
        log(sprintf("Removed column (because near zero variance): %s", vals));
        data <- data[, setdiff(colnames(data), vals)];
      }
    }
    
    ######################### FIX ?
    if(length(intersect(c("Bio_Panel"), colnames(data))) == 1) {
        print("Separating train with Bio_Panel == CVD1 and test with Bio_Panel == CVD2")
        dataTrain <- data[data$Bio_Panel == "CVD1", setdiff(colnames(data), "Bio_Panel")]
        dataTest <- data[data$Bio_Panel == "CVD2", setdiff(colnames(data), "Bio_Panel")]
    } else {
      set.seed(seed);
      trainIndex <- createDataPartition(data$ResultVariable, p = split_ratio, list = FALSE, times = 1);
  
      dataTrain <- data[trainIndex,];
      dataTest <- data[-trainIndex,];
    }
    
    yTrain <- dataTrain$ResultVariable;
    dataTrain$ResultVariable <- NULL;
    yTest <- dataTest$ResultVariable;
    dataTest$ResultVariable <- NULL;
    
    if(!is.null(preprocess_methods)) {
      if(preprocess_individually) {
        set.seed(seed);
        preProcValues <- preProcess(dataTrain, method = preprocess_methods);
        print("TRAIN: set_preprocess")
        print(preProcValues)
        dataTrain <- predict(preProcValues, dataTrain);
        
        set.seed(seed);
        preProcValues <- preProcess(dataTest, method = preprocess_methods);
        print("TEST: set_preprocess")
        print(preProcValues)
        dataTest <- predict(preProcValues, dataTest);
      } else {
        set.seed(seed);
        preProcValues <- preProcess(dataTrain, method = preprocess_methods);
        print("set_preprocess")
        print(preProcValues)
        dataTrain <- predict(preProcValues, dataTrain);
        dataTest <- predict(preProcValues, dataTest);
      }
    }
    
    ################################################################# FIX?
    for(i in colnames(dataTrain)) {
        if(class(dataTrain[[i]]) == "factor") {
            if(length(levels(dataTrain[[i]])) == 1) {
                log(sprintf("Removed column (because amount of levels in factors = 1): %s", i))
                dataTrain[[i]] <- NULL
              }
          }
      }
    for(i in colnames(dataTest)) {
      if(class(dataTest[[i]]) == "factor") {
          if(length(levels(dataTest[[i]])) == 1) {
              log(sprintf("Removed column (because amount of levels in factors = 1): %s", i))
              dataTest[[i]] <- NULL
            }
        }
    }
    ################################################################# FIX?
    
    dataTrain$ResultVariable <- yTrain;
    dataTest$ResultVariable <- yTest;
    
    if(remove_na_rows == T) {
      dataTrain <- dataTrain[rowSums(is.na(dataTrain)) == 0, ];
      dataTest <- dataTest[rowSums(is.na(dataTest)) == 0, ];
    }
    
    # balance_with_sample = "rose"
    # if(!is.null(balance_with_sample)) {
    #   print("BALANCING INSIDE PREPROCESS")
    #   set.seed(seed); ##did not have before
    #   log(sprintf("Seed value for sampling is: %d", seed))
    #   log(sprintf("Distribution of train data originally was:"))
    #   log(table(dataTrain$ResultVariable))
    #   log(sprintf("Sampling technique is: %s", balance_with_sample))
    #   if(balance_with_sample == "upsample") {
    #     dataTrain <- upSample(x = dataTrain,
    #                           y = dataTrain$ResultVariable)
    #     dataTrain$ResultVariable <- dataTrain$Class
    #     dataTrain$Class <- NULL
    #   } else if(balance_with_sample == "downsample") {
    #     dataTrain <- downSample(x = dataTrain,
    #                             y = dataTrain$ResultVariable)
    #     dataTrain$ResultVariable <- dataTrain$Class
    #     dataTrain$Class <- NULL
    #   } else if(balance_with_sample == "rose") {
    #     # for(i in (colnames(dataTrain))) {
    #     #   print(i)
    #     #   print(class(dataTrain[[i]]))
    #     #   print(table(dataTrain[[i]]))
    #     # }
    #     # print(length(levels(dataTrain$ResultVariable)))
    # 
    #     # for(i in colnames(dataTrain)) {
    #     #   dataTrain[[i]] <- factor(dataTrain[[i]])
    #     #   if(length(levels(dataTrain[[i]])) <= 1) {
    #     #     print(i)
    #     #     print(levels(dataTrain[[i]]))
    #     #   }
    #     # }
    # 
    #     dataTrain <- ROSE(ResultVariable ~ .,
    #                       data = dataTrain, seed=seed)$data
    #   } else {
    #     log(sprintf("Invalid option for preprocess:set_preprocess:balance_with_sample: %s", balance_with_sample));
    #     return(-1)
    #   }
    # }
    
    return(list(dataTrain=dataTrain, dataTest=dataTest));
}

set_preprocess_already_splitted <- function(dataTrain, dataTest, preprocess_methods=c("center", "scale", "knnImpute"), seed=123, remove_na_rows=T, preprocess_individually = F) {
  log(sprintf("Preprocessing methods is: %s", preprocess_methods));
  
  yTrain <- dataTrain$ResultVariable;
  dataTrain$ResultVariable <- NULL;
  yTest <- dataTest$ResultVariable;
  dataTest$ResultVariable <- NULL;

  if(!is.null(preprocess_methods)) {
    if(preprocess_individually) {
      set.seed(seed);
      preProcValues <- preProcess(dataTrain, method = preprocess_methods);
      dataTrain <- predict(preProcValues, dataTrain);
      print("TRAIN: set_preprocess_already_splitted")
      print(preProcValues)
      
      set.seed(seed);
      preProcValues <- preProcess(dataTest, method = preprocess_methods);
      dataTest <- predict(preProcValues, dataTest);
      print("TEST: set_preprocess_already_splitted")
      print(preProcValues)
    } else {
      set.seed(seed);
      preProcValues <- preProcess(dataTrain, method = preprocess_methods);
      print("set_preprocess_already_splitted")
      print(preProcValues)
      dataTrain <- predict(preProcValues, dataTrain);
      dataTest <- predict(preProcValues, dataTest);
    }
  }
  
  dataTrain$ResultVariable <- yTrain;
  dataTest$ResultVariable <- yTest;
  
  if(remove_na_rows == T) {
    dataTrain <- dataTrain[rowSums(is.na(dataTrain)) == 0, ];
    dataTest <- dataTest[rowSums(is.na(dataTest)) == 0, ];
  }
  
  return(list(dataTrain=dataTrain, dataTest=dataTest));
}

set_preprocess_one_set <- function(data, preprocess_methods=c("center", "scale", "knnImpute"), remove_na_rows=F, do_nzv = T) {
  log(sprintf("Preprocessing methods is: %s", preprocess_methods));
  
  nzv <- nearZeroVar(data);
  if(length(nzv) > 0 & do_nzv) {
    if(length(intersect(colnames(data)[nzv], c("ResultVariable"))) == 1) {
      log(sprintf("nzv tried to remove ResultVariable!"))
    }
    vals <- setdiff(colnames(data)[nzv], "ResultVariable")
    log(sprintf("Removed column (because near zero variance): %s", vals));
    data <- data[, setdiff(colnames(data), vals)];
  }

  y <- data$ResultVariable;
  data$ResultVariable <- NULL;
  
  preProcValues <- preProcess(data, method = preprocess_methods);
  data <- predict(preProcValues, data);

  for(i in colnames(data)) {
    if(class(data[[i]]) == "factor") {
      if(length(levels(data[[i]])) == 1) {
        log(sprintf("Removed column (because amount of levels in factors = 1): %s", i))
        data[[i]] <- NULL
      }
    }
  }
  data$ResultVariable <- y;

  if(remove_na_rows) {
    data <- data[rowSums(is.na(data)) == 0, ];
  }

  ret <- NULL;
  ret$data <- data;

  return(ret);
}

set_preprocess_nzv <- function(data, do_nzv = T) {
  nzv <- nearZeroVar(data);
  if(length(nzv) > 0 & do_nzv) {
    if(length(intersect(colnames(data)[nzv], c("ResultVariable"))) == 1) {
      log(sprintf("nzv tried to remove ResultVariable!"))
    }
    vals <- setdiff(colnames(data)[nzv], "ResultVariable")
    log(sprintf("Removed column (because near zero variance): %s", vals));
    data <- data[, setdiff(colnames(data), vals)];
  }
  
  ret <- NULL;
  ret$data <- data;
  
  return(ret);
}

set_preprocess_transform_factor <- function(df) {
  # get all columns that are factor
  w <- which(sapply( df, class ) == 'factor' );
  
  # get all columns with 2 factors
  c <- which(sapply(w, function(e) { length(levels(df[[e]])) }) == 2);
  
  for(i in names(c)) {
    if(i == "ResultVariable") {
      next;
    }
    nvar <- paste("HOT_ENCODED", i, sep="_");
    
    df[[nvar]] <- as.numeric(df[[i]]);
    df[is.na(df[[nvar]]), nvar] <- 0;
    df[df[[nvar]] == 2, nvar] <- -1;
    
    df[[i]] <- NULL;
  }
  
  return(list(data=df));
}

set_preprocess_remove_columns_na <- function(df, fraction_to_keep_col=0.7) {
  log(sprintf("Removed column (because amount of NA >= %f): %s", fraction_to_keep_col, colnames(df)[colSums(is.na(df)) >= nrow(df)*fraction_to_keep_col]));
  removed_columns <- colnames(df)[colSums(is.na(df)) < nrow(df)*fraction_to_keep_col]
  df <- df[, colSums(is.na(df)) < nrow(df)*fraction_to_keep_col]
  
  return(list(data=df, removed_columns = removed_columns));
}

set_preprocess_remove_rows_na <- function(df, fraction_to_keep_row=0.7) {
  begin <- nrow(df)
  df <- df[rowSums(is.na(df)) < ncol(df)*(fraction_to_keep_row), ]
  log(sprintf("Amount of removed rows (because amount of NA >= %f): %.0f", fraction_to_keep_row, begin - nrow(df)));
  
  return(list(data=df));
}

set_preprocess_get_some_columns <- function(df, seed = 123) {
  set.seed(seed)
  removal_of_variables = floor(runif(1, 1, 2^(ncol(df)) + 1))
  
  pop_var <- function(val, depth = 1, keep_or_not = NULL) {
    if(is.null(keep_or_not)) {
      keep_or_not <- rep(T, ncol(df))
    }
    cur_eval <- 2**(ncol(df) - depth + 1)
    
    if(val >= cur_eval) {
      keep_or_not[depth] = F
    }
    
    val <- val %% cur_eval
    if(depth <= ncol(df) & val > 0) {
      return(pop_var(val = val, depth = depth + 1, keep_or_not = keep_or_not))
    }
    return(keep_or_not)
  }
  
  keep_or_not <- union(colnames(df)[pop_var(val = removal_of_variables)], "ResultVariable")
  log(sprintf("Removed column (because random selection): %s", setdiff(colnames(df), keep_or_not)))
  return(list(data=df[, keep_or_not], columns_kept=keep_or_not))
}
