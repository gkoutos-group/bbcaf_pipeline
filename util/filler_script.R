fill_dataset <- function(data, MERGING_DATA, MERGED_NAME="UNNAMED", conversion_relation, do_genderless=T, do_gender=T, remove_older_columns=F, new_continuous=T, new_categorical=T, verbose=F) {
  merging_data <- read.csv(MERGING_DATA)
  
  if(verbose) {
    s <- sprintf("Available variables are:")
    print(s)
    print(levels(unique(merging_data$VARIABLE_NAME_EXTENDED)))

    if(do_genderless) {
      s <- sprintf("Adding variables without gender information");
      print(s);
    }

    if(do_gender) {
      s <- sprintf("Adding variables with gender information");
      print(s);
    }

    if(remove_older_columns) {
      s <- sprintf("Removing original columns");
      print(s);
    }

    if(new_continuous) {
      s <- sprintf("Adding new continuous variables");
      print(s);
    }

    if(new_categorical) {
      s <- sprintf("Adding new categorical variables");
      print(s);
    }
  }

  for(i in names(conversion_relation)) {
    from_merge <- conversion_relation[[i]]
    at_source <- i
    
    if(length(colnames(data) == at_source) == 0) {
      stop(sprintf("Column %s not in data file.", at_source));
    } else if(length(unique(merging_data$VARIABLE_NAME_EXTENDED) == from_merge) == 0) {
      stop(sprintf("Parameter %s not in merge file.", from_merge));
    } else if(do_gender & nrow(merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]) != 1) {
      stop(sprintf("Parameter %s has invalid number of `FEMALE` values in merge file (%d).", from_merge, nrow(merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ])));
    } else if(do_gender & nrow(merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]) != 1) {
      stop(sprintf("Parameter %s has invalid number of `MALE` values in merge file (%d).", from_merge, nrow(merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ])));
    } else if(do_genderless & nrow(merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]) != 1) {
      stop(sprintf("Parameter %s has invalid number of `BOTH` values in merge file (%d).", from_merge, nrow(merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ])));
    }
  }
  
  new_columns <- list()
  
  for(i in names(conversion_relation)) {
    from_merge <- conversion_relation[[i]];
    at_source <- i;
    
    if(verbose) {
      s <- sprintf("Variable `%s` to be added with contents of %s on the merge file", at_source, from_merge);
      print(s);
    }

    #genderless block    
    if(do_genderless) {
      if(new_continuous) {
        new_varname <- paste("MERGED", MERGED_NAME, from_merge, sep="_");
        data[[new_varname]] <- (data[[at_source]] - merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION;
        new_columns[[new_varname]] <- T;
      }
      
      if(new_categorical) {
        new_varname <- paste("MERGED", MERGED_NAME, from_merge, "1SD", sep="_");
        data[[new_varname]] <- abs((data[[at_source]] - merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) <= 1;
        new_columns[[new_varname]] <- T;
        
        new_varname <- paste("MERGED", MERGED_NAME, from_merge, "2SD", sep="_");
        data[[new_varname]] <- abs((data[[at_source]] - merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) <= 2;
        new_columns[[new_varname]] <- T;
        
        new_varname <- paste("MERGED", MERGED_NAME, from_merge, "Higher-SD", sep="_");
        data[[new_varname]] <- abs((data[[at_source]] - merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "BOTH" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) > 2;
        new_columns[[new_varname]] <- T;
      }
    }
    
    #gender block
    if(do_gender) {
      if(new_continuous) {
        new_varname <- paste("GENDER_MERGED", MERGED_NAME, from_merge, sep="_");
        data[data$Female == 0, c(new_varname)] <- (data[data$Female == 0, ][[at_source]] - merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION;
        data[data$Female == 1, c(new_varname)] <- (data[data$Female == 1, ][[at_source]] - merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION;
        new_columns[[new_varname]] <- T;
      }
      
      if(new_categorical) {
        new_varname <- paste("GENDER_MERGED", MERGED_NAME, from_merge, "1SD", sep="_");
        data[data$Female == 0, c(new_varname)] <- abs((data[data$Female == 0, ][[at_source]] - merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) <= 1;
        data[data$Female == 1, c(new_varname)] <- abs((data[data$Female == 1, ][[at_source]] - merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) <= 1;
        new_columns[[new_varname]] <- T;
        
        new_varname <- paste("GENDER_MERGED", MERGED_NAME, from_merge, "2SD", sep="_");
        data[data$Female == 0, c(new_varname)] <- abs((data[data$Female == 0, ][[at_source]] - merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) <= 2;
        data[data$Female == 1, c(new_varname)] <- abs((data[data$Female == 1, ][[at_source]] - merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) <= 2;
        new_columns[[new_varname]] <- T;
        
        new_varname <- paste("GENDER_MERGED", MERGED_NAME, from_merge, "Higher-SD", sep="_");
        data[data$Female == 0, c(new_varname)] <- abs((data[data$Female == 0, ][[at_source]] - merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "FEMALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) > 2;
        data[data$Female == 1, c(new_varname)] <- abs((data[data$Female == 1, ][[at_source]] - merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$MEAN_VALUE)/ merging_data[merging_data$GENDER == "MALE" & merging_data$VARIABLE_NAME_EXTENDED == from_merge, ]$STANDARD_DEVIATION) > 2;
        new_columns[[new_varname]] <- T;
      }
    }
    
    if(remove_older_columns) {
      data[[at_source]] <- NULL;
    }
  }
  
  ret <- list(new_columns=new_columns, data=data)
  
  return(ret);
}
