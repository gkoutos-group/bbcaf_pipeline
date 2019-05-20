
library(ggplot2)
library(caret)
library(rgl)

run_pca_plot <- function(df, ignore_columns=c(NULL), preprocessMethod = c("pca"), labels=NULL, values=NULL, col="", title="Title", scale = F) {
  if(Sys.info()["sysname"] == "Windows") {
    WD <- "//MDS/USER/S-Z/VXR610/Desktop/PhD/Hands On/common_pipeline";
  } else {
    WD <- paste(path.expand("~"), "/PhD/Hands On/common_pipeline/", sep="")
  }
  setwd(WD)
  source("../common_pipeline/datasets/load_bbcaf.R");
  
  if(is.null(labels)) {
    labels <- names(levels(df$ResultVariable))
  }
  
  if(is.null(values)) {
    values <- c("blue", "green3")
  }
  
  preProcValues <- preProcess(df[, setdiff(colnames(df), ignore_columns)], method = preprocessMethod)
  print(preProcValues$method)
  print(preProcValues$rotation)
  df_pca <- predict(preProcValues, df[, setdiff(colnames(df), ignore_columns)])
  
  df_pca[, ignore_columns] <- df[, ignore_columns]
  # df_pca$ResultVariable <- df$ResultVariable
  if(scale) {
  a <- ggplot(df_pca, aes(PC1, PC2)) + geom_point(aes(colour = ResultVariable)) +
    scale_color_manual(labels = labels, values = labels) + labs(col = col, title = title)
  }
  else {
    a <- ggplot(df_pca, aes(PC1, PC2)) + geom_point(aes(colour = ResultVariable)) + labs(col = col, title = title)
  }
  plot(a)
  
  return(list(plot=a, data=df_pca))
}