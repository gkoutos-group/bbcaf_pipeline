library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)

source("../../bbcaf_pipeline/util/settings.R")

source("../../bbcaf_pipeline/pipe.R", chdir=T)

load_set <- function() {
    n <- 100
    p <- 40
    sigma <- 1
    set.seed(1)
    sim <- mlbench.friedman1(n, sd = sigma)
    colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                        paste("bogus", 1:5, sep = ""))
    bogus <- matrix(rnorm(n * p), nrow = n)
    colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
    data <- cbind(sim$x, bogus)

    normalization <- preProcess(data)
    data <- predict(normalization, data)
    data <- as.data.frame(data)

    data$ResultVariable <- sim$y
    data$ResultVariable[data$ResultVariable <= 14.613] <- 0
    data$ResultVariable[data$ResultVariable > 0] <- 1
    data$ResultVariable <- as.factor(data$ResultVariable)
    levels(data$ResultVariable) <- c("X0", "X1")
    return(list(data=data))
}


settings$feature_selection <- T
settings$seed_key <- 123456
settings$algos <- c("rf")

ret = pipe(database=load_set, settings=settings)

