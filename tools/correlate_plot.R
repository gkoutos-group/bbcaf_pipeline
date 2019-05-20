

library(here)
setwd(here())

source("../common_pipeline/datasets/load_set.R")
source("../common_pipeline/util/preprocess.R")

library(ggplot2)
library(dplyr)
library(reshape2)

df <- load_set()
df$ResultVariable <- NULL

# df <- set_preprocess_remove_columns_na(df, fraction_to_keep=0.7)$data

ndf <- df[, which(lapply(df, class) != "factor")]

correlation <- cor(ndf, use="p", method="pearson")
cor_var <- qplot(x=Var1, y=Var2, data=melt(correlation), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
cor_var
