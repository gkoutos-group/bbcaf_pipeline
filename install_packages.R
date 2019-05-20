install.packages("here")
install.packages("ModelMetrics") #this dependency fails if it tries within caret
install.packages("caret", dependencies = c("Depends", "Suggests"))

install.packages("readr")
install.packages("sva")

# ggplot
install.packages("ggplot2", dependencies = c("Depends", "Suggests"))
install.packages("munsell")
# ggplot

libs <- c("actfun", "adjust", "alpha", "alpha2", "alpha3", "alpha4", "alpha.pvals.expli", "bag", "beta", "C", "coefImp", "coeflearn", "coefReg", "colsample_bytree", "col_sample_rate", "cost", "Cost", "cp", "criteria", "cut.off.growth", "decay", "degree", "depth", "df", "diagonal", "dimen", "direction", "distance", "eps", "epsilon", "estimateTheta", "estimator", "eta", "final_smooth", "fL", "gamma", "hidden_dropout", "hp", "interaction.depth", "iter", "k", "K", "kappa", "kernel", "kmax", "K.prov", "L", "l2reg", "lambda", "lambda2", "lambda.freqs", "layer1", "layer2", "layer3", "learn_rate", "length", "link", "Loss", "loss_type", "M", "maxdepth", "max_depth", "max.gen", "maxinter", "maxInteractionOrder", "max.iter", "max.num.rule", "maxvar", "method", "mfinal", "min_child_weight", "mincriterion", "minibatchsz", "Minkowski", "min_rows", "MinWeights", "mode", "model", "momentum", "MPD", "mstop", "mtry", "ncomp", "negativeThreshold", "newdim", "nhid", "nIter", "nleaves", "n.minobsinnode", "None", "nprune", "nrounds", "nt", "ntrees", "n.trees", "nu", "NumFolds", "num_iter", "num.labels", "NumOpt", "numRandomCuts", "num_trees", "NumVars", "oblique.splits", "parallel", "penalty", "popu.size", "predFixed", "prior", "prune", "pruned", "q", "qval", "R", "repeats", "scale", "score", "select", "shrinkage", "shrinkage_type", "sigma", "size", "smooth", "sp", "span", "split", "subclasses", "subsample", "tau", "threshold", "topo", "tree_depth", "treesize", "trials", "type.mf", "usekernel", "variable.selection", "vars", "visible_dropout", "weight", "Weight", "winnow", "xdim", "xgenes", "xweight", "ydim", "xgboost", "kknn", "LogicReg", "deepnet", "LiblineaR", "RRF", "rocc", "adabag", "fastAdaboost", "doMC", "AppliedPredictiveModeling", "nnet", "rocc", "LiblineaR", "ROSE")


for(i in libs) {
	tryCatch(
		install.packages(i),
		error = function(e) {},
		warning = function(w) {} )
}


extra <- False
if(extra) {
    source("https://bioconductor.org/biocLite.R")
    biocLite()
    biocLite("sva")
    BiocInstaller::biocLite(c("genefilter", "gmodels","caret","caretEnsemble","doParallel","plyr","DESeq2","STRINGdb","igraph","hash","rain","pheatmap","genefilter","RColorBrewer","ggplot2","pheatmap","RColorBrewer","ggplot2","gridExtra","cowplot","grid","gridExtra","dplyr","missForest","Amelia","AnnotationDbi","AnnotationHub","ArrayExpress","pROC","AUC","BaySeq","biclust","bibtex","BiocGenerics","BiocParallel","biomaRt","Biostrings","BMA","BSgenome","car","caTools","chrom","clusterProfiler","corrplot","COSINE","curl","cvTools","data.table","DiffCorr","DO.db","doMC","DOSE","e1071","edgeR","Epi","foreach","foreign","GeneCycle","geneplotter","GenomicFeatures","GenomicRanges","ggthemes","git2r","GO.db","gplots","gProfileR","graph","Gviz","haven","h20","hda","Hmisc","httr","igraph","igraphdata","IRanges","jsonlite","KEGG.db","KEGGREST","KernSmooth","kernlab","knitr","lattice","limma","lme4","LogicReg","loo","magrittr","mboost","mclust","mcmc","MCMCpack","metafor","metap","mi","mice","mlogit","NbClust","NHANES","openxlsx","org.Dm.eg.db","org.hs.eb.db","org.Mm.eg.db","pamr","pcaMethods","penalized","perm","permute","pheatmap","plotly","plot3D","pwr","psych","pvclust","qvalue","rain","randomForest","rARPACK","RCircos","RColorBrewer","Rccp","RccpArmadillo","RccpEigen","RCurl","RCytoscape","readstata13","reshape2","Rgraphviz","RJSONIO","rmeta","robust","ROCR","rpart","rpart.plot","rPython","Rsamtools","rtracklayer","Rtsne","samplesize","shiny","shynyBS","snow","STRINGdb","stringr","TDARACNE","tidyr","tseries","tsne","VenDiagram","WGCNA","xtable","zoo"))
}
