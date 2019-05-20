get_pairs <- function(cor, higher_than = NULL, lower_than = NULL) {
  higher <- NULL;
  if(!is.null(higher_than)) {
    higher <- which(cor >= higher_than);
  }
  lower <- NULL;
  if(!is.null(lower_than)) {
    lower <- which(cor <= lower_than);
  }
  vals <- c(lower, higher);
  
  cols <- colnames(cor);
  width <- ncol(cor);
  height <- nrow(cor);
  for(i in vals) {
    line <- floor(i/width) + 1;
    at_line <- i %% width;
    
    # because of simmetry lets ignore the repeats
    if(at_line > line) {
      s <- sprintf("%s, %s, %f", cols[line], cols[at_line], cor[line, at_line]);
      print(s)
    }
  }
}