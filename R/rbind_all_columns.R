#' Combine two data frames while preserving all columns
#'
#' @param x First data frame
#' @param y Second data frame
#' @return Combined data frame with all columns from both
#' @export
rbind.all.columns <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA
  return(rbind(x, y))
}
