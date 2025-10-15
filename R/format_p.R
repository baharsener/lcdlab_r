#' Format p-values in APA style
#'
#' @param pval Numeric p-value or vector of p-values
#' @return Character string(s) formatted for reporting
#' @export
format_p <- function(pval) {
  if (!is.numeric(pval)) stop("pval must be numeric")
  sapply(pval, function(p) {
    if (is.na(p)) return(NA_character_)
    if (p < .001) {
      "p < .001"
    } else if (p < .01) {
      paste0("p = ", stringr::str_remove(format(round(p, 3), nsmall = 3), "^0"))
    } else {
      paste0("p = ", stringr::str_remove(format(round(p, 2), nsmall = 2), "^0"))
    }
  })
}
