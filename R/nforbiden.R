#' forbiden patterns
#'
#' This function calculates Number of "forbiden patterns" (cf. Amigo 2010)
#' Using package statcomp (https://www.rdocumentation.org/packages/statcomp/versions/0.0.1.1000/topics/global_complexity)
#'
#' @param time_serie input time series data
#' @param ndemb (OPTIONAL) If x is given, the embedding dimension (ndemb) is required.
#' @return Number of "forbiden patterns" (cf. Amigo 2010)
measure.nforbiden <- function(time_serie, ndemb) {
  # suppressMessages(library(statcomp))
  result = tryCatch({
    out=statcomp::global_complexity(x = time_serie, ndemb = ndemb)
    out[3]
  }, error = function(e) {
    out=NA
    names(out)="nforbiden"
    out
  })
  return (result)
}



