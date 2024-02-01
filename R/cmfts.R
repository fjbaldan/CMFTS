#' Calculates the complexity measures of a dataframe
#'
#' Gets the complexity measures of a dataframe composed by
#' time series. Each row contains the values of a time series.
#'
#' @param dataset Data to process (data.frame). Each row contains a time series.
#' @param n_cores Number of cores for parallel procesing (integer).
#' @param na Presence of NAs in the output dataframe (logical). It is recommended to use True in this parameter.
#' @param timeout Maximum calculation time allowed per method.
#' @return A dataframe containing the 41 time series characteristics for each time series processed.
#' @export
#' @import stats R.utils parallel
cmfts <- function(dataset=NULL, n_cores=(parallel::detectCores()-1), na=T, timeout=999999) {
  if(is.null(dataset)==TRUE){
    return(0)
  }

  n_timeseries = nrow(dataset)

  run <- function(i) {
    time_serie=as.ts(as.numeric((dataset[i,])))
    #time_serie=ts((dataset[i,]))
    result=NULL
    # Time series characteristics
    aux=withTimeout({measure.lempel_ziv(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.lempel_ziv(NA) else aux )

    aux=withTimeout({measure.aproximation_entropy(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.aproximation_entropy(NA) else aux )

    aux=withTimeout({measure.sample_entropy(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.sample_entropy(NA) else aux )

    aux=withTimeout({measure.permutation_entropy(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.permutation_entropy(NA) else aux )

    aux=withTimeout({measure.shannonEntropy(time_serie,"CS")}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.shannonEntropy(NA,"CS") else aux )

    aux=withTimeout({measure.shannonEntropy(time_serie,"SG")}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.shannonEntropy(NA,"SG") else aux )

    aux=withTimeout({measure.spectral_entropy(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.spectral_entropy(NA) else aux )

    aux=withTimeout({measure.nforbiden(time_serie,6)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.nforbiden(NA) else aux )

    aux=withTimeout({measure.kurtosis(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.kurtosis(NA) else aux )

    aux=withTimeout({measure.skewness(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) measure.skewness(NA) else aux )

    aux=withTimeout({measures.Hyndman(time_serie)}, timeout = timeout, onTimeout = "silent")
    result <- c(result, if(is.null(aux)) sapply(measures.Hyndman(as.vector(0)),function(x){NA}) else aux )

    result
  }

  if(.Platform$OS.type=="windows"){
    results <- mclapply(1:n_timeseries, run, mc.cores=1)
    # results <- lapply(1:n_timeseries, run)
  }else{
    results <- mclapply(1:n_timeseries, run, mc.cores=n_cores)
    # results <- mclapply(1:n_timeseries, run)
  }
  # save=results
  # saveRDS(object = results, file = "results_care.RDS")

  # Optional elimination of NA
  if(na==F){
    a=which(lapply(results,function(x){
      sum(is.na(x))
    })>=1)

    if(length(a)>=1){
      results=results[-a]
    }
  }
  n_timeseries=length(results)

  if(!is.null(results)) {
    experiment_result=data.frame(t(as.data.frame(results)))
    rownames(experiment_result) <- NULL
  }

  return (experiment_result)
}

