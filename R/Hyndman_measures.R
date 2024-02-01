#' Calculate some interesting features of the time series based on the tsfeatures package
#'
#' Using package tsfeatures (https://www.rdocumentation.org/packages/tsfeatures/versions/1.0.1)
#' Methods for extracting various features from time series data. The features provided are those
#' from Hyndman, Wang and Laptev (2013) <doi:10.1109/ICDMW.2015.104>, Kang, Hyndman and
#' Smith-Miles (2017) <doi:10.1016/j.ijforecast.2016.09.004> and from Fulcher,
#' Little and Jones (2013) <doi:10.1098/rsif.2013.0048>.
#'
#' @param ts imput time series.
measures.Hyndman<-function(ts){
  methods=c("length","acf_features","pacf_features",
            "entropy","nonlinearity",
            "hurst","stability","lumpiness",
            "unitroot_kpss","unitroot_pp",
            "stl_features",
            "max_level_shift",
            "max_var_shift",
            "max_kl_shift"
            # ,
            # "heterogeneity"
  )
  out=lapply(methods, function(method){
    tryCatch({
      switch(method,
             length = ,
             acf_features = ,
             pacf_features= ,
             entropy = ,
             nonlinearity = ,
             hurst = ,
             stability = ,
             lumpiness = ,
             unitroot_kpss = ,
             unitroot_pp = {
               tsfeatures::tsfeatures(ts, method, parallel = FALSE)
             },
             heterogeneity = {
               tsfeatures::tsfeatures(ts, method, parallel = FALSE)
             },
             stl_features = {
               tsfeatures::tsfeatures(ts, method, s.window='periodic', robust=TRUE, parallel = FALSE)
             },
             max_level_shift = {
               tsfeatures::tsfeatures(ts, method, trim=TRUE, parallel = FALSE)
             },
             max_var_shift = {
               tsfeatures::tsfeatures(ts, method, trim=TRUE, parallel = FALSE)
             },
             max_kl_shift = {
               tsfeatures::tsfeatures(ts, method, width=48, parallel = FALSE)
             }
      )
      # 0/s
    }, error = function(e) {
      ts=ts(c(1,2,3,4))
      struct = switch(method,
                      length = ,
                      acf_features = ,
                      pacf_features= ,
                      entropy = ,
                      nonlinearity = ,
                      hurst = ,
                      stability = ,
                      lumpiness = ,
                      unitroot_kpss = ,
                      unitroot_pp = {
                        tsfeatures::tsfeatures(ts, method , parallel = FALSE)
                      },
                      heterogeneity = {
                        tsfeatures::tsfeatures(ts, method , parallel = FALSE)
                      },
                      stl_features = {
                        tsfeatures::tsfeatures(ts, method, s.window='periodic', robust=TRUE, parallel = FALSE)
                      },
                      max_level_shift = {
                        tsfeatures::tsfeatures(ts, method, trim=TRUE, parallel = FALSE)
                      },
                      max_var_shift = {
                        tsfeatures::tsfeatures(ts, method, trim=TRUE, parallel = FALSE)
                      },
                      max_kl_shift = {
                        tsfeatures::tsfeatures(ts, method, width=48, parallel = FALSE)
                      }
      )
      fe=names(struct)
      out=rep(NA, length(fe))
      names(out)=fe
      out
    })
  })
  out=unlist(out)
  out
}
