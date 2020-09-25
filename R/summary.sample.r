#' @export
summary.sample<-function(dat)
{
  if(!is.sample(dat)) stop("Argument 'dat' must be of class 'sample'")
  if(dat$M>dat$m) {
    cat("\n         Cluster Sample Summary")
    cat("\n=============================================")
    cat("\n                           number of clusters: ",dat$M)
    cat("\n                   number of clusters sampled: ",dat$m)
    #    cat("\n                            cluster sizes (N): ",dat$N)
    #    cat("\n                   mean N across all clusters: ",dat$mu.x)
    cat("\n                             clusters sampled: ",dat$units)
    cat("\n response variable totals in sampled clusters: ",format(dat$y.value))
    cat("\n                   mean N in sampled clusters: ",mean(dat$x.value))
    cat("\n=============================================\n")
  } else if(dat$M>1) {
    cat("\n      Stratified random Sample Summary")
    cat("\n=============================================")
    for(i in 1:dat$M) {
      cat("\n Stratum ",i)
      cat("\n---------------")
      cat("\n                          population size (N): ",dat[[i]]$N)
      cat("\n                              sample size (n): ",dat[[i]]$n)
      cat("\n                   population units in sample: ",dat[[i]]$units)
      cat("\n             sample mean of response variable: ",format(mean(dat[[i]]$y.value)))
      if(is.null(dat[[i]]$mu.x)) {
        cat("\n (no auxiliary variable)")
      } else {
        cat("\n      population mean of auxiliary variable: ",format(dat[[i]]$mu.x))
        cat("\n          sample mean of auxiliary variable: ",format(mean(dat[[i]]$x.value)))
        cat("\n correlation between response and auxiliary: ",format(cor(as.vector(dat[[i]]$y.value),as.vector(dat[[i]]$x.value))))
      }
    }
    cat("\n=============================================\n")
  } else {
    cat("\n      Simple random Sample Summary")
    cat("\n=============================================")
    cat("\n                          population size (N): ",dat$N)
    cat("\n                              sample size (n): ",dat$n)
    cat("\n                   population units in sample: ",dat[[1]]$units)
    cat("\n             sample mean of response variable: ",format(mean(dat[[1]]$y.value)))
    if(is.null(dat[[1]]$mu.x)) {
      cat("\n (no auxiliary variable)")
    } else {
      cat("\n        population mean of auxiliary variable: ",format(dat[[1]]$mu.x))
      cat("\n            sample mean of auxiliary variable: ",format(mean(dat[[1]]$x.value)))
      cat("\n   correlation between response and auxiliary: ",format(cor(as.vector(dat[[1]]$y.value),as.vector(dat[[1]]$x.value))))
    }
    cat("\n=============================================\n")
  }
}