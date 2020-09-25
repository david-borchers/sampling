#' @export
ybar<-function(dat)
{
  apply(dat$y.value,1,mean) 
}