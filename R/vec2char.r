#' @export
# collapse vector into a character with brackets outside and commas between
vec2char<-function(indices,sep=",")
{
  paste("(",paste(as.character(indices),collapse=sep),")",sep="")
}