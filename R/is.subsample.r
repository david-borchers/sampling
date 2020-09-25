#' @export
is.subsample <- function (dat) 
{
  # test if <dat> is of the type "subsample"
  inherits(dat, "subsample")
}