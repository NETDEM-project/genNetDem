#'ib_int
#'
#'internal function to find integers between two non-integer number
#'
#'@param a an non-integer number
#'@param b a second non-integer number (higher)
#'
#'@details internal function to find integers between two non-integer number
#'
#'@return a vector of the integers between them
#'
#'@export

ib_int <- function(a, b) {
  u <- sort(c(a, b))
  res <- setdiff(ceiling(u[1]):floor(u[2]), c(a, b))
  if (!length(res)) {
    NULL
  } else if(length(res)==1){
    res
  } else if(res[1]>res[2]){
    NULL
  } else{
    res
  }
}
