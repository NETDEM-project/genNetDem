#'group_calc
#'
#'internal function to generate group sizes
#'
#'@param ni number of individuals
#'@param mgs mean group size
#'
#'@return vector of group sizes
#'
#'@export

group_calc<-function(ni,mgs){
  divides<-sort(c(0,stats::runif((ni/mgs)-1,1,ni),ni+1))
  grs<-numeric()
  for(ps in 1:(length(divides)-1)){
    grs[ps]<-length(ib_int(divides[ps],divides[ps+1]))
  }
  return(grs)
}
