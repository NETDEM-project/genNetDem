#'jpc
#'
#'Internal function used in interaction generation
#'
#'@param group column of matrix (see use)
#'@param pm used to adjust social network connection strengths to probabilities of being sampled in the same group.
#'@param float used to provide a minimum probability of being sampled in the same group to allow all groups to be filled.
#'@return the join probabilities for candidates to join an interaction
#'
#'@export

jpc<-function(group,pm,float){
  ret<-prod(group)+sum(group)/pm+float
  return(ret)
}
