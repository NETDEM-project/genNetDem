#'basic_survival
#'
#'Generates survival probabilities for all individuals in the population based on draws from a Gaussian distribution on a logit scale.
#'
#'@param indiv_data current indiv_data dataframe
#'@param mps Mean survival probability per demographic timestep (between 0 and 1)
#'@param lvps Variance in survival probability on a logit scale
#'@details Generates survival probabilities for all individuals in the population based on draws from a Gaussian distribution on a logit scale.
#'@return The new indiv_data dataframe with newly calculated survival probabilities added
#'@export

basic_survival<-function(indiv_data,mps,lvps){
  if(mps<0|mps>1){stop("Survival probability must be between 0 and 1")}
  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  lmps<-car::logit(mps,adjust=0.001)
  indiv_data$survival<-boot::inv.logit(stats::rnorm(nrow(indiv_data),lmps,lvps))
  return(indiv_data)
}
