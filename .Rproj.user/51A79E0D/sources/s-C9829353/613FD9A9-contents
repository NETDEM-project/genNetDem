#'timestep_demographics
#'
#'Survival and recruitment of individuals at the end of each demographic timestep.
#'
#'@param indiv_data the current indiv_data dataframe (including data on individual ID, social group, spatial location and survival)
#'@param full_indiv_data the current full_indiv_data dataframe containing the information for all individuals present during the study
#'@param recruitment = indicates whether recruitment into the population is possible. Defaults to TRUE
#'@param mps the mean population survival. Defaults to NULL at which point it is calculated from indiv_data$survival.
#'@param lvps the variation in survival probability used to generate initial values in the function. Relatively unimportant for most uses of the code where this information is replaced subsequently. Defaults to 0.5.
#'
#'@details Survival is stochastic based on each individual’s survival probability. The recruitment rate is calculated from population mean survival probability and is then implemented stochastically via a draw from a Poisson distribution. Note that for recruitment into social groups: a) when there are no social groups (each individual is its own group) then individuals are recruited into empty group locations to replace dead individuals where possible and then if not new locations are sampled; b) When there are social groups then individuals are sampled into social groups weighted by the inverse of group size (individuals more likely to be recruited into smaller groups).
#'
#'@return a list of three parts. 1)  The new iteration of indiv_data; 2) the updated version of full_indiv_data; 3) a new distance matrix for the updated population.
#'
#'@export

timestep_demographics<-function(indiv_data,full_indiv_data,recruitment=TRUE,
                                mps=0.95,lvps=0.5){

  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(is.data.frame(full_indiv_data)==FALSE){stop("Correctly formatted full_indiv_data is required")}
  if(ncol(full_indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted full_indiv_data is required")}
  if(mps<0|mps>1){stop("Survival probability must be between 0 and 1")}

  #Survival on logit scale
  lmps<-car::logit(mps,adjust=0.001)
  #Recruitment rate
  rr<-(1/mps-1)

  survived<-stats::rbinom(nrow(indiv_data),1,prob=indiv_data$survival)
  dead<-sum(survived==0)
  which_dead<-which(survived==0)

  if(recruitment==TRUE&dead>0){

  indivs_which_dead<-indiv_data$indivs[which(survived==0)]
  groups_which_dead<-indiv_data$groups[which(survived==0)]

  n_recruits<-stats::rpois(1,rr*(sum(survived==1)))

  n<-length(unique(indiv_data$indivs))
  ng<-length(unique(indiv_data$groups))

  recruit_groups<-numeric()
  if(n==ng){
    tl<-length(groups_which_dead)
    if(tl==1&n_recruits>0){
      recruit_groups[1]<-groups_which_dead
    }
    if(tl>1&tl<=n_recruits){
      recruit_groups[1:tl]<-sample(groups_which_dead,tl,replace=FALSE)
    }
    if(tl>1&tl>n_recruits){
      recruit_groups[1:n_recruits]<-sample(groups_which_dead,n_recruits,replace=FALSE)
    }
    if(length(recruit_groups)<n_recruits){
      l_tmp<-length(recruit_groups)
      l_rem<-n_recruits-length(recruit_groups)
      recruit_groups[(l_tmp+1):(l_tmp+l_rem)]<-seq(max(indiv_data$groups)+1,max(indiv_data$groups)+l_rem,1)
    }
  }

  if(n>ng){
    recruit_groups<-sample(sort(unique(indiv_data$groups)),n_recruits,replace=TRUE,prob=1/table(indiv_data$groups))
  }

  new_groups<-recruit_groups[recruit_groups%in%indiv_data$groups==FALSE]
  new_x<-stats::runif(length(new_groups),0,1)
  new_y<-stats::runif(length(new_groups),0,1)
  new_gi<-data.frame(new_groups,new_x,new_y)

  recruit_indivs<-seq(max(indiv_data$indivs)+1,max(indiv_data$indivs)+n_recruits,1)

  new_indiv_data<-data.frame(recruit_indivs,recruit_groups)
  new_indiv_data$y<-new_indiv_data$x<-rep(NA,nrow(new_indiv_data))
  names(new_indiv_data)<-c("indivs","groups","x","y")
  for(i in 1:nrow(new_indiv_data)){
    if(new_indiv_data$groups[i]%in%new_groups==TRUE){
      new_indiv_data$x[i]<-new_gi$new_x[which(new_gi$new_groups==new_indiv_data$groups[i])]
      new_indiv_data$y[i]<-new_gi$new_y[which(new_gi$new_groups==new_indiv_data$groups[i])]
    }
    if(new_indiv_data$groups[i]%in%new_groups==FALSE){
      new_indiv_data$x[i]<-unique(indiv_data$x[indiv_data$groups==new_indiv_data$groups[i]])
      new_indiv_data$y[i]<-unique(indiv_data$y[indiv_data$groups==new_indiv_data$groups[i]])
    }
  }

  #This may need updating once they have been assigned a position in a social network
  #so could revert to NAs or leave here as a dummy variable to be replaced
  new_indiv_data$survival<-boot::inv.logit(stats::rnorm(nrow(new_indiv_data),lmps,lvps))

  full_indiv_data<-rbind(full_indiv_data,new_indiv_data)

  indiv_data<-rbind(indiv_data,new_indiv_data)

  } #end if loop for if recruitment is TRUE

  indiv_data$alive<-1
  indiv_data$alive[which_dead]<-0
  indiv_data<-indiv_data[which(indiv_data$alive==1),]
  indiv_data<-indiv_data[,1:5]

  dist_mat<-as.matrix(stats::dist(cbind(indiv_data$x,indiv_data$y),upper=TRUE))

  output<-list(indiv_data,full_indiv_data,dist_mat)

  return(output)

}
