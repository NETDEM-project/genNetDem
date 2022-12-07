#'indiv_info_add
#'
#'Adds trait data for individuals at a demographic timestep
#'
#'@param indiv_info existing indiv_info dataframe.
#'@param ii_tag information on variables already included in the dataframe.
#'@param indiv_data dataframe containing data on individual IDs, spatial locations and social group IDs
#'@details Adds trait data (e.g. sex, body size etc.) for newly recruited individuals.
#'
#'@return A nested list. The first element of the list contains the dataframe of individual data. Each subsequent element contains a list of information about each explanatory variable added.
#'
#'@export

indiv_info_add<-function(indiv_info,ii_tag,indiv_data){

  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(is.data.frame(indiv_data)==FALSE){stop("the indiv_info dataframe is required [not the full object]")}

  indiv_excl<-which(indiv_info[,1]%in%indiv_data$indivs==FALSE)
  indiv_add<-which(indiv_data$indivs%in%indiv_info[,1]==FALSE)

  indiv_info2<-indiv_info[-indiv_excl,]

  n_obs<-length(indiv_add)
  n_indiv_info<-data.frame(indiv_data$indivs[indiv_add])
  names(n_indiv_info)<-"ID"

  for(i in 1:length(ii_tag)){

    obs<-rep(NA,n_obs)
    n_indiv_info$trait<-obs

    trait_type<-ii_tag[[i]][[2]]
    x_dist<-ii_tag[[i]][[3]]
    level_names<-ii_tag[[i]][[4]]
    prob_levels<-ii_tag[[i]][[5]]

    if(trait_type=="cov"){
      if(x_dist=="unif"){
        n_indiv_info$trait<-stats::runif(n_obs,0,1)
      }
      if(x_dist=="norm"){
        n_indiv_info$trait<-stats::rnorm(n_obs)
      }
      if(x_dist=="norm"){
        n_indiv_info$trait<-stats::rlnorm(n_obs)
      }
    }

    if(trait_type=="fac"){
      levels<-length(level_names)
      if(is.vector(prob_levels)){prob<-prob_levels}
      if(is.vector(prob_levels)==FALSE){prob<-NULL}
      n_indiv_info$trait<-sample(level_names,n_obs,replace=TRUE,prob=prob)
      n_indiv_info$trait<-as.factor(n_indiv_info$trait)
    }

    names(n_indiv_info)[which(names(n_indiv_info)=="trait")]<-ii_tag[[i]][[1]]

  }

  indiv_info2<-rbind(indiv_info2,n_indiv_info)

  output<-list(indiv_info2,ii_tag)

  return(output)

}
