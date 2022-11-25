#'indiv_info_gen
#'
#'Generates trait data for individuals in the population. Adds one variable at a time sequentially.
#'
#'@param indiv_info existing indiv_info dataframe. Minimum input is a single-column dataframe of individual IDs
#'@param ii_tag information on variables already included in the dataframe if present. Defaults to NULL
#'@param indiv_data dataframe from population_generation_basic() containing data on individual IDs, spatial locations and social group IDs
#'@param trait name of trait to be added
#'@param trait_type either a covariate (“cov”) or factor (“fac”)
#'@param x_dist the distribution of a covariate. Either uniform (“unif”), normal (“norm”) or log-normal (“lnorm”).
#'@param level_names the names of factor levels for categorical variables. Defaults to NULL.
#'@param prob_levels the probability of being assigned to a particular category for categorical variables (provides some ability to control proportions of individuals in each category).
#'
#'@details Generation of trait data (e.g. sex, body size etc.) for individuals in the population. Each time the function is run it adds one variable at a time sequentially. to the existing object.
#'
#'@return A nested list. The first element of the list contains the dataframe of individual data. Each subsequent element contains a list of information about each explanatory variable added.
#'
#'@export

indiv_info_gen<-function(indiv_info,ii_tag=NULL,indiv_data,trait,trait_type=c("cov","fac"),x_dist=c("norm"),level_names=NULL,prob_levels=NULL){

  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(is.data.frame(indiv_info)==FALSE){stop("the indiv_info dataframe is required [not the full object]")}
  if(trait_type%in%c("cov","fac")==FALSE){stop("trait_type must be either 'cov' or 'fac'")}
  if(trait_type=="cov"&x_dist%in%c("unif","norm","lnorm")==FALSE){stop("trait_type must be either 'unif' or 'norm' or 'lnorm'")}

  names(indiv_info)[1]<-"ID"
  n_obs<-nrow(indiv_info)
  obs<-rep(NA,n_obs)
  indiv_info$trait<-obs

  if(trait_type=="cov"){
    if(x_dist=="unif"){
      indiv_info$trait<-stats::runif(n_obs,0,1)
    }
    if(x_dist=="norm"){
      indiv_info$trait<-stats::rnorm(n_obs)
    }
    if(x_dist=="lnorm"){
      indiv_info$trait<-stats::rlnorm(n_obs)
    }
  }

  if(trait_type=="fac"){
    levels<-length(level_names)
    if(is.vector(prob_levels)){prob<-prob_levels}
    if(is.vector(prob_levels)==FALSE){prob<-NULL}
    indiv_info$trait<-sample(level_names,n_obs,replace=TRUE,prob=prob)
    indiv_info$trait<-as.factor(indiv_info$trait)
  }

  if(is.list(ii_tag)==FALSE){ii_tag=list()}

  ii_tag[[which(names(indiv_info)=="trait")-1]]<-list()

  ii_tag[[which(names(indiv_info)=="trait")-1]][[1]]<-trait
  ii_tag[[which(names(indiv_info)=="trait")-1]][[2]]<-trait_type
  ii_tag[[which(names(indiv_info)=="trait")-1]][[3]]<-x_dist
  ii_tag[[which(names(indiv_info)=="trait")-1]][[4]]<-level_names
  ii_tag[[which(names(indiv_info)=="trait")-1]][[5]]<-prob_levels
  ii_tag[[which(names(indiv_info)=="trait")-1]][[6]]<-paste("Variable")

  names(indiv_info)[which(names(indiv_info)=="trait")]<-paste(trait)

  output<-list(indiv_info,ii_tag)

  return(output)

}
