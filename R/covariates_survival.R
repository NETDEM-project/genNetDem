#'covariates_survival
#'
#'Generates survival probabilities for all individuals in the population based on their individual traits and position in the population social network.
#'
#'@param indiv_data current indiv_data dataframe
#'@param indiv_info current indiv_info
#'@param network adjacency matrix for the network used to calculate survival probabilities
#'@param group_means social group (as defined in population generation) effects on survival probabilies. Provided on logit scale.
#'@param ext_vars vector extrinsic variables (non-network) used to calculate survival probability. Names of columns in indiv_info dataframe. Defaults to NULL.
#'@param ext_effs a list of effect sizes for each extrinsic variable provided. Single value for covariates. Value for each level for factors. Defaults to NULL
#'@param scale_ext whether to scale extrinsic variables. Defaults to TRUE.
#'@param net_vars vector of network measures that affect survival. Should be named as per the name of the function used in R (for a given package provided in net_packages=). Defaults to NULL
#'@param net_effs list of effect sizes used to calculate effect of network measures on survival
#'@param net_packages a vector of R packages of the same length of the net_vars providing the R package used to calculate each network measure. See examples
#'@param scale_net whether to scale network measures. Defaults to TRUE.
#'@param net_cov whether there should be network covariance in survival probability. Defaults to FALSE.
#'@param max_cor maximum correlation (positive or negative) for network covariance in survival probabilities
#'@param covMat_check whether to calculate a network regression to demonstrate that the adjusted covariance matrix (adjusted to be positive definite) is strongly correlated with the input matrix. Defaults to FALSE.
#'@param mps overall mean population survival probability
#'@param lvps variance in survival probability on logit scale
#'@details This function simulates survival probabilities for all individuals in the population based on the covariates provided. Note that there is a huge amount of flexibility in this function, especially the network measures part of it but that its impossible to anticipate all possible demands with some packages/functions requiring varied inputs. The main ones are already covered but any issues with this can be reported as bugs and the package can be updated. Note also that the covariance matrix must be positive definite. We use the R function nearPD from the Matrix package to find the closest matrix to the provided network that is positive definite. This approximation normally works well but there is capability included to check how well correlated it is with the input network.

#'@return The new indiv_data dataframe with newly calculated survival probabilities added
#'@export

covariates_survival<-function(indiv_data,indiv_info,network,
                              group_means=NULL,
                              ext_vars=NULL,ext_effs,scale_ext=TRUE,
                              net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                              net_cov=FALSE,max_cor=0.8,covMat_check=FALSE,
                              mps,lvps){

  if(mps<0|mps>1){stop("Survival probability must be between 0 and 1")}
  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(max_cor< -1|max_cor>1){stop("Correlation must be between -1 and 1")}
  if(is.matrix(network)==FALSE){stop("Network must be in adjacency matrix format")}
  if(dim(network)[1]!=dim(network)[2]){stop("Adjacency matrix not square")}
  if(isSymmetric(network)==FALSE){stop("Adjacency matrix must be symmetric")}
  if(is.list(indiv_info)==FALSE){stop("Full indiv_info object required")}

  lmps<-car::logit(mps,adjust=0.001)

  if(net_cov==FALSE){
    t_survival<-stats::rnorm(nrow(indiv_data),lmps,lvps)
  }
  if(net_cov==TRUE){
    networkB<-network/max(network)
    corMat<-max_cor*networkB
    diag(corMat)<-1
    stddev<-rep(lvps^0.5,nrow(indiv_data))
    covMat <- stddev %*% t(stddev) * corMat
    covMat2<-as.matrix(Matrix::nearPD(covMat)$mat)
    if(covMat_check==TRUE){
      print(summary(sna::netlm(y=covMat2,x=covMat)))
    }
    t_survival <- MASS::mvrnorm(n = 1, mu = rep(lmps,nrow(indiv_data)), Sigma = covMat2, empirical = FALSE)
  }

  if(is.vector(group_means)){
    t_survival<-t_survival+group_means[indiv_data$groups]
  }
  if(is.vector(ext_vars)){

    c_l<-length(ext_vars)

    for(i in 1:length(ext_vars)){

      t_i<-which(grepl(ext_vars[i],indiv_info[[2]]))

      if(indiv_info[[2]][[t_i]][[2]]=="cov"){
        if(scale_ext==FALSE){
          t_survival<-t_survival+ext_effs[[i]]*indiv_info[[1]][,t_i+1]
        }
        if(scale_ext==TRUE){
          t_survival<-t_survival+ext_effs[[i]]*scale(indiv_info[[1]][,t_i+1])
        }
      }

      if(indiv_info[[2]][[t_i]][[2]]=="fac"){
        for(j in 1:nrow(indiv_info[[1]])){
          t_survival[j]<-t_survival[j]+ext_effs[[i]][as.numeric(indiv_info[[1]][j,t_i+1])]
        }
      }
    }
  }

  if(is.vector(net_vars)){

    c_l<-length(net_vars)

    for(i in 1:length(net_vars)){

      if(is.na(net_packages[i])){
        met<-eval(parse(text=paste0(net_vars[i],"(network)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="igraph"){
        network2<-igraph::graph.adjacency(network,mode="undirected",weighted=TRUE)
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="sna"){
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="tnet"){
        network2<-tnet::as.tnet(network,type="weighted one-mode tnet")
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2)")))
        met<-met[,ncol(met)]
        if(length(met)<nrow(network)){
          met<-c(met,rep(0,(nrow(network)-length(met))))
        }
      }

      #Convert indivs with NA for clustering coefficient to have clustering coefficient of 0
      if(net_vars[i]%in%c("clustering_local_w","clustering_local","transitivity")){
        met[is.na(met)]<-0
      }

      if(scale_net==FALSE){
        t_survival<-t_survival+net_effs[[i]]*met
      }
      if(scale_net==TRUE){
        if(stats::sd(met,na.rm=TRUE)>0){
          t_survival<-t_survival+net_effs[[i]]*scale(met)
        }
        if(stats::sd(met,na.rm=TRUE)==0){
          t_survival<-t_survival+net_effs[[i]]*(met-mean(met))
        }
      }

    }
  }

  indiv_data$survival<-boot::inv.logit(t_survival)
  return(indiv_data)

}

##to do - community effect on survival?
