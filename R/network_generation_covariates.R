#'network_generation_covariates
#'
#'Generates underlying social network structure (weighted edges) based on spatial locations, group membership and individual traits.
#'
#'@param indiv_data the indiv_data dataframe providing information on social group ID, spatial location etc.
#'@param dist_mat the distance matrix between all individuals in the population
#'@param indiv_info the indiv_info dataframe containing information on individual traits
#'@param p_ig probability of a within-group edge
#'@param p_og probability of a between-group edge
#'@param wi_m mean within-group edge weight to generate a beta distribution of potential values
#'@param wi_v variance in within-group edge weight to generate a beta distribution of potential values
#'@param wo_m mean between-group edge weight to generate a beta distribution of potential values
#'@param wo_v variance in within-group edge weight to generate a beta distribution of potential values
#'@param d_effp distance effect of the probability of between-group edges. Implemented as (1/d_eff)^distance.
#'@param d_effw distance effect of the probability of between-group edges. Implemented as (1/d_eff)^distance.
#'@param covs the columns of indiv_info used as covariates when generating the network
#'@param effs list of effect sizes (each element as vector for covariate or matrix for factor) containing effect of each trait (given by covs=) on each of the 6 parameters (p_ig, wi_m, wi_v, p_og, wo_m, wo_v, d_effp, d_effw)
#'@param plot Whether to plot the network generated. Defaults to TRUE.
#'
#'@details More complex social network generation based on individual characteristics. Probabilities of edges within and between groups (binomial). Weights of edges within and between groups (Beta, 2 parameter). Edge probabilities and weights depend on distance between groups. Additions from network_generation_basic() are that you can use covariates from indiv_info to generate differences in network behaviour. Note that this function is parameterised using the mean and variance of the beta distribution so is more intuitive but perhaps a little less flexible. There is currently no functionality for interactions between individual traits but this could be added.There is currently no functionality for more complex network effects (e.g. transitivity, homophily etc.) but this is planned.
#'
#'@return List of two parts: A) adjacency matrix and B) igraph network object
#'
#'@export

network_generation_covariates<-function(indiv_data,dist_mat,indiv_info,
                                   p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                   p_og=0.2,wo_m=0.25,wo_v=0.025,
                                   d_effp=4,d_effw=4,
                                   covs=2,effs,
                                   plot=TRUE){

  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(is.matrix(dist_mat)==FALSE){stop("Distance matrix must be in matrix format")}
  if(dim(dist_mat)[1]!=dim(dist_mat)[2]){stop("Distance matrix not square")}
  if(p_ig<0|p_ig>1){stop("p_ig must be between zero and one")}
  if(wi_m<0|wi_m>1){stop("wi_m must bebetween zero and one")}
  if(wi_v<0){stop("wi_v must be greater than zero")}
  if(wi_v>0.25){stop("wi_v must be less or equal to than 0.25")}
  if(p_og<0|p_og>1){stop("p_og must be between zero and one")}
  if(wo_m<0|wo_m>1){stop("w_og1 must be  between zero and one")}
  if(wo_v<0){stop("wo_v must be greater than zero")}
  if(wo_v>0.25){stop("wo_v must be less than or equal to 0.25")}

  ################
  fn_sol <- function(x) {

    mean <- x[1]/(x[1]+x[2]) - beta_mean
    var <- (x[1]*x[2])/(((x[1]+x[2])^2)*(x[1]+x[2]+1)) - beta_var

    return(c(mean, var))

  }

  ################

  n<-nrow(indiv_data)

  #Generate underlying network of affiliative relationships
  pop_mat<-matrix(0,nrow=n,ncol=n)

  #to add: assortativity, transitivity

  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if(indiv_data$groups[i]==indiv_data$groups[j]){

        lprobs<-boot::logit(c(p_ig,wi_m,wi_v))

        c_l<-length(covs)

        for(cl in 1:c_l){
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="cov"){
            leffs<-effs[[covs[cl]-1]][1,1:3]
            lprobs<-lprobs+as.vector((indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]]*leffs+indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]]*leffs)/2)
          }
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="fac"){
            leffs<-effs[[covs[cl]-1]][,1:3]
            tr1<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]])
            tr2<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]])
            lprobs<-lprobs+as.vector((leffs[tr1,]+leffs[tr2,])/2)
          }
        }
        probs<-boot::inv.logit(lprobs)

        t_e1<-stats::rbinom(1,1,probs[1])
        beta_mean<-probs[2]
        beta_var<-probs[3]
        weights<-nleqslv::nleqslv(c(1,1), fn_sol)$x
        t_e2<-stats::rbeta(1,weights[1],weights[2])
        pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
      }

      if(indiv_data$groups[i]!=indiv_data$groups[j]){

        lvals<-c(boot::logit(c(p_og,wo_m,wo_v)),log(c(d_effp,d_effw)))

        c_l<-length(covs)

        for(cl in 1:c_l){
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="cov"){
            leffs<-effs[[covs[cl]-1]][1,4:8]
            lvals<-lvals+as.vector((indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]]*leffs+indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]]*leffs)/2)
          }
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="fac"){
            leffs<-effs[[covs[cl]-1]][,4:8]
            tr1<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]])
            tr2<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]])
            lvals<-lvals+as.vector((leffs[tr1,]+leffs[tr2,])/2)
          }
        }
        vals<-c(boot::inv.logit(lvals[1:3]),exp(lvals[4:5]))

        t_e1<-stats::rbinom(1,1,vals[1]*(1/(1+vals[4]))^dist_mat[i,j])
        beta_mean<-vals[2]
        beta_var<-vals[3]
        weights<-nleqslv::nleqslv(c(1,1),fn_sol)$x
        t_e2<-stats::rbeta(1,weights[1],weights[2])*(1/(1+vals[5]))^dist_mat[i,j]
        pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
      }
    }
  }

  if(sum(is.na(pop_mat))>0){{stop("NAs in adjacency matrix suggests difficulty fitting Beta distribution. Try reducing edge weight variance")}}

  pop_net<-igraph::graph.adjacency(pop_mat,mode="undirected",weighted=TRUE)

  if(plot==TRUE){
    graphics::par(mar=c(0,0,0,0))
    plot(pop_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(pop_net)$weight*10)^1.5)
    graphics::par(mar=c(5,5,2,2))
  }

  output<-list(pop_mat,pop_net)

  return(output)

}

#################################
#################################


