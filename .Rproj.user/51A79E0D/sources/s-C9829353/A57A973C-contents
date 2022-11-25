#'network_rewire_covariates
#'
#'A function to add new individuals to and rewire a social network at each demographic timestep (or more frequently if desired).
#'
#'@param network the network to be rewired
#'@param indiv_data the current iteration (i.e. after survival/recruitment simulated) indiv_data dataframe providing information on social group ID, spatial location etc.
#'@param prev_data the previous iteration (i.e. before survival/recruitment simulated) indiv_data dataframe providing information on social group ID, spatial location etc.
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
#'@param p_wr_i probability of an existing individual have its existing social relationships in the network re-wired
#'@param p_wr_e probability that each edge is rewired for a selected individual.
#'@param plot Whether to plot the network generated. Defaults to TRUE.
#'
#'@details Function to add new individuals to and rewire a social network. Uses the same structure as network_generation_covariates. The same caveats apply to this function as the network_generation_covariates function.
#'
#'@return List of two parts: A) adjacency matrix and B) igraph network object
#'
#'@export

#Need to prevent the distance effect from going negative with the wrong values
#Same for the weight parameters of the Beta distribution

network_rewire_covariates<-function(network,
                                    indiv_data,prev_data,dist_mat,indiv_info,
                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                    d_effp=4,d_effw=4,
                                    covs=2,effs,
                                    p_wr_i=0.2,p_wr_e=0.5,
                                    plot=TRUE){

  if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
  if(is.data.frame(prev_data)==FALSE){stop("Correctly formatted prev_data is required")}
  if(ncol(prev_data)%in%c(4,5)==FALSE){stop("Correctly formatted prev_data is required")}
  if(is.matrix(dist_mat)==FALSE){stop("Distance matrix must be in matrix format")}
  if(dim(dist_mat)[1]!=dim(dist_mat)[2]){stop("Distance matrix not square")}
  if(p_ig<0|p_ig>1){stop("p_ig must be between zero and one")}
  if(wi_m<0|wi_m>1){stop("wi_m must bebetween zero and one")}
  if(wi_v<0){stop("wi_v must be greater than zero")}
  if(p_og<0|p_og>1){stop("p_og must be between zero and one")}
  if(wo_m<0|wo_m>1){stop("w_og1 must be  between zero and one")}
  if(wo_v<0){stop("wo_v must be greater than zero")}
  if(p_wr_i<0|p_wr_i>1){stop("p_wr_i must be between zero and one")}
  if(p_wr_e<0|p_wr_e>1){stop("p_wr_e must be between zero and one")}

  ################
  fn_sol <- function(x) {

    mean <- x[1]/(x[1]+x[2]) - beta_mean
    var <- (x[1]*x[2])/(((x[1]+x[2])^2)*(x[1]+x[2]+1)) - beta_var

    return(c(mean, var))

  }

  ################

  t_rem<-which(prev_data$indivs%in%indiv_data$indivs==FALSE)
  if(length(t_rem)>0){network2<-network[-t_rem,-t_rem]}
  if(length(t_rem)==0){network2<-network}

  n<-nrow(indiv_data)
  network3<-matrix(0,nrow=n,ncol=n)

  network3[1:nrow(network2),1:ncol(network2)]<-network2

  for(i in 1:nrow(network2)){
    c_f<-stats::rbinom(1,1,p_wr_i)
    if(c_f==1){
      for(j in 1:ncol(network2)){
        c_f2<-stats::rbinom(1,1,p_wr_e)
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
          network3[i,j]<-network3[j,i]<-t_e1*t_e2
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
          weights<-nleqslv::nleqslv(c(1,1), fn_sol)$x
          t_e2<-stats::rbeta(1,weights[1],weights[2])*(1/(1+vals[5]))^dist_mat[i,j]
          network3[i,j]<-network3[j,i]<-t_e1*t_e2
        }
      }
    }
  }

  if(nrow(network3)>nrow(network2)){

  for(i in (nrow(network2)+1):nrow(network3)){
    for(j in 1:nrow(network3)){
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
        network3[i,j]<-network3[j,i]<-t_e1*t_e2
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
        weights<-nleqslv::nleqslv(c(1,1), fn_sol)$x
        t_e2<-stats::rbeta(1,weights[1],weights[2])*(1/(1+vals[5]))^dist_mat[i,j]
        network3[i,j]<-network3[j,i]<-t_e1*t_e2
      }
    }
  }
  }

  diag(network3)<-0

  pop_mat<-network3
  pop_net<-igraph::graph.adjacency(pop_mat,mode="undirected",weighted=TRUE)

  if(plot==TRUE){
    graphics::par(mar=c(0,0,0,0))
    plot(pop_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(pop_net)$weight*10)^1.5)
    graphics::par(mar=c(5,5,2,2))
  }

  output<-list(pop_mat,pop_net)

  return(output)

}
