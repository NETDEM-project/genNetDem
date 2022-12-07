#'network_generation_basic
#'
#'Generates underlying social network structure (weighted edges) based on spatial locations and group membership.
#'
#'@param indiv_data the indiv_data dataframe providing information on social group ID, spatial location etc.
#'@param dist_mat the distance matrix between all individuals in the population
#'@param p_ig probability of a within-group edge
#'@param w_ig1 first parameter of the beta distribution controlling within-group edge weights
#'@param w_ig2 second parameter of the beta distribution controlling within-group edge weights
#'@param p_og (intercept) probability of a between-group edge
#'@param w_og1 first parameter of the beta distribution controlling between-group edge weights (intercept)
#'@param w_og2 second parameter of the beta distribution controlling between-group edge weights (intercept)
#'@param d_effp distance effect of the probability of between-group edges. Implemented as (1/d_eff)^distance.
#'@param d_effw distance effect of the probability of between-group edges. Implemented as (1/d_eff)^distance.
#'@param plot Whether to plot the network generated. Defaults to TRUE.
#'
#'@details Probabilities of edges within and between groups (binomial). Weights of edges within and between groups (Beta, 2 parameter). Edge probabilities and weights depend on distance between groups. Note that for this function (as opposed to network_generation_covariates) you parameterise the beta distribution for edge weights directly.
#'
#'@return List of two parts: A) adjacency matrix and B) igraph network object
#'
#'@export

network_generation_basic<-function(indiv_data,dist_mat,
                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                   p_og=0.2,w_og1=1,w_og2=5,
                                   d_effp=4,d_effw=4,
                                   plot=TRUE){

if(is.data.frame(indiv_data)==FALSE){stop("Correctly formatted indiv_data is required")}
if(ncol(indiv_data)%in%c(4,5)==FALSE){stop("Correctly formatted indiv_data is required")}
if(is.matrix(dist_mat)==FALSE){stop("Distance matrix must be in matrix format")}
if(dim(dist_mat)[1]!=dim(dist_mat)[2]){stop("Distance matrix not square")}
if(p_ig<0|p_ig>1){stop("p_ig must be between zero and one")}
if(w_ig1<0){stop("w_ig1 must be greater than zero")}
if(w_ig2<0){stop("w_ig2 must be greater than zero")}
if(p_og<0|p_og>1){stop("p_og must be between zero and one")}
if(w_og1<0){stop("w_og1 must be greater than zero")}
if(w_og2<0){stop("w_og2 must be greater than zero")}
if(d_effp<0){stop("d_effp must not be less than zero")}
if(d_effw<0){stop("d_effw must not be less than zero")}

n<-nrow(indiv_data)

#Generate underlying network of affiliative relationships
pop_mat<-matrix(0,nrow=n,ncol=n)

#to add: assortativity, transitivity, affects of individual traits

for(i in 1:(n-1)){
  for(j in (i+1):n){
    if(indiv_data$groups[i]==indiv_data$groups[j]){
      t_e1<-stats::rbinom(1,1,p_ig)
      t_e2<-stats::rbeta(1,w_ig1,w_ig2)
      pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
    }
    if(indiv_data$groups[i]!=indiv_data$groups[j]){
      t_e1<-stats::rbinom(1,1,p_og*(1/(1+d_effp))^dist_mat[i,j])
      t_e2<-stats::rbeta(1,w_og1,w_og2)*(1/(1+d_effw))^dist_mat[i,j]
      pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
    }
  }
}

pop_net<-igraph::graph.adjacency(pop_mat,mode="undirected",weighted=TRUE)

if(plot==TRUE){
  graphics::par(mar=c(0,0,0,0))
  plot(pop_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(pop_net)$weight*10)^1.5)
}

output<-list(pop_mat,pop_net)

return(output)

}

