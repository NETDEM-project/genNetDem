#'population_generation_basic
#'
#'Generates basic data for a population clustered in social groups distributed uniformly in 2D space.
#'
#'@param n the total number of individuals in the population
#'@param ng the number of social groups the individuals should be divided into
#'@param plot Whether to plot spatial locations of groups. Defaults to TRUE.
#'
#'@details Function for basic population generation.Integrates information on number of individuals, groups and group locations. When group sizes are greater than 1 individuals are sampled into groups randomly, so not all groups will necessarily be identical in size. Currently the spatial grid remains the same size regardless of population size.
#'
#'@return A list of two parts. A) A dataframe providing information on individual IDs, spatial coordinates and social group IDs. B) A distance matrix providing the spatial distances between all individuals in the population.
#'
#'@export

population_generation_basic<-function(n,ng,plot=TRUE){

  if(n<0){stop("number of individuals must be more than zero")}
  if(ng<0){stop("number of groups must be more than zero")}

  #Individual codes
  indivs<-seq(1,n,1)

  #Sort individuals into groups
  groups<-vector()
  if(n==ng){
    groups<-indivs
  }
  if(n<ng){
    stop("invalid number of groups")
  }
  if(n>ng){
    groups<-sample(seq(1,ng,1),n,replace=TRUE)
  }

  #Locations
  x1<-stats::runif(ng,0,1)
  y1<-stats::runif(ng,0,1)

  x<-numeric()
  y<-numeric()

  for(i in 1:n){
    x[i]<-x1[groups[i]]
    y[i]<-y1[groups[i]]
  }

  #Check locations
  if(plot==TRUE){
    plot(x,y,main="Spatial Locations",xaxt="n",yaxt="n",cex.lab=1.5,pch=16,col=grDevices::adjustcolor("black",0.4))
  }

  #Create dataframe for individuals
  indiv_data<-data.frame(indivs,groups,x,y)

  #Create distance matrix between locations
  dist_mat<-as.matrix(stats::dist(cbind(indiv_data$x,indiv_data$y),upper=TRUE))

  output<-list(indiv_data,dist_mat)

  return(output)

}

