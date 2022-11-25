#'network_checker_simul
#'
#'Used to check how well simultaneous interaction data reflect underlying network
#'
#'@param gbi group-by-individual matrix generated from the interaction generation function
#'@param pop_net underlying network used to generate GBI
#'
#'@details This function can be used to test various properties of the social network generated from the interaction dataset to see how closely it is related to the original underlying network.
#'@return 1) a plot of the network generated from interaction data; 2) an estimate of the correlation between the two networks; 3) plots of metric correlations between the two networks
#'
#'@export

network_checker_simul<-function(gbi,pop_net){

  if(is.matrix(gbi)==FALSE){stop("gbi must be a matrix")}
  if(class(pop_net)!="igraph"){stop("pop_net must be an igraph object")}

  gbi2<-gbi
  full_mat<-asnipe::get_network(gbi2)
  full_net<-igraph::graph.adjacency(full_mat,mode="undirected",weighted=TRUE)

  #Plot network
  graphics::par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(full_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(full_net)$weight*2)^1.5)

  pop_mat<-igraph::as_adjacency_matrix(pop_net,sparse=FALSE,attr="weight")

  #Check correlation between networks
  cor_check<-sna::netlm(full_mat,pop_mat,nullhyp="qapspp")
  print(summary(cor_check))

  #Plot correlation between centrality measures from underlying network
  #and that derived from groups/interactions
  graphics::par(mfrow=c(2,2))
  graphics::par(mar=c(5,5,2,2))
  plot(igraph::degree(full_net)~igraph::degree(pop_net),ylab="Degree in association network",xlab="Degree in underlying network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
  plot(igraph::strength(full_net)~igraph::strength(pop_net),ylab="Strength in association network",xlab="Strength in underlying network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
  plot(igraph::betweenness(full_net,weights=1/igraph::E(full_net)$weight)~igraph::betweenness(pop_net,weights=1/igraph::E(pop_net)$weight),ylab="Betweenness in association network",xlab="Betweenness in underlying network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
  plot(igraph::closeness(full_net,weights=1/igraph::E(full_net)$weight)~igraph::closeness(pop_net,weights=1/igraph::E(pop_net)$weight),ylab="Closeness in association network",xlab="Closeness in underlying network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))

}
