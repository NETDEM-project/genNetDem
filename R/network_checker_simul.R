#'network_checker_simul
#'
#'Used to check how well simultaneous interaction data reflect underlying network
#'
#'@param gbi group-by-individual matrix generated from the interaction generation function
#'@param pop_net underlying network used to generate GBI
#'@param net_mets vector providing additional individual-level network measures that a user wishes to calculate. Defaults to NULL.
#'@param net_packages vector providing the R packages required to calculate the measures defined in net_mets. Defaults to NULL.
#'
#'@details This function can be used to test various properties of the social network generated from the interaction dataset to see how closely it is related to the original underlying network.
#'@return By default the function returns three things: 1) a plot of the network generated from interaction data; 2) an estimate of the correlation between the two networks; 3) plots of metric correlations between the two networks. With additional arguments the function will also return plots and correlations (Spearman's rank) for extra network measures.
#'
#'@export

network_checker_simul<-function(gbi,pop_net,net_mets=NULL,net_packages=NULL){

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

  if(is.null(net_mets)==FALSE){

    if(length(net_mets)!=length(net_packages)){stop("net_mets and net_packages must be vectors of the same length")}

    for(i in 1:length(net_mets)){

      mat1<-pop_mat
      mat2<-full_mat

      if(net_packages[i]=="igraph"){
        network1<-igraph::graph.adjacency(mat1,mode="undirected",weighted=TRUE)
        network2<-igraph::graph.adjacency(mat2,mode="undirected",weighted=TRUE)
        if(net_mets[i]%in%c("betweenness","closeness")){
          met1<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(network1,weights=(1/E(network2)$weight))")))
          met2<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(network2,weights=(1/E(network2)$weight))")))
        }
        if(net_mets[i]%in%c("betweenness","closeness")==FALSE){
          met1<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(network1)")))
          met2<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(network2)")))
        }
      }
      if(net_packages[i]=="sna"){
        met1<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(mat1)")))
        met2<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(mat2)")))
      }
      if(net_packages[i]=="tnet"){
        network1<-tnet::as.tnet(mat1,type="weighted one-mode tnet")
        network2<-tnet::as.tnet(mat2,type="weighted one-mode tnet")
        met1<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(network1)")))
        met1<-met1[,ncol(met1)]
        if(length(met1)<nrow(mat1)){
          met1<-c(met1,rep(0,(nrow(mat1)-length(met1))))
        }
        met2<-eval(parse(text=paste0(net_packages[i],"::",net_mets[i],"(network2)")))
        met2<-met2[,ncol(met2)]
        if(length(met2)<nrow(mat2)){
          met2<-c(met2,rep(0,(nrow(mat2)-length(met2))))
        }
      }

      plot(met2~met1,ylab=paste0(net_mets[i]," in association network"),xlab=paste0(net_mets[i]," in underlying network"),las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
      plot(met2~met1,ylab=paste0(net_mets[i]," in association network"),xlab=paste0(net_mets[i]," in underlying network"),las=1,pch=16,col=grDevices::adjustcolor("black",0.4))

      stats::cor.test(met1,met2,method="spearman")$statistic

    }

  }

}
