#'obs_net_checker
#'
#'Generates observations that can be used to generate CMR data and observed social networks.
#'
#'@param gbi_o observed group-by-individual matrix
#'@param full_mat adjacency matrix for the social network generated from grouping events
#'@param pop_mat adjacency matrix for the underlying network
#'@param net_mets vector providing additional individual-level network measures that a user wishes to calculate. Defaults to NULL.
#'@param net_packages vector providing the R packages required to calculate the measures defined in net_mets. Defaults to NULL.
#'
#'@details A series of checks of how well the observed network mtaches the actual and underlying networks
#'@return By default the function returns four things: 1) A plot of the observed network; 2) Correlation with the actual network; 3) Correlation with the underlying network; 4) Correlations of key centrality measures between the observed and actual network. With additional arguments the function will also return plots and correlations (Spearman's rank) for extra network measures.
#'
#'@export

obs_net_checker<-function(gbi_o,full_mat,pop_mat,net_mets=NULL,net_packages=NULL){

  if(is.matrix(gbi_o)==FALSE){stop("gbi must be a matrix")}
  if(is.matrix(full_mat)==FALSE){stop("full_mat must be in adjacency matrix format")}
  if(dim(full_mat)[1]!=dim(full_mat)[2]){stop("full_mat matrix not square")}
  if(is.matrix(pop_mat)==FALSE){stop("pop_mat must be in adjacency matrix format")}
  if(dim(pop_mat)[1]!=dim(pop_mat)[2]){stop("pop_mat matrix not square")}

  gbi_o<-gbi_o[rowSums(gbi_o)>0,]

  obs_mat<-asnipe::get_network(gbi_o)
  obs_net<-igraph::graph.adjacency(obs_mat,mode="undirected",weighted=TRUE)

  #Plot network
  graphics::par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(obs_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(obs_net)$weight*2)^1.5)

  #Check correlation with actual network
  cor_check<-sna::netlm(obs_mat,full_mat,nullhyp="qapspp")
  print(summary(cor_check))

  #Check correlation with underlying network
  cor_check2<-sna::netlm(obs_mat,pop_mat,nullhyp="qapspp")
  print(summary(cor_check2))

  full_net<-igraph::graph.adjacency(full_mat,mode="undirected",weighted = TRUE)

  #Plot centrality correlations with actual network
  graphics::par(mfrow=c(2,2))
  graphics::par(mar=c(5,5,2,2))
  plot(igraph::degree(obs_net)~igraph::degree(full_net),ylab="Degree in observed network",xlab="Degree in true network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
  plot(igraph::strength(obs_net)~igraph::strength(full_net),ylab="Strength in observed network",xlab="Strength in true network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
  plot(igraph::betweenness(obs_net,weights=1/igraph::E(obs_net)$weight)~igraph::betweenness(full_net,weights=1/igraph::E(full_net)$weight),ylab="Betweenness in observed network",xlab="Betweenness in true network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))
  plot(igraph::closeness(obs_net,weights=1/igraph::E(obs_net)$weight)~igraph::closeness(full_net,weights=1/igraph::E(full_net)$weight),ylab="Closeness in observed network",xlab="Closeness in true network",las=1,pch=16,col=grDevices::adjustcolor("black",0.4))

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
