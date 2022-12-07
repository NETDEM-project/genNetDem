#'cap_dat_gen
#'
#'Function to generate typical CMR datasets from the output of cap_and_obs.
#'
#'@param CG a list of the observed GBIs (element 3 from the output of cap_and_obs) for all demographic timesteps.
#'@param SW_store a list of all sampling windows (element 2 from the output of interaction_generation_simul)
#'@param full_indiv_data the full_indiv_data dataframe containing information on the ID, group and spatial location of all individuals recorded in the population
#'@param inds_alive a list of individuals recorded alive at each demographic timestep.
#'@param bs number of behavioural timesteps per demographic timestep.
#'
#'@details used to create typical CMR capture history datasets from the output of cap_and_obs for use in hidden Markov modelling.
#'
#'@return  A list of 2 elements: A) Capture history summarised by demographic timesteps; B) Capture history for all behavioural timesteps
#'
#'@export


cap_dat_gen<-function(CG,SW_store,full_indiv_data,inds_alive,bs){
  for(i in 1:length(CG)){

    if(i==1){
      cap_dat_full<-data.frame(full_indiv_data$indivs)
      cap_dat_full2<-data.frame(full_indiv_data$indivs)
      names(cap_dat_full)<-names(cap_dat_full2)<-"ID"
    }

    cap_dat<-data.frame(CG[[i]],SW_store[[i]])
    cap_dat2<-stats::aggregate(cap_dat,by=list(cap_dat[,ncol(cap_dat)]),sum)
    cap_dat2<-cap_dat2[,2:(ncol(cap_dat2)-1)]
    cap_dat_sum<-sign(colSums(cap_dat2))

    cap_dat3<-data.frame(inds_alive[[i]],t(cap_dat2))
    names(cap_dat3)<-c("ID",bs*(i-1)+seq(1,bs,1))

    cap_dat4<-data.frame(inds_alive[[i]],cap_dat_sum)
    names(cap_dat4)<-c("ID","cap")

    cap_dat_full<-merge(cap_dat_full,cap_dat4,by="ID",all.x=TRUE)
    cap_dat_full2<-merge(cap_dat_full2,cap_dat3,by="ID",all.x=TRUE)

    names(cap_dat_full)[i+1]<-i

  }

  cap_dat_full[is.na(cap_dat_full)]<-0
  cap_dat_full2[is.na(cap_dat_full2)]<-0


  output<-list(cap_dat_full,cap_dat_full2)

  return(output)

}

