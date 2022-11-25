#'cap_and_obs2
#'
#'Generates observations that can be used to generate CMR data and observed social networks.
#'
#'@param samp_wind vector recording which groups correspond to which sampling windows/behavioural timesteps.
#'@param gbi the true group-by-individual matrix
#'@param pcg probability of observing or capturing a group in a sampling window with captures
#'@param pmg probability of observing or observing a group in a sampling window with observations
#'@param pmi the probability of an individual being observed in a sampled group
#'@param pci the probability of an individual being captured in a sampled group
#'@param obs_seq is an ordered vector of behavioural timesteps that observations take place in, e.g. c(2,4) would indicate observations in the second and fourth timesteps
#'@param cap_seq is an ordered vector of behavioural timesteps that captures take place in, e.g. c(2,4) would indicate captures in the second and fourth timesteps
#'@param pre_cap vector indicating whether any individuals have been previously captured (and so are observable). Defaults to NULL.
#'@details Note that observations do not happen in the same sampling windows as captures even if defined to do so. Only previously captured individuals are observable. See notes in comments for future updates. I would also like to allow some observation error in another version of this function.
#'@return A list with 4 elements: A) the full GBI but for only captured groups; B) a vector indicating which groups were captured ; C) the full GBI for observed and captured groups combined; D) a vector indicating which groups were observed
#'
#'@export

cap_and_obs2<-function(samp_wind,gbi=gbi,
                      pcg=0.5,pmg=0.5,pmi=0.9,pci=0.9,
                      obs_seq=1,
                      cap_seq=1,
                      pre_cap=NULL){

  if(pcg<0|pcg>1){stop("pcg is a probability, please set accordingly")}
  if(pmg<0|pmg>1){stop("pcg is a probability, please set accordingly")}
  if(pci<0|pci>1){stop("pmi is a probability, please set accordingly")}
  if(pmi<0|pmi>1){stop("pci is a probability, please set accordingly")}
  if(is.matrix(gbi)==FALSE){stop("gbi must be a matrix")}
  if(is.vector(samp_wind)==FALSE){stop("samp_wind must be a matrix")}

  n_samp_wind<-length(unique(samp_wind))
  samp_wind_size<-table(samp_wind)

  cap_winds<-cap_seq
  obs_winds<-obs_seq
  start_cap<-cap_seq[1]
  end_cap<-cap_seq[length(cap_seq)]
  start_obs<-obs_seq[1]
  end_obs<-obs_seq[length(obs_seq)]
  if(start_obs<1|start_obs>max(samp_wind)){stop("start_obs must lie within the sampling window")}
  if(end_obs<1|end_obs>max(samp_wind)){stop("end_obs must lie within the sampling window")}
  if(start_cap<1|start_cap>max(samp_wind)){stop("start_cap must lie within the sampling window")}
  if(end_cap<1|end_cap>max(samp_wind)){stop("end_cap must lie within the sampling window")}
  if(end_obs<start_obs){stop("end_obs must be after start_obs")}
  if(end_cap<start_cap){stop("end_cap must be after start_cap")}

  gbi_o<-NULL
  gbi_c<-NULL

  gbi2<-gbi

  for(sw in 1:n_samp_wind){
    if(sw%in%cap_winds){
      gbi_t<-gbi2[samp_wind==sw,]
      if(sw==1){
        st_row=0
      }
      if(sw>1){
        st_row<-max(which(samp_wind==(sw-1)))
      }
      t_grs<-seq(1,nrow(gbi_t),1)
      st_grs<-sample(c(0,1),length(t_grs),replace=TRUE,prob=c(1-pcg,pcg))
      st_grs2<-st_grs*t_grs
      st_grs2<-st_grs2[st_grs2>0]
      gbi_t<-gbi_t[st_grs2,]
      if(is.vector(gbi_t)){gbi_t<-t(as.matrix(gbi_t))}
      if(nrow(gbi_t)>0){
        gbi_mirr<-array(sample(c(0,1),length(gbi_t),replace=TRUE,prob=c(1-pmi,pmi)),dim=dim(gbi_t))
        gbi_t<-gbi_t*gbi_mirr
        if(is.matrix(gbi_c)==FALSE){
          gbi_c<-gbi_t
          samp_wind_c<-rep(sw,length(st_grs2))
          #print("a")
          #print(st_grs2)
          #print(st_row)
          #print("b")
          c_grs<-st_grs2+st_row
        } else if(is.matrix(gbi_c)){
          gbi_c<-rbind(gbi_c,gbi_t)
          samp_wind_c<-c(samp_wind_c,rep(sw,length(st_grs2)))
          #c_grs<-c(c_grs,st_grs2+st_row)
          #print("c")
          #print(st_grs2)
          #print(st_row)
          #print("d")
          if(!exists("c_grs")){c_grs<-st_grs2+st_row}
          if(exists("c_grs")){c_grs<-c(c_grs,st_grs2+st_row)}
        }
      }
      #print(st_grs2)
      #print(gbi_t)
      #print(c_grs)
      #print(gbi_c)
    }
    if(sw%in%cap_winds==FALSE&sw%in%obs_winds==TRUE){
      gbi_t<-gbi2[samp_wind==sw,]
      if(sw==1){
        st_row=0
      }
      if(sw>1){
        st_row<-max(which(samp_wind==(sw-1)))
      }
      t_grs<-seq(1,nrow(gbi_t),1)
      st_grs<-sample(c(0,1),length(t_grs),replace=TRUE,prob=c(1-pmg,pmg))
      st_grs2<-st_grs*t_grs
      st_grs2<-st_grs2[st_grs2>0]
      gbi_t<-gbi_t[st_grs2,]
      if(is.vector(gbi_t)){gbi_t<-t(as.matrix(gbi_t))}
      if(nrow(gbi_t>0)){
        gbi_mirr<-array(sample(c(0,1),length(gbi_t),replace=TRUE,prob=c(1-pci,pci)),dim=dim(gbi_t))
        gbi_t<-gbi_t*gbi_mirr
        if(is.matrix(gbi_o)==FALSE){
          gbi_o<-gbi_t
          samp_wind_o<-rep(sw,length(st_grs2))
          o_grs<-st_grs2+st_row
        } else if(is.matrix(gbi_o)){
          gbi_o<-rbind(gbi_o,gbi_t)
          samp_wind_o<-c(samp_wind_o,rep(sw,length(st_grs2)))
          #print(exists("o_grs"))
          if(!exists("o_grs")){o_grs<-st_grs2+st_row}
          if(exists("o_grs")){o_grs<-c(o_grs,st_grs2+st_row)}
        }
      }
    }
  }

  gbi_c2<-array(0,dim=dim(gbi2))
  if(exists("c_grs")){gbi_c2[c_grs,]<-gbi_c}
  gbi_c3<-sign(apply(gbi_c2,2,cumsum))
  if(is.vector(pre_cap)){
    #pre_cap2<-t(as.matrix(pre_cap))
    gbi_c3<-sign(sweep(gbi_c3, 2, pre_cap, "+"))
  }
  gbi_o3<-array(0,dim=dim(gbi2))
  if(exists("o_grs")){
    #print(o_grs)
    gbi_c4<-gbi_c3[o_grs,]
    gbi_o2<-gbi_o*gbi_c4
    gbi_o3[o_grs,]<-gbi_o2
  }
  gbi_o4<-gbi_o3+gbi_c2

  if(!exists("c_grs")){c_grs<-NULL}
  if(!exists("o_grs")){o_grs<-NULL}

  #May need to adjust output to save some of these alternative matrices
  output<-list(gbi_c2,c_grs,gbi_o4,o_grs)

  return(output)

}




