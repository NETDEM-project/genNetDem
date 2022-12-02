test_that("covariates_survival works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))
  indiv_info<-readRDS(test_path("fixtures","indiv_info_1.RDS"))
  network<-readRDS(test_path("fixtures","pop_mat_1.RDS"))

  expect_error(genNetDem::covariates_survival(indiv_data=indiv_data,
                                 indiv_info=indiv_info,
                                 network=network,
                                 group_means=NULL,
                                 ext_vars=NULL,ext_effs,scale_ext=TRUE,
                                 net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                                 net_cov=FALSE,max_cor=0,covMat_check=FALSE,
                                 mps=1.1,lvps=1),
               "Survival probability must be between 0 and 1")

  expect_error(genNetDem::covariates_survival(indiv_data=indiv_data,
                                              indiv_info=indiv_info,
                                              network=network,
                                              group_means=NULL,
                                              ext_vars=NULL,ext_effs,scale_ext=TRUE,
                                              net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                                              net_cov=TRUE,max_cor=-1.1,covMat_check=FALSE,
                                              mps=0.9,lvps=1),
               "Correlation must be between -1 and 1")

  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                              indiv_info=indiv_info,
                                              network=network,
                                              group_means=NULL,
                                              ext_vars=NULL,ext_effs,scale_ext=TRUE,
                                              net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                                              net_cov=FALSE,max_cor=0,covMat_check=FALSE,
                                              mps=0.9,lvps=0.1)

  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))
  expect_equal(round(mean(t_res[,5]),1),0.9)

  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                        indiv_info=indiv_info,
                                        network=network,
                                        group_means=NULL,
                                        ext_vars=NULL,ext_effs,scale_ext=TRUE,
                                        net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                                        net_cov=TRUE,max_cor=1,covMat_check=FALSE,
                                        mps=0.5,lvps=0.05)

  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))
  expect_equal(round(mean(t_res[,5]),1),0.5)

  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                        indiv_info=indiv_info,
                                        network=network,
                                        group_means=NULL,
                                        ext_vars=NULL,ext_effs,scale_ext=TRUE,
                                        net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                                        net_cov=TRUE,max_cor=-1,covMat_check=FALSE,
                                        mps=0.5,lvps=0.05)

  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))
  expect_equal(round(mean(t_res[,5]),1),0.5)

  t_effs<-list()
  t_effs[[1]]<-c(0,1)
  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                        indiv_info=indiv_info,
                                        network=network,
                                        group_means=NULL,
                                        ext_vars="sex",ext_effs=t_effs,scale_ext=TRUE,
                                        net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                                        net_cov=FALSE,max_cor=0,covMat_check=FALSE,
                                        mps=0.8,lvps=0.2)

  t_sum<-aggregate(t_res[,5],by=list(indiv_info[[1]][,2]),mean)
  t_sum2<-t_sum[t_sum[,1]=="F",2]
  t_sum3<-t_sum[t_sum[,1]=="M",2]
  t_sum4<-t_sum3>t_sum2
  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))
  expect_equal(round(t_sum2,1),0.8)
  expect_equal(as.numeric(t_sum4),1)

  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                        indiv_info=indiv_info,
                                        network=network,
                                        group_means=NULL,
                                        ext_vars=NULL,ext_effs=t_effs,scale_ext=TRUE,
                                        net_vars="degree",net_effs=0.5,net_packages="igraph",scale_net=TRUE,
                                        net_cov=FALSE,max_cor=0,covMat_check=FALSE,
                                        mps=0.8,lvps=0.2)
  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))

  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                        indiv_info=indiv_info,
                                        network=network,
                                        group_means=NULL,
                                        ext_vars=NULL,ext_effs=t_effs,scale_ext=TRUE,
                                        net_vars="closeness",net_effs=0.5,net_packages="sna",scale_net=TRUE,
                                        net_cov=FALSE,max_cor=0,covMat_check=FALSE,
                                        mps=0.8,lvps=0.2)
  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))

  t_res<-genNetDem::covariates_survival(indiv_data=indiv_data,
                                        indiv_info=indiv_info,
                                        network=network,
                                        group_means=NULL,
                                        ext_vars=NULL,ext_effs=t_effs,scale_ext=TRUE,
                                        net_vars="betweenness_w",net_effs=0.5,net_packages="tnet",scale_net=TRUE,
                                        net_cov=FALSE,max_cor=0,covMat_check=FALSE,
                                        mps=0.8,lvps=0.2)
  expect_equal(dim(t_res),dim(indiv_data)+c(0,1))

})
