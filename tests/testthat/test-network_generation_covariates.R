test_that("network_generation_covariates works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))
  dist_mat<-readRDS(test_path("fixtures","dist_mat_1.RDS"))
  indiv_info<-readRDS(test_path("fixtures","indiv_info_1.RDS"))

  effs<-list()
  effs[[1]]<-matrix(0,nr=2,nc=8)
  effs[[1]][2,]<-c(0.2,0.2,0,0.2,0.2,0,1,1)

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=-0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "p_ig must be between zero and one")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=-0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "p_og must be between zero and one")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=1.25,wi_v=0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "wi_m must be between zero and one")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=0.2,wo_m=1.25,wo_v=0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "wo_m must be between zero and one")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=-0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "wi_v must be greater than zero")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.26,
                                             p_og=0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "wi_v must be less or equal to than 0.25")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=-0.025,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "wo_v must be greater than zero")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=0.26,
                                             d_effp=4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "wo_v must be less than or equal to 0.25")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=-4,d_effw=4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "d_effp must not be less than zero")

  expect_error(genNetDem::network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                             p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                             p_og=0.2,wo_m=0.25,wo_v=0.025,
                                             d_effp=4,d_effw=-4,
                                             covs=2,effs=effs,
                                             plot=FALSE),
               "d_effw must not be less than zero")

  t_res<-network_generation_covariates(indiv_data=indiv_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                p_og=0.2,wo_m=0.25,wo_v=0.025,
                                d_effp=4,d_effw=4,
                                covs=2,effs=effs,
                                plot=FALSE)
  expect_equal(class(t_res[[1]]),c("matrix","array"))
  expect_equal(class(t_res[[2]]),"igraph")
  expect_equal(dim(t_res[[1]])[1],nrow(indiv_data))
  expect_equal(dim(t_res[[1]])[2],nrow(indiv_data))
  expect_equal(as.numeric(isSymmetric(t_res[[1]])),1)
  expect_equal(sum(t_res[[1]]<0|t_res[[1]]>1),0)

})
