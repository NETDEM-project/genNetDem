test_that("multiplication works", {
  prev_data<-readRDS(test_path("fixtures","indiv_data_1a.RDS"))
  indiv_data<-readRDS(test_path("fixtures","indiv_data_2.RDS"))
  dist_mat<-readRDS(test_path("fixtures","dist_mat_2.RDS"))
  network<-readRDS(test_path("fixtures","pop_mat_1.RDS"))
  indiv_info<-readRDS(test_path("fixtures","indiv_info_2.RDS"))

  effs<-list()
  effs[[1]]<-matrix(c(0,0,0,1,1,1,0,0),nr=1,nc=8)
  effs[[1]]<-rbind(rep(0,8),effs[[1]])

  t_res<-genNetDem::network_rewire_covariates(network=network,
                            indiv_data=indiv_data,prev_data=prev_data,
                            dist_mat=dist_mat,indiv_info=indiv_info,
                            p_ig=0.2,wi_m=0.25,wi_v=0.025,
                            p_og=0.2,wo_m=0.25,wo_v=0.025,
                            d_effp=4,d_effw=4,
                            covs=2,effs,
                            p_wr_i=0.2,p_wr_e=0.5,
                            plot=FALSE)

  expect_equal(class(t_res[[1]]),c("matrix","array"))
  expect_equal(class(t_res[[2]]),"igraph")
  expect_equal(dim(t_res[[1]])[1],nrow(indiv_data))
  expect_equal(dim(t_res[[1]])[2],nrow(indiv_data))
  expect_equal(as.numeric(isSymmetric(t_res[[1]])),1)
  expect_equal(sum(t_res[[1]]<0|t_res[[1]]>1),0)

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=1.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "p_ig must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=1.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "p_og must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=1.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "wi_m must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=1.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "wo_m must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=-0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "wi_v must be greater than zero")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.26,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "wi_v must be less than or equal to 0.25")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=-0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "wo_v must be greater than zero")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.26,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "wo_v must be less than or equal to 0.25")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=-4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "d_effp must not be less than zero")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=-4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "d_effw must not be less than zero")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=-0.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "p_wr_i must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=1.2,p_wr_e=0.5,
                                                    plot=FALSE),
               "p_wr_i must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=-0.5,
                                                    plot=FALSE),
               "p_wr_e must be between zero and one")

  expect_error(genNetDem::network_rewire_covariates(network=network,
                                                    indiv_data=indiv_data,prev_data=prev_data,
                                                    dist_mat=dist_mat,indiv_info=indiv_info,
                                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                                    d_effp=4,d_effw=4,
                                                    covs=2,effs,
                                                    p_wr_i=0.2,p_wr_e=1.5,
                                                    plot=FALSE),
               "p_wr_e must be between zero and one")


})
