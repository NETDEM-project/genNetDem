test_that("network_generation_basic works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))
  dist_mat<-readRDS(test_path("fixtures","dist_mat_1.RDS"))

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                    p_ig=-0.1,w_ig1=1.5,w_ig2=5,
                                    p_og=0.2,w_og1=1,w_og2=5,
                                    d_effp=4,d_effw=4,
                                    plot=FALSE),
               "p_ig must be between zero and one")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=1.1,w_ig1=1.5,w_ig2=5,
                                                   p_og=0.2,w_og1=1,w_og2=5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "p_ig must be between zero and one")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                                   p_og=-0.1,w_og1=1,w_og2=5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "p_og must be between zero and one")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                                   p_og=1.1,w_og1=1,w_og2=5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "p_og must be between zero and one")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=-1.5,w_ig2=5,
                                                   p_og=0.2,w_og1=1,w_og2=5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "w_ig1 must be greater than zero")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=-5,
                                                   p_og=0.2,w_og1=1,w_og2=5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "w_ig2 must be greater than zero")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                                   p_og=0.2,w_og1=-1,w_og2=5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "w_og1 must be greater than zero")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                                   p_og=0.2,w_og1=1,w_og2=-5,
                                                   d_effp=4,d_effw=4,
                                                   plot=FALSE),
               "w_og2 must be greater than zero")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                                   p_og=0.2,w_og1=1,w_og2=5,
                                                   d_effp=-4,d_effw=4,
                                                   plot=FALSE),
               "d_effp must not be less than zero")

  expect_error(genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                                   p_og=0.2,w_og1=1,w_og2=5,
                                                   d_effp=4,d_effw=-4,
                                                   plot=FALSE),
               "d_effw must not be less than zero")

  t_res<-genNetDem::network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                             p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                             p_og=0.2,w_og1=1,w_og2=5,
                                             d_effp=4,d_effw=4,
                                             plot=FALSE)

  expect_equal(class(t_res[[1]]),c("matrix","array"))
  expect_equal(class(t_res[[2]]),"igraph")
  expect_equal(dim(t_res[[1]])[1],nrow(indiv_data))
  expect_equal(dim(t_res[[1]])[2],nrow(indiv_data))
  expect_equal(as.numeric(isSymmetric(t_res[[1]])),1)
  expect_equal(sum(t_res[[1]]<0|t_res[[1]]>1),0)

})
