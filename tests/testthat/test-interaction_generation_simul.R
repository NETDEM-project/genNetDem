test_that("interaction_generation_simul works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))
  pop_mat<-readRDS(test_path("fixtures","pop_mat_1.RDS"))

  expect_error(genNetDem::interaction_generation_simul(pop_mat=pop_mat,indiv_data=indiv_data,
                                                       mean_group_size=2,n_ts=0,
                                                       pm=50,float=0.00001,pow=4),
               "number of timesteps must be 1 or more")

  expect_error(genNetDem::interaction_generation_simul(pop_mat=pop_mat,indiv_data=indiv_data,
                                                       mean_group_size=0,n_ts=10,
                                                       pm=50,float=0.00001,pow=4),
               "mean group size must be greater than zero")

  expect_error(genNetDem::interaction_generation_simul(pop_mat=pop_mat,indiv_data=indiv_data,
                                                       mean_group_size=2,n_ts=10,
                                                       pm=0,float=0.00001,pow=4),
               "pm must be greater than zero")

  expect_error(genNetDem::interaction_generation_simul(pop_mat=pop_mat,indiv_data=indiv_data,
                                                       mean_group_size=2,n_ts=10,
                                                       pm=50,float=-0.00001,pow=4),
               "float must be greater than or equal to zero")

  expect_warning(genNetDem::interaction_generation_simul(pop_mat=pop_mat,indiv_data=indiv_data,
                                                         mean_group_size=2,n_ts=10,
                                                         pm=50,float=0.1,pow=4),
                 "Float value is high. Interactions unlikely to be strongly linked to underlying network")

  t_res<-genNetDem::interaction_generation_simul(pop_mat=pop_mat,indiv_data=indiv_data,
                                                 mean_group_size=2,n_ts=10,
                                                 pm=50,float=0.00001,pow=4)

  expect_equal(dim(t_res[[1]])[2],nrow(indiv_data))
  expect_equal(sum(unique(rowSums(t_res[[1]]))<=0),0)
  expect_equal(sort(unique(as.vector(t_res[[1]]))),c(0,1))
  expect_equal(sort(unique(t_res[[2]])),seq(1,10,1))
  expect_equal(nrow(t_res[[1]]),length(t_res[[2]]))

})
