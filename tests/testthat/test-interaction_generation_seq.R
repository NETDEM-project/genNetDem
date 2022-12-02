test_that("interaction_generation_seq works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))
  pop_mat<-readRDS(test_path("fixtures","pop_mat_1.RDS"))

  expect_error(genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                             ne=100,mgs=2,style_gs="wrong",
                             pm=20,float=0.00001),
               "style_gs must be either 'rep' or 'pois'")

  expect_error(genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                                          ne=0,mgs=2,style_gs="rep",
                                          pm=20,float=0.00001),
               "number of events must be 1 or more")

  expect_error(genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                                          ne=100,mgs=-2,style_gs="rep",
                                          pm=20,float=0.00001),
               "mean group size must be greater than zero")

  expect_error(genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                                          ne=100,mgs=2,style_gs="rep",
                                          pm=0,float=0.00001),
               "pm must be greater than zero")

  expect_error(genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                                          ne=100,mgs=2,style_gs="rep",
                                          pm=20,float=-0.01),
               "float must be greater than or equal to zero")

  expect_warning(genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                                          ne=100,mgs=2,style_gs="rep",
                                          pm=20,float=0.1),
                 "Float value is high. Interactions unlikely to be strongly linked to underlying network")

  t_res<-genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                             ne=100,mgs=2,style_gs="rep",
                             pm=20,float=0.00001)

  expect_equal(dim(t_res)[1],100)
  expect_equal(dim(t_res)[2],nrow(indiv_data))
  expect_equal(unique(rowSums(t_res)),2)
  expect_equal(sort(unique(as.vector(t_res))),c(0,1))

  t_res<-genNetDem::interaction_generation_seq(pop_mat,indiv_data,
                                    ne=100,mgs=2,style_gs="pois",
                                    pm=20,float=0.00001)

  expect_equal(dim(t_res)[1],100)
  expect_equal(dim(t_res)[2],nrow(indiv_data))
  expect_equal(sort(unique(as.vector(t_res))),c(0,1))

})
