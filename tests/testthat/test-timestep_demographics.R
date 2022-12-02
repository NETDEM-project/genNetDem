test_that("timestep_demographics works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_2.RDS"))
  full_indiv_data<-readRDS(test_path("fixtures","full_indiv_data.RDS"))

  t_res<-genNetDem::timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,
                        recruitment=TRUE,
                        mps=0.8,lvps=0.5)

  expect_equal(length(t_res),3)
  expect_equal(nrow(t_res[[1]]),dim(t_res[[3]])[1])
  expect_equal(nrow(t_res[[1]]),dim(t_res[[3]])[2])
  expect_equal(as.numeric(isSymmetric(t_res[[3]])),1)
  expect_equal(sum(t_res[[3]]<0),0)

  expect_error(genNetDem::timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,
                                     recruitment=TRUE,
                                     mps=-0.8,lvps=0.5),
               "Survival probability must be between 0 and 1")

  expect_error(genNetDem::timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,
                                     recruitment=TRUE,
                                     mps=1.8,lvps=0.5),
               "Survival probability must be between 0 and 1")

})
