test_that("basic_survival works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))
  t_res<-genNetDem::basic_survival(indiv_data=indiv_data,mps=0.8,lvps=0)
  expect_equal(round(t_res[,5],1),rep(0.8,nrow(indiv_data)))
  t_res<-genNetDem::basic_survival(indiv_data=indiv_data,mps=0.7,lvps=4)
  expect_equal(sum(t_res[,5]<0)+sum(t_res[,5]>1),0)
  expect_error(genNetDem::basic_survival(indiv_data=indiv_data,mps=1.6,lvps=2),"Survival probability must be between 0 and 1")
  expect_error(genNetDem::basic_survival(indiv_data=indiv_data,mps=-0.2,lvps=2),"Survival probability must be between 0 and 1")
})
