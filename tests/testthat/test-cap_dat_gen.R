test_that("cap_dat_gen works", {
  CG<-readRDS(test_path("fixtures","CG_1.RDS"))
  SW_store<-readRDS(test_path("fixtures","SW_store_1.RDS"))
  inds_alive<-readRDS(test_path("fixtures","inds_alive_1.RDS"))
  full_indiv_data<-readRDS(test_path("fixtures","full_indiv_data_2.RDS"))

  t_res<-genNetDem::cap_dat_gen(CG=CG,SW_store=SW_store,inds_alive=inds_alive,full_indiv_data=full_indiv_data,bs=20) #bs=20 must stay fixed

  expect_equal(ncol(t_res[[1]]),length(CG)+1)
  expect_equal(ncol(t_res[[2]]),length(CG)*20+1)
  expect_equal(sort(unique(unlist(t_res[[1]][,-1]))),c(0,1))
  expect_equal(sort(unique(unlist(t_res[[2]][,-1]))),c(0,1))
  expect_equal(t_res[[1]][,1],full_indiv_data[,1])
  expect_equal(t_res[[2]][,1],full_indiv_data[,1])
})
