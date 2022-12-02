test_that("indiv_info_add works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_2.RDS"))
  indiv_info<-readRDS(test_path("fixtures","indiv_info_1.RDS"))

  t_res<-indiv_info_add(indiv_info=indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data=indiv_data)

  expect_equal(length(t_res),2)
  expect_equal(t_res[[1]][,1],indiv_data[,1])

})
