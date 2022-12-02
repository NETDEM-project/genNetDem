test_that("indiv_info_gen works", {
  indiv_data<-readRDS(test_path("fixtures","indiv_data_1.RDS"))

  t_res<-data.frame(indiv_data$indivs)
  t_res<-indiv_info_gen(t_res,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))

  expect_equal(nrow(t_res[[1]]),nrow(indiv_data))
  expect_equal(as.character(sort(unique(t_res[[1]][,2]))),c("F","M"))
  expect_equal(t_res[[2]][[1]][[1]],c("sex"))
  expect_equal(t_res[[2]][[1]][[2]],c("fac"))
  expect_equal(t_res[[2]][[1]][[4]],c("M","F"))

  t_res<-data.frame(indiv_data$indivs)
  t_res<-indiv_info_gen(t_res,ii_tag=NULL,indiv_data=indiv_data,trait="size",trait_type="cov",level_names=NULL,x_dist="unif")

  expect_equal(nrow(t_res[[1]]),nrow(indiv_data))
  expect_equal(t_res[[2]][[1]][[1]],c("size"))
  expect_equal(t_res[[2]][[1]][[2]],c("cov"))
  expect_equal(t_res[[2]][[1]][[3]],c("unif"))
  expect_equal(sum(t_res[[1]][,2]<0|t_res[[1]][,2]>1),0)

  t_res<-data.frame(indiv_data$indivs)
  t_res<-indiv_info_gen(t_res,ii_tag=NULL,indiv_data=indiv_data,trait="size",trait_type="cov",level_names=NULL,x_dist="norm")

  expect_equal(nrow(t_res[[1]]),nrow(indiv_data))
  expect_equal(t_res[[2]][[1]][[1]],c("size"))
  expect_equal(t_res[[2]][[1]][[2]],c("cov"))
  expect_equal(t_res[[2]][[1]][[3]],c("norm"))

  t_res<-data.frame(indiv_data$indivs)
  t_res<-indiv_info_gen(t_res,ii_tag=NULL,indiv_data=indiv_data,trait="size",trait_type="cov",level_names=NULL,x_dist="lnorm")

  expect_equal(nrow(t_res[[1]]),nrow(indiv_data))
  expect_equal(t_res[[2]][[1]][[1]],c("size"))
  expect_equal(t_res[[2]][[1]][[2]],c("cov"))
  expect_equal(t_res[[2]][[1]][[3]],c("lnorm"))
  expect_equal(sum(t_res[[1]][,2]<0),0)

  t_res<-data.frame(indiv_data$indivs)
  t_res<-indiv_info_gen(t_res,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
  t_res<-indiv_info_gen(t_res[[1]],ii_tag=t_res[[2]],indiv_data=indiv_data,trait="size",trait_type="cov",level_names=NULL,x_dist="lnorm")

  expect_equal(nrow(t_res[[1]]),nrow(indiv_data))
  expect_equal(ncol(t_res[[1]]),3)
  expect_equal(length(t_res[[2]]),2)

})
