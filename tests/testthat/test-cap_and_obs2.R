test_that("cap_and_obs2 works", {
  gbi<-readRDS(test_path("fixtures","gbi_1.RDS"))
  samp_wind<-readRDS(test_path("fixtures","samp_wind_1.RDS"))

  expect_error(genNetDem::cap_and_obs2(samp_wind,gbi=gbi,
                           pcg=1.1,pmg=0.5,pmi=0.9,pci=0.9,
                           obs_seq=1,
                           cap_seq=1,
                           pre_cap=NULL),"pcg is a probability, please set accordingly")

  expect_error(genNetDem::cap_and_obs2(samp_wind,gbi=gbi,
                           pcg=0.5,pmg=1.1,pmi=0.9,pci=0.9,
                           obs_seq=1,
                           cap_seq=1,
                           pre_cap=NULL),"pmg is a probability, please set accordingly")

  expect_error(genNetDem::cap_and_obs2(samp_wind,gbi=gbi,
                           pcg=0.5,pmg=0.5,pmi=1.1,pci=0.9,
                           obs_seq=1,
                           cap_seq=1,
                           pre_cap=NULL),"pmi is a probability, please set accordingly")

  expect_error(genNetDem::cap_and_obs2(samp_wind,gbi=gbi,
                           pcg=0.5,pmg=0.5,pmi=0.9,pci=1.1,
                           obs_seq=1,
                           cap_seq=1,
                           pre_cap=NULL),"pci is a probability, please set accordingly")

  t_res<-genNetDem::cap_and_obs2(samp_wind,gbi=gbi,
                      pcg=1,pmg=1,pmi=1,pci=1,
                      obs_seq=seq(1,20,1),
                      cap_seq=seq(1,2,1),
                      pre_cap=NULL)

  expect_equal(sort(unique(as.vector(t_res[[1]]))),c(0,1))
  expect_equal(sort(unique(as.vector(t_res[[3]]))),c(0,1))
  expect_equal(length(t_res[[2]]),sum(rowSums(t_res[[1]])>0))
  expect_equal(length(unique(c(t_res[[4]],t_res[[2]]))),sum(rowSums(t_res[[3]])>0))

  t_seq<-seq(1,2,1)
  expect_equal(unique(samp_wind[t_res[[2]]]),t_seq)
  t_seq2<-seq(1,20,1)
  t_seq2<-t_seq2[t_seq2%in%t_seq==FALSE]
  expect_equal(unique(samp_wind[t_res[[4]]]),t_seq2)

  t_res<-genNetDem::cap_and_obs2(samp_wind,gbi=gbi,
                      pcg=0.5,pmg=0.5,pmi=1,pci=1,
                      obs_seq=seq(1,20,1),
                      cap_seq=seq(1,2,1),
                      pre_cap=NULL)

  expect_equal(sort(unique(as.vector(t_res[[1]]))),c(0,1))
  expect_equal(sort(unique(as.vector(t_res[[3]]))),c(0,1))
  expect_equal(length(t_res[[2]]),sum(rowSums(t_res[[1]])>0))
  expect_true(length(unique(c(t_res[[4]],t_res[[2]])))>=sum(rowSums(t_res[[3]])>0))

})
