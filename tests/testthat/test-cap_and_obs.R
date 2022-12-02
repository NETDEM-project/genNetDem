test_that("cap_and_obs works", {
  gbi<-readRDS(test_path("fixtures","gbi_1.RDS"))
  samp_wind<-readRDS(test_path("fixtures","samp_wind_1.RDS"))

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
              pcg=1.1,pmi=0.9,pci=0.9,
              start_obs=1,end_obs=max(samp_wind),interval_obs=1,
              start_cap=1,end_cap=max(samp_wind),interval_cap=1,
              pre_cap=NULL),"pcg is a probability, please set accordingly")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=1.1,pci=0.9,
                           start_obs=1,end_obs=max(samp_wind),interval_obs=1,
                           start_cap=1,end_cap=max(samp_wind),interval_cap=1,
                           pre_cap=NULL),"pmi is a probability, please set accordingly")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=1.1,
                           start_obs=1,end_obs=max(samp_wind),interval_obs=1,
                           start_cap=1,end_cap=max(samp_wind),interval_cap=1,
                           pre_cap=NULL),"pci is a probability, please set accordingly")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=0.9,
                           start_obs=0,end_obs=max(samp_wind),interval_obs=1,
                           start_cap=1,end_cap=max(samp_wind),interval_cap=1,
                           pre_cap=NULL),"start_obs must lie within the sampling window")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=0.9,
                           start_obs=1,end_obs=max(samp_wind),interval_obs=1,
                           start_cap=0,end_cap=max(samp_wind),interval_cap=1,
                           pre_cap=NULL),"start_cap must lie within the sampling window")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=0.9,
                           start_obs=1,100,interval_obs=1,
                           start_cap=1,end_cap=max(samp_wind),interval_cap=1,
                           pre_cap=NULL),"end_obs must lie within the sampling window")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=0.9,
                           start_obs=1,end_obs=max(samp_wind),interval_obs=1,
                           start_cap=1,100,interval_cap=1,
                           pre_cap=NULL),"end_cap must lie within the sampling window")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=0.9,
                           start_obs=5,end_obs=4,interval_obs=1,
                           start_cap=1,max(samp_wind),interval_cap=1,
                           pre_cap=NULL),"end_obs must be after start_obs")

  expect_error(genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                           pcg=0.5,pmi=0.9,pci=0.9,
                           start_obs=1,end_obs=max(samp_wind),interval_obs=1,
                           start_cap=5,4,interval_cap=1,
                           pre_cap=NULL),"end_cap must be after start_cap")

  t_res<-genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
              pcg=1,pmi=1,pci=1,
              start_obs=1,end_obs=max(samp_wind),interval_obs=1,
              start_cap=1,end_cap=2,interval_cap=1,
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

  t_res<-genNetDem::cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                     pcg=0.5,pmi=1,pci=1,
                     start_obs=1,end_obs=max(samp_wind),interval_obs=1,
                     start_cap=1,end_cap=2,interval_cap=1,
                     pre_cap=NULL)

  expect_equal(sort(unique(as.vector(t_res[[1]]))),c(0,1))
  expect_equal(sort(unique(as.vector(t_res[[3]]))),c(0,1))
  expect_equal(length(t_res[[2]]),sum(rowSums(t_res[[1]])>0))
  expect_true(length(unique(c(t_res[[4]],t_res[[2]])))>=sum(rowSums(t_res[[3]])>0))

})
