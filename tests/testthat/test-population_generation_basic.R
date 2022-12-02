test_that("population_generation_basic works", {
  t_pop<-genNetDem::population_generation_basic(n=100,ng=100,plot=FALSE)
  expect_equal(nrow(t_pop[[1]]),100)
  expect_equal(length(unique(t_pop[[1]][,2])),100)
  expect_equal(dim(t_pop[[2]]),c(100,100))

  t_pop<-genNetDem::population_generation_basic(n=100,ng=10,plot=FALSE)
  expect_equal(nrow(t_pop[[1]]),100)
  expect_equal(length(unique(t_pop[[1]][,2]))<=10,TRUE)
  expect_equal(dim(t_pop[[2]]),c(100,100))

  expect_error(genNetDem::population_generation_basic(n=10,ng=1000,plot=FALSE),"invalid number of groups")
})
