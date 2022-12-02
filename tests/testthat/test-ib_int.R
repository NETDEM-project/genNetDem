test_that("ib_int works", {
  t_comp<-genNetDem::ib_int(5,10)
  expect_equal(t_comp,c(6,7,8,9))
  t_comp<-genNetDem::ib_int(12,12)
  expect_equal(t_comp,NULL)
  t_comp<-genNetDem::ib_int(105,103)
  expect_equal(t_comp,104)
})
