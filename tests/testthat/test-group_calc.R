test_that("group_calc works", {
  gs<-genNetDem::group_calc(100,5)
  expect_equal(sum(gs), 100)
})
