test_that("jpc works", {
  test_mat<-matrix(0,nr=10,nc=10)
  for(i in 1:9){
    for(j in (i+1):10){
      test_mat[i,j]<-test_mat[j,i]<-rbinom(1,1,0.4)*rbeta(1,2,10)
    }
  }
  can_join<-3:10
  t_mat<-test_mat[1:2,can_join]
  t_res<-apply(t_mat,2,genNetDem::jpc,pm=50,float=0.001)
  expect_equal(sum(t_res<0.001), 0)
  expect_equal(length(t_res), dim(t_mat)[2])
})
