Renv=new.env(parent = globalenv())
Renv$x<-c(0,0)
Renv$y<-1:2
Renv$fls<-var(cbind(1:6,2:8))
FLenv=as.FL(Renv)

test_that("test for mahalanobis distance",{
  result=eval_expect_equal({
    object<-mahalanobis(x,y,fls)
  },Renv,FLenv,
    tolerance=0.000001)
})
