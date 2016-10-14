#Testing LCM (R Package: numbers)
#Error, function not in FL yet
Renv <- new.env(parent = globalenv())
Renv$a<-c(12,10)
Renv$b<-c(46368,75025)
FLenv <- as.FL(Renv)
test_that("Testing lcm", {
  result1=eval_expect_equal({
  test1<-LCM(a)
  test2<-LCM(b)
  },Renv,FLenv)
})
