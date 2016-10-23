#Testing GCD (R Package: numbers)
#Error,function not in FL yet
Renv <- new.env(parent = globalenv())
Renv$a <-c(12,10)
Renv$a=as.integer(Renv$a)
Renv$b<-c(46368,75025)
Renv$b=as.integer(Renv$b)
FLenv <- as.FL(Renv)
test_that("Testing gcd", {
  result1=eval_expect_equal({
    test1<-GCD(a)
    test2<-GCD(b)
  },Renv,FLenv)
})
