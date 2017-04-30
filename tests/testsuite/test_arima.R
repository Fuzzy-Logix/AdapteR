Renv=new.env(parent = globalenv())
Renv$x<-rnorm(1000)
FLenv<-as.FL(Renv)

test_that("Test with no d for arima",{
  robj<-arima(Renv$x, order=c(3,0,0))
  flobj<-arima(FLenv$x, order=c(3,0,0))
  result1=expect_equal(length(robj$coef),length(flobj$coef))
  result2=expect_equal(flobj$sigma2>1,robj$sigma2>1)
  result3=expect_equal(flobj$nobs,robj$nobs)
  result4=expect_equal(sign(flobj$loglik),sign(robj$loglik))
})

test_that("Test with d for arima",{
  robj<-arima(Renv$x, order=c(3,1,0))
  flobj<-arima(FLenv$x, order=c(3,1,0))
  result1=expect_equal(length(robj$coef),length(flobj$coef)-1)
  result2=expect_equal(flobj$sigma2>1,robj$sigma2>1)
  result3=expect_equal(flobj$nobs,robj$nobs)
  result4=expect_equal(sign(flobj$loglik),sign(robj$loglik))
})
