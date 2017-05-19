flv<-as.FLVector(co2)
periodicity<-7

test_that("test for FLHoltWinters",{
  flobj1<-HoltWinters(flv, periodicity=periodicity)
  flobj2<-HoltWinters(flv, beta = FALSE, periodicity=periodicity)
  result1 = expect_equal(length(flobj1$fitted),length(flobj2$fitted),periodicity)
  result2 = expect_equal(flobj1$alpha,flobj2$alpha)
  result3 = expect_equal(flobj1$gamma,flobj2$gamma)
})

test_that("test for FLexponential smoothing functions",{
  flobj1<-HoltWinters(flv, gamma = FALSE)
  flobj2<-HoltWinters(flv, beta= FALSE, gamma = FALSE) 
  result1 = expect_equal(length(flobj1$fitted),length(flobj2$fitted),periodicity)
  result2 = expect_equal(flobj1$alpha,flobj2$alpha)
})