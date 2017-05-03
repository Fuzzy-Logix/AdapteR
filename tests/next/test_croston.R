y<-rpois(20, lambda = 0.3)
flv<-as.FLVector(y)
h<-10

test_that("test for croston",{
  flobj<-croston(flv, h=h)
  result1 = expect_equal(length(flobj$mean),h)
  result2 = expect_equal(length(flobj$fitted),length(flobj$x),length(flobj$residuals))
  result3 = expect_equal(all(flobj$fitted),TRUE)
})