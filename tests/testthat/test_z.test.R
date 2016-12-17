## requires "PASWR" package
## link to the package: https://cran.r-project.org/web/packages/PASWR/PASWR.pdf
a<-rnorm(50)
b<-sample(c(0,1),replace = T,size =50)
C<-rnorm(50)
d<-sample(c(0,1),replace = T,size =50)
mu<-0.5
test_that("test for FLzTest1S",{
  rz<-z.test(a,sigma.x=sd(a),mu=mu)
  flz<-z.test(as.FLVector(a),test_val = mu)
  FLexpect_equal(as.numeric(rz[["p.value"]]),as.numeric(flz[["p.value"]]),tolerance=1e-5)
  FLexpect_equal(as.numeric(rz[["statistic"]]),as.numeric(flz[["statistic"]]))
})

test_that("test for FLzTest1P",{
  rz<-z.test(b,sigma.x=sd(b),mu=mu)
  flz<-z.test(as.FLVector(b),test_val = mu)
  FLexpect_equal(as.numeric(rz[["p.value"]]),as.numeric(flz[["p.value"]]),tolerance=1e-5)
  FLexpect_equal(as.numeric(rz[["statistic"]]),as.numeric(flz[["statistic"]]))
})

test_that("test for FLzTest2S",{
  rz<-z.test(a,C,sigma.x=sd(a),sigma.y=sd(C))
  flz<-z.test(as.FLVector(a),as.FLVector(C))
  FLexpect_equal(as.numeric(rz[["p.value"]]),as.numeric(flz[["p.value"]]),tolerance=1e-5)
  FLexpect_equal(as.numeric(rz[["statistic"]]),as.numeric(flz[["statistic"]]),tolerance=1e-5)
})

test_that("test for FLzTest2P",{
  rz<-z.test(b,d,sigma.x=sd(b),sigma.y=sd(d))
  flz<-z.test(as.FLVector(b),as.FLVector(d))
  FLexpect_equal(as.numeric(rz[["p.value"]]),as.numeric(flz[["p.value"]]),tolerance=1e-5)
  FLexpect_equal(as.numeric(rz[["statistic"]]),as.numeric(flz[["statistic"]]))
})

