## requires "PASWR" package
## link to the package: https://cran.r-project.org/web/packages/PASWR/PASWR.pdf
library(testthat)
a<-rnorm(50)
b<-sample(c(0,1),replace = T,size =50)
C<-rnorm(50)
d<-sample(c(0,1),replace = T,size =50)
mu<-0.5
test_that("test for FLzTest1S",{
  rz<-PASWR::z.test(a,sigma.x=sd(a),mu=mu)
  flz<-z.test(as.FLVector(a),test_val = mu)
  FLexpect_equal(as.numeric(rz[["p.value"]]),as.numeric(flz[["p.value"]]),tolerance=1e-5)
  FLexpect_equal(as.numeric(rz[["statistic"]]),as.numeric(flz[["statistic"]]),tolerance=1e-5)
})

test_that("test for FLzTest1P",{
  flt <- FLTable(getTestTableName("tblzTest"),
        "obsid", 
        whereconditions= "groupid=1")
  flx <- flt[[2]]
  flz <- z.test(flx, prob= 1, test_val= 0.45)
  FLexpect_equal(as.numeric(flz$p.value), 0.0004, tolerance= 1e-3)
  FLexpect_equal(as.numeric(flz$statistic), 3.5176, tolerance= 1e-3)
})

test_that("test for FLzTest2S",{
  rz<-PASWR::z.test(a,C,sigma.x=sd(a),sigma.y=sd(C))
  flz<-z.test(as.FLVector(a),as.FLVector(C))
  FLexpect_equal(as.numeric(rz[["p.value"]]),as.numeric(flz[["p.value"]]),tolerance=1e-5)
  FLexpect_equal(as.numeric(rz[["statistic"]]),as.numeric(flz[["statistic"]]),tolerance=1e-5)
})

test_that("test for FLzTest2P",{
  flt1 <- FLTable(getTestTableName("tblzTest"),
        "obsid", 
        whereconditions= "groupid=1")
  flt2 <- FLTable(getTestTableName("tblzTest"),
        "obsid", 
        whereconditions= "groupid=2")
  flx <- flt1$NUM_VAL
  fly <- flt2$NUM_VAL
  if(is.TDAster())
    flx <- flt1$num_val
    fly <- flt2$num_val
  flz <- z.test(flx, fly, prob= 1)
  FLexpect_equal(as.numeric(flz[["p.value"]]),0.06176, tolerance= 1e-3)
  FLexpect_equal(as.numeric(flz[["statistic"]]),1.8680, tolerance= 1e-3)
})

