
a<-rnorm(50)
b<-sample(c(0,1),replace = T,size =50)
c<-rnorm(50)
d<-sample(c(0,1),replace = T,size =50)
mu<-0.5
test_that("test for FLzTest1S",{
  z<-(mean(a)-mu)/(sd(a)/sqrt(length(a)))
  p<-2*pnorm(-abs(z))
  flz<-z.test(as.FLVector(a),test_val = mu)
  FLexpect_equal(z,as.numeric(flz[["parameter"]]))
  FLexpect_equal(p,as.numeric(flz[["statistic"]]))
})

test_that("test for FLzTest1P",{
  z<-(mean(b)-mu)/sqrt(mu*(1-mu)/(length(b)))
  p<-2*pnorm(z,lower.tail = FALSE)
  flz<-z.test(as.FLVector(b),test_val = mu,prob =1)
  FLexpect_equal(z,as.numeric(flz[["parameter"]]))
  FLexpect_equal(p,as.numeric(flz[["statistic"]]))
})

test_that("test for FLzTest2S",{
  z<-(mean(a)-mean(c))/(sqrt((var(a)/length(a))+(var(c)/length(c))))
  p<-2*pnorm(-abs(z))
  flz<-z.test(as.FLVector(a),as.FLVector(c))
  FLexpect_equal(z,as.numeric(flz[["parameter"]]))
  FLexpect_equal(p,as.numeric(flz[["statistic"]]))
})

test_that("test for FLzTest2P",{
  p1<-mean(b)
  p2<-mean(d)
  p<-(p1*length(b)+p2*length(d))/(length(b)+length(d))
  z<-(mean(b)-mean(d))/sqrt(p*(1-p)*((1/length(b))+(1/length(d))))
  pval<-2*pnorm(-abs(z))
  flz<-z.test(as.FLVector(b),as.FLVector(d),prob = 1)
  FLexpect_equal(z,as.numeric(flz[["parameter"]]))
  FLexpect_equal(pval,as.numeric(flz[["statistic"]]))
})

