
## gam function doesn't work
## asana ticket- https://app.asana.com/0/143316600934101/158507710191280


test_that("test for gam",{
  
  fldata <- FLTable("FL_DEMO.tblGAMSimData","ObsID")
  rdata<-as.data.frame(fldata)
  colnames(rdata)<-colnames(fldata)
  formula <-yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)
  flobj <- gam(formula=formula,data=fldata,offset="x2Val")
  robj <- gam(formula=yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)+offset(x2Val),data=rdata,family = poisson)
  expect_equal(coef(flobj),coef(robj))

})
