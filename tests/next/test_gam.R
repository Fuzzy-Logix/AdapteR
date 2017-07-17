
## gam function doesn't work

	fldata <- FLTable(getTestTableName("tblGAMSimData"),
	  					"ObsID")
	rdata<-as.data.frame(fldata)
	colnames(rdata)<-colnames(fldata)
	formula <-yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)
	flobj <- gam(formula=formula,data=fldata,offset="x2Val")
	robj <- gam(formula=yVal~x0Val+s(x1Val,m=3,k=10)+te(x1Val,x2Val,m=3,k=5)+s(x2Val,x1Val)+offset(x2Val),data=rdata,family = poisson)


test_that("test for gam",{
	
	flPred <- predict(flobj, fldata)
	RPred <- predict(robj, rdata)
	expect_equal(length(flPred), length(RPred))

	expect_equal(length(flobj$fitted.values), length(robj$fitted.values))
	expect_equal(length(flobj$residuals), length(robj$residuals))	

	## this test fails because of difference in lengths of coefficients.
	## asana ticket- https://app.asana.com/0/143316600934101/158507710191280
	expect_equal(coef(flobj),coef(robj))
})


## DBLytix Manual Example

fldata <- FLTable(getTestTableName("tblGAMSimData"),
  					"ObsID")
rdata<-as.data.frame(fldata)
colnames(rdata)<-colnames(fldata)
rdata$groupID <- as.factor(rdata$groupID) ## mgcv::gam needs factor or numeric in by varname
formula <-yVal~s(x0Val,m=3,k=10,by=groupID)+te(x1Val,x2Val,m=3,k=5)
flobj <- gam(formula=formula,data=fldata)
robj <- gam(formula=formula,data=rdata,family = poisson)

## Coefficients do not match
## JIRA :- https://fuzzyl.atlassian.net/browse/TDFL-831
## https://app.asana.com/0/143316600934101/158507710191280
test_that("test for coefficients from FLGAM dblytix Example https://fuzzyl.atlassian.net/browse/TDFL-831",{
	expect_equal(coef(flobj),coef(robj))
})

test_that("test for fitted.values and residuals from FLGAM dblytix Example",{
	expect_equal(length(flobj$fitted.values), length(robj$fitted.values))
	expect_equal(length(flobj$residuals), length(robj$residuals))
})

test_that("test if scoring works using predict for FLGAM dblytix Example",{
	flPred <- predict(flobj, fldata)
	RPred <- predict(robj, rdata)
	expect_equal(length(flPred), length(RPred))
})


