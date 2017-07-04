
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
