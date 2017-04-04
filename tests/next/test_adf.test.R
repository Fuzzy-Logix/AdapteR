Renv=new.env(parent=globalenv())
x<-rnorm(1000)
FLenv<-as.FL(Renv)
k<-8

## fails presently. Need to figure out why.
test_that("test for Augmented Dickey Fuller test",{
	result=eval_expect_equal({
		obj<-adf.test(x, k= k)
		},Renv,FLenv)
	})