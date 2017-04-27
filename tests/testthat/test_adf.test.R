Renv=new.env(parent=globalenv())
Renv$x<-rnorm(1000)
FLenv<-as.FL(Renv)
k<-6

test_that("test for Augmented Dickey Fuller test",{
	result=eval_expect_equal({
		obj<-adf.test(x, k= k)
		},Renv,FLenv)
	})
Renv$obj<-adf.test(Renv$x, k=k)
FLenv$obj<-adf.test(FLenv$x, k=k)
