Renv=new.env(parent=globalenv())
Renv$x<-rnorm(100)
Renv$y<-rnorm(100)
FLenv<-as.FL(Renv)

## test fails due to different stats values
test_that("Siegel Tukey Test for 2 vectors",{
	result = eval_expect_equal({
		obj<-SiegelTukeyTest(x,y)
		},Renv,FLenv,
		noexpectation="obj")
	for(n in c("statistic","p.value"))
	FLexpect_equal(as.vector(Renv$obj[[n]]),as.vector(FLenv$obj[[n]]))
})
