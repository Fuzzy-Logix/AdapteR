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

## The result does not match with R.
data <- FLTable("tblSTTest","obsid","groupid","num_val", "datasetid = 1")
FLenv$a <- data$'1'
FLenv$b <- data$'2'
Renv$a <- as.vector(FLenv$a)
Renv$b <- as.vector(FLenv$b)

test_that("Siegel Tukey Test : DB-Lytix Example",{
	result = eval_expect_equal({
		res <- SiegelTukeyTest(a,b)
		},Renv,FLenv,
		noexpectation="res")
	for(n in c("statistic","p.value"))
	expect_equal(as.vector(Renv$res[[n]]),as.vector(FLenv$res[[n]]))
})
