Renv=new.env(parent=globalenv())
Renv$obja<-rnorm(50)
Renv$objb<-sample(c(1,2),replace = T,size = 25)
FLenv<-as.FL(Renv)

test_that("one sample t test",{
	result = eval_expect_equal({
		obj<-t.test(obja)
	},Renv,FLenv,
	noexpectation="obj")
for(n in c("statistic","p.value"))
	FLexpect_equal(as.vector(Renv$obj[[n]]),as.vector(FLenv$obj[[n]]))
})

test_that("two sample t test",{
	result = eval_expect_equal({
		obj<-t.test(obja,objb)
		obj2<-t.test(obja,objb,var.equal = TRUE)
	},Renv,FLenv,
	noexpectation=c("obj","obj2")
)
for(n in c("statistic","p.value")){
	FLexpect_equal(as.vector(Renv$obj[[n]]),as.vector(FLenv$obj[[n]]))
  FLexpect_equal(as.vector(Renv$obj2[[n]]),as.vector(FLenv$obj2[[n]]))}
})

