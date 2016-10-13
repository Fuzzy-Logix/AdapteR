Renv=new.env(parent=globalenv())
Renv$obja<-rnorm(50)
Renv$objb<-sample(c(1,2),replace = T,size = 25)
FLenv<-as.FL(Renv)

test_that("one sample t test",{
	result = eval_expect_equal({
		obj<-t.test(obja)
	},Renv,FLenv,
        tolerance=1e-4,
        verbose=F)
})


test_that("two sample t test: conf-int wrong https://app.asana.com/0/143316600934101/195568724935159",{
	result = eval_expect_equal({
		obj<-t.test(obja,objb)
		obj2<-t.test(obja,objb,var.equal = TRUE)
	},Renv,FLenv,
        tolerance=1e-4,
	expectation=c("obj","obj2"),
        verbose=F
        )
})

