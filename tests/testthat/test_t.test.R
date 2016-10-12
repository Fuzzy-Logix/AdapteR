Renv=new.env(parent=globalenv())
Renv$obja<-rnorm(50)
Renv$objb<-sample(c(1,2),replace = T,size = 25)
FLenv<-as.FL(Renv)

test_that("one sample t test",{
	result = eval_expect_equal({
		obj<-t.test(obja)
	},Renv,FLenv,
        expectation="obj",
        verbose=T)
})


test_that("two sample t test",{
	result = eval_expect_equal({
		obj<-t.test(obja,objb)
		obj2<-t.test(obja,objb,var.equal = TRUE)
	},Renv,FLenv,
	expectation=c("obj","obj2")
        )
})

