Renv=new.env(parent=globalenv())
## Using R examples
Renv$obja<-1:10
Renv$objb<-7:20
FLenv<-as.FL(Renv)

test_that("one sample t test",{
	result = eval_expect_equal({
		obj<-t.test(obja)
	},Renv,FLenv,
        tolerance=1e-4,
        expectation=c("obj"),
        verbose=F)
})

test_that("two sample t test: ",{
	result = eval_expect_equal({
		obj1<-t.test(obja,objb)
		obj2<-t.test(obja,objb,var.equal = TRUE)
	},Renv,FLenv,
        tolerance=1e-4,
	expectation=c("obj","obj2"),
        verbose=F
        )
})

