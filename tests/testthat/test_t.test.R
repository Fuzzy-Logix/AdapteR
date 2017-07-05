Renv=new.env(parent=globalenv())
## Using R examples
Renv$obja<-1:10
Renv$objb<-7:20
FLenv<-as.FL(Renv)

test_that("one sample t test",{
	result = eval_expect_equal({
		obj <- t.test(obja)
		expr1 <- obj$statistic
		expr2 <- obj$p.value
	},Renv,FLenv,
        tolerance=1e-4,
        expectation=c("expr1", "expr2"),
        noexpectation= c("obj"),
        verbose=F
        )
})

test_that("two sample t test: ",{
	result = eval_expect_equal({
		obj1<-t.test(obja,objb)
		obj2<-t.test(obja,objb,var.equal = TRUE)
		expr1 <- obj1$statistic
		expr2 <- obj1$p.value
		expr3 <- obj2$statistic
		expr4 <- obj2$p.value
	},Renv,FLenv,
        tolerance=1e-4,
		expectation=c("expr1","expr2","expr3","expr4"),
		noexpectation= c("obj1", "obj2"),
        verbose=F
        )
})

