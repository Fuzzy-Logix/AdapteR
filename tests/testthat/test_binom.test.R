Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)
v <- c("two.sided", "less", "greater")
Renv$a <- 682
FLenv <- as.FL(Renv)


## p.value differ by a small number and conf.int is not available
test_that("binom.test R example: check result Equality without p.value:",{
    result = eval_expect_equal({
        res1 <- binom.test(a,682+243,p=3/4)
        estimate <- res1$estimate
        pValue <- res1$p.value 
    },Renv,FLenv,
    expectation=c("estimate", "pValue"), 
    noexpectation = c("res1"),
    tolerance = 0.01,
    scale = 1,
    check.attributes=FALSE)
})


# "TWO_SIDED" "LE" "GE"
test_that("binom.test R example: check result Equality without p.value:",{
	for (i in v){
		res <- binom.test(FLenv$a,682+243,p=3/4, i)
		expect_equal(as.R(res$estimate), binom.test(682,682+243,p=3/4, i)$estimate)
		expect_equal(as.R(res$p.value), binom.test(682,682+243,p=3/4, i)$p.value, tolerance = 0.01, scale = 1)
	}    	    
})

