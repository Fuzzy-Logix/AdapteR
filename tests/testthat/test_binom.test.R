Renv = new.env(parent = globalenv())
FLenv = as.FL(Renv)

Renv$a <- 682
FLenv <- as.FL(Renv)

## p.value does not match and conf.int not available
test_that("binom.test R example: check result Equality without p.value:",{
    result = eval_expect_equal({
            res1 <- binom.test(a,682+243,p=3/4)
            class(res1) <- "list"
            res1$p.value <- NULL
            res1$conf.int <- NULL
    },Renv,FLenv,
    expectation=c("res1"),
    check.attributes=FALSE)
})
