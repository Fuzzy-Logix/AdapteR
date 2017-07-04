Renv = new.env(parent = globalenv())
Renv$x <- rnorm(50, mean = 0, sd = 2)
Renv$y <- rnorm(30, mean = 1, sd = 1)
FLenv <- as.FL(Renv)

test_that("var.test : ", {
    result = eval_expect_equal({
        res <- var.test(x, y)
        statistic <- res$statistic
        pValue <- res$p.value
        estimate <- res$estimate
        ConfInterval <- res$conf.int
    },Renv,FLenv,
    expectation = c("statistic", "pValue", "estimate", "ConfInterval"),
    noexpectation = c("res"),
    check.attributes = T
    )
})


