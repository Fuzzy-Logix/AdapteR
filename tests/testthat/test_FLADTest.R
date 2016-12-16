##Anderson-Darling Test:Test Case 1

Renv = new.env(parent = globalenv())
set.seed(200)
Renv$a <- rnorm(100, mean = 5, sd = 3)
FLenv = as.FL(Renv)

test_that("Anderson-Darling Test: nortest package example", {
    result = eval_expect_equal({
        res <- ad.test(a)
    },Renv,FLenv,
    expectation = "res",
    check.attributes = T,
    tolerance = .01,
    verbose = T
    )
    })
