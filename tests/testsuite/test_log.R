## Test for <log> function using examples from R Documentation

Renv = new.env(parent = globalenv())
Renv$n1 = 3
Renv$n2 = 1e7
Renv$x <- 1:10
Renv$nvector <- 1:9

FLenv <- as.FL(Renv)

#log/exp
test_that("Log and Exp: tolerance=1e-6",{
    result = eval_expect_equal({
            len1 <- log(exp(n1))
            ln2 <- log10(n2)
            exp1 <- expm1(x)
            exp1 <- log1p(x)
    }, Renv, FLenv,
    tolerance=1e-6
    )
})

## power not yet supported in Hadoop
test_that("power, vector arithmetic",{
    result = eval_expect_equal({
        constVec <- 2.1*nvector
        y <- 10.0^-(1+constVec)
        length(nvector)
    }, Renv, FLenv,
    expectation = c("constVec","y"))
})

## power not yet supported in Hadoop
test_that("power, vector arithmetic, integer types",{
    result = eval_expect_equal({
        constVec <- 2*nvector
        y <- 10^-(1+constVec)
        length(nvector)
    }, Renv, FLenv,
    expectation = c("constVec","y"))
})

