Renv = new.env(parent = globalenv())

Renv$var1 =  rnorm(10)
Renv$prob1 = c(0.1, 0.5, 1, 2)/100

FLenv = as.FL(Renv)

## Precision Error in names of output in Hadoop
## Very minor
test_that("quantile: r vector probs",{
    result = eval_expect_equal({
        test10 = quantile(var1)
        test11 = quantile(var1,probs = prob1)
    },Renv,FLenv,
    tolerance=1e-2/length(Renv$var1))
})


test_that("quantile: FLVector probs",{
    result = eval_expect_equal({
        prob2 = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100
        test12 = quantile(var1,probs = prob2)
    },Renv,FLenv,
    tolerance=1e-2/length(Renv$var1))
})
