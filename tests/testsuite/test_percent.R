Renv = new.env(parent = globalenv())

Renv$var1 = runif(10)
Renv$var2 = runif(10, 1, 10)

FLenv = as.FL(Renv)

test_that("percent",{
          result = eval_expect_equal({
                   test1 = percent(var1)
                   test2 = percent(var2)
            },Renv,FLenv)
})


