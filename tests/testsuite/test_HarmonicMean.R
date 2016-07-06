Renv = new.env(parent = globalenv())

Renv$var1 = seq(1,5)
Renv$var2 = (Renv$var1)^2

FLenv = as.FL(Renv)

test_that("Check for harmonic mean function",{
          result = eval_expect_equal({
                   test1 = harmonic.mean(var1)
                   test2 = harmonic.mean(var2)
            },Renv,FLenv)
          ##print(result)
    })
