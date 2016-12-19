Renv = new.env(parent = globalenv())

Renv$var1 = 1:4
Renv$var2 = c(1:3, 100, 1000)

FLenv = as.FL(Renv)



test_that("check median result ",{
          result = eval_expect_equal({
                   test1 = median(var1)
                   test2 = median(var2)
            },Renv,FLenv)
          ##print(result)
    })


