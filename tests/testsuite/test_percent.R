Renv = new.env(parent = globalenv())

Renv$var1 = runif(10)
Renv$var2 = runif(10, 1, 10)

FLenv = as.FL(Renv)

## Fails in Hadoop: no index column in output
## https://app.asana.com/0/150173007236461/233442378511381
test_that("percent https://app.asana.com/0/150173007236461/233442378511381",{
          result = eval_expect_equal({
                   test1 = percent(var1)
                   test2 = percent(var2)
            },Renv,FLenv)
})


