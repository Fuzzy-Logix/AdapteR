Renv = new.env(parent = globalenv())

Renv$var1 = c(3.7,3.3,3.5,2.8)
Renv$weight1 = c(5,  5,  4,  1)/15

FLenv = as.FL(Renv)

# Sum of all the weight should be 1.
#Default weight is considered to be equal for all observations.

test_that("Check for weighted mean function",{
          result = eval_expect_equal({
                   test1 = weighted.mean(var1, weight1)
            },Renv,FLenv)
          print(result)
    })

