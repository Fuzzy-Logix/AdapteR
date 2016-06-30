Renv = new.env(parent = globalenv())
Renv$var1 = seq(-2, 4, by = .5)

FLenv <- as.FL(Renv)

#Developement in progress
test_that("Check for ceiling function ",{
    result = eval_expect_equal({
        test1 <- ceiling(var1)
    }, Renv, FLenv)
    print(result)
})
