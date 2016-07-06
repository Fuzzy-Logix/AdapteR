Renv = new.env(parent = globalenv())
Renv$x <- 1:10

FLenv <- as.FL(Renv)
##https://app.asana.com/0/143778401455745/145657030318443
## cbind,rbind fail
test_that("cbind to a matrix",{
    result = eval_expect_equal({
        m <- cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
        dim(m)
    }, Renv, FLenv)
    ##print(result)
})
