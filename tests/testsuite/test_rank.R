Renv = new.env(parent = globalenv())

Renv$var1 = c(3, 1, 4, 15, 92)
FLenv = as.FL(Renv)

test_that("rank: default method average, named vectors",{
    result = eval_expect_equal({
        test1 = rank(var1)
    },Renv,FLenv,
    expectation=c("test1"))
    ##print(result)
})

test_that("rank: check for idempotent characteristic of rank function with ties.method=perc, duplicate",{
    result = eval_expect_equal({
        rtest1 <- rank(test1)
        expect_equal(as.vector(rtest1),as.vector(test1))
    },Renv,FLenv)
    ##print(result)
})
