## Fails to properly evaluate default R function due to
## R's implementation uses environments
Renv = new.env(parent = globalenv())
FLenv <- as.FL(Renv)

Renv$wb <- aggregate(warpbreaks$breaks,
                  by = list(w = warpbreaks$wool,
                            t = warpbreaks$tension),
                  FUN = mean)

FLenv$wb <- as.FLTable(Renv$wb,
                    tableName="ARBaseTestTempTable",
                    drop=TRUE)


test_that("Friedman Test on FLTable: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit <- friedman.test(x~w|t, data = wb)
            # fit$p.value <- NULL
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})

## Fails on TD and Aster

test_that("Friedman Test on FLVectors: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit <- friedman.test(wb$x,wb$w,wb$t)
    },Renv,FLenv,
    expectation=c("fit"))
})

##@phani: wrong results on Aster
test_that("Friedman Test on FLTable: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit <- friedman.test(x~w|t, data = wb)
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})
