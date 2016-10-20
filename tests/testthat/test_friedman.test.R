Renv = new.env(parent = globalenv())

Renv$RoundingTimes <- matrix(c(5.40, 5.50, 5.55,
                           5.85, 5.70, 5.75,
                           5.20, 5.60, 5.50,
                           5.55, 5.50, 5.40,
                           5.90, 5.85, 5.70,
                           5.45, 5.55, 5.60,
                           5.40, 5.40, 5.35,
                           5.45, 5.50, 5.35,
                           5.25, 5.15, 5.00,
                           5.85, 5.80, 5.70,
                           5.25, 5.20, 5.10,
                           5.65, 5.55, 5.45,
                           5.60, 5.35, 5.45,
                           5.05, 5.00, 4.95,
                           5.50, 5.50, 5.40,
                           5.45, 5.55, 5.50,
                           5.55, 5.55, 5.35,
                           5.45, 5.50, 5.55,
                           5.50, 5.45, 5.25,
                           5.65, 5.60, 5.40,
                           5.70, 5.65, 5.55,
                           6.30, 6.30, 6.25),
                         nrow = 22,
                         byrow = TRUE,
                         dimnames = list(1 : 22,
                                         c("Round Out", "Narrow Angle", "Wide Angle")))
flv <- 1:6
FLenv <- as.FL(Renv)

Renv$wb <- aggregate(warpbreaks$breaks,
                  by = list(w = warpbreaks$wool,
                            t = warpbreaks$tension),
                  FUN = mean)

FLenv$wb <- as.FLTable(Renv$wb,
                    tableName="ARBaseTestTempTable",
                    drop=TRUE)

test_that("Friedman Test on FLMatrix: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit <- friedman.test(RoundingTimes)
            # fit$p.value <- NULL
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})

test_that("Friedman Test on FLVectors: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit <- friedman.test(wb$x,wb$w,wb$t)
            # fit$p.value <- NULL
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})


Renv$fit <- stats::friedman.test(x~w|t, data = Renv$wb)
FLenv$fit <- friedman.test(x~w|t, data = FLenv$wb)

test_that("Friedman Test on FLTable: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})

## Replicate same data and then subset
Renv$wb <- rbind(Renv$wb,Renv$wb)
FLenv$wb <- as.FLTable(Renv$wb,
                       tableName="ARBaseTestTempTable",
                       drop=TRUE)

Renv$fit <- stats::friedman.test(x~w|t, data = Renv$wb, subset=flv)
FLenv$fit <- friedman.test(x~w|t, data = FLenv$wb, subset=flv)

test_that("Friedman Test on FLTable: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit$data.name <- NULL
            class(fit) <- "list"
    },Renv,FLenv,
    expectation=c("fit"))
})
