
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
Renv$flv <- 1:6
FLenv <- as.FL(Renv)

Renv$wb <- aggregate(warpbreaks$breaks,
                  by = list(w = warpbreaks$wool,
                            t = warpbreaks$tension),
                  FUN = mean)

FLenv$wb <- as.FLTable(Renv$wb)

test_that("Friedman Test on FLMatrix: R example: checking Result Equality without data.name:",{
    result = eval_expect_equal({
            fit <- friedman.test(RoundingTimes)
    },Renv,FLenv,
    expectation=c("fit"))
})

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

## ## Replicate same data and then subset
## Renv$wb <- rbind(Renv$wb,Renv$wb)
## FLenv$wb <- as.FLTable(Renv$wb,
##                        tableName="ARBaseTestTempTable",
##                        drop=TRUE)

## test_that("Friedman Test on FLTable: R example: checking Result Equality without data.name:",{
##     result = eval_expect_equal({
##             fit <- friedman.test(x~w|t, data = wb, subset=flv)
##             fit$data.name <- NULL
##             class(fit) <- "list"
##     },Renv,FLenv,
##     verbose=F,
##     expectation=c("fit"))
## })

fltMD <- FLTableMD("tblFriedmanTest","datasetid","obsid","groupid","num_val")

data1 <- FLTable("tblFriedmanTest","obsid","groupid","num_val", "datasetid = 1")
data2 <- FLTable("tblFriedmanTest", "obsid","groupid","num_val", "datasetid = 2")
d1 <- as.matrix(data1)
d2 <- as.matrix(data2)

test_that("Friedman Test : DBLytix Example",{
    res <- friedman.test(data = fltMD)
    R_res1 <- friedman.test(d1)
    R_res2 <- friedman.test(d2)
    expect_equal(res[[1]]$statistic, R_res1$statistic)
    expect_equal(res[[1]]$p.value, R_res1$p.value)
    expect_equal(res[[2]]$statistic, R_res2$statistic)
    expect_equal(res[[2]]$p.value, R_res2$p.value)
})
