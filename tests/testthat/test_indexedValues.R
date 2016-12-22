

Renv = new.env(parent = globalenv())
FLenv <- as.FL(Renv)
Renv$N <- 10000000
FLenv$N <- FLSerial(1,Renv$N)

test_that("FLSimpleVector: usage of random number vectors and aggregates",{
    eval_expect_equal({
        rvec <- rnorm(N,0,1)
        mean <- mean(rvec)
    }, Renv,FLenv,
    tolerance=10/sqrt(Renv$N), ## SE of the mean, 95% success of test
    noexpectation="rvec")
})



## never use slot access to @dimColumns like:
## rvec@dimColumns[[1]]
##
## instead use to get name of sql column for row indices
## getIndexSQLName(rvec,1)
test_that("FLSimpleVector: getting SQL value expression and name",{
    N <- FLSerial(1,10)
    expect_equal(
        getValueSQLExpression(N), "fzzlSerial.serialVal")
    expect_equal(
        getValueSQLName(N), "serialVal")
    ##
    rvec <- runif(N,-1,1)
    expect_true(
        grepl("FLSim.*\\(fzzlSerial.serialVal",getValueSQLExpression(rvec)))
    expect_equal(
        getValueSQLName(rvec), "r_runif")
})


test_that("FLSimpleVector: getting SQL index expression and name",{
    N <- FLSerial(1,10)
    expect_equal(length(dim(N)), 1)
    expect_equal(dim(N), 10)
    expect_equal(length(N), 10)
    expect_equal(
        getIndexSQLExpression(N,1), "fzzlSerial.serialVal")
    expect_equal(
        getIndexSQLName(N,1), "serialVal")
    rvec <- runif(N,-1,1)
    expect_equal(
        getIndexSQLExpression(rvec,1), "serialVal")
    expect_equal(
        getIndexSQLName(rvec,1), "serialVal")
})



test_that("FLSimpleVector: no storage of values required for function expressions",{
    rvec <- runif(FLSerial(0,10),-1,1)
    mean <- mean(rvec)
    ##ervec <- exp(rvec)
})
