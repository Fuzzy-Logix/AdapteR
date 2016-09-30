#No signature for function in as.FL.Thus, defined function in environment other than R environment.
# Asana Ticket = https://app.asana.com/0/143316600934101/144942913968262
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h9 <- hilbert(9)

Renv = new.env(parent = globalenv())

Renv$mat1 = h9
Renv$mat2 = 1:9/10
Renv$mat3 = matrix(runif(12), 4)
Renv$mat4 =  1:4
Renv$mat5 = matrix(runif(12), 3)
Renv$mat6 = 1:3

Renv$mat7 <- matrix(rnorm(25),5)
Renv$mat8 <- matrix(rnorm(25),5)

FLenv <- as.FL(Renv)

## rankMatrix output may not match as noted here -
## https://app.asana.com/0/143316600934101/144942913968279
test_that("qr: rank from qr Decomposition function with tol",{
    result = eval_expect_equal({
        test2 = qr(mat1,tol = 1e-10)$rank
    },Renv,FLenv)
})


## todo gk: how can we compute: pivot part of r result
test_that("qr: support of rank, pivot, qraux, qr",{
    eval_expect_equal({
        qrmat7 <- qr(mat7)
        m1rank <- qrmat7$rank
        m1pivot <- qrmat7$pivot
        m1qraux <- length(qrmat7$qraux)
        m1qr <- dim(qrmat7$qr)
    },Renv,FLenv,
    noexpectation="qrmat7",
    expectation = c("m1rank","m1pivot","m1qraux","m1qr"),
    verbose=F)
})

test_that("qr.fitted: low precision tol=1e-7",{
    result = eval_expect_equal({
        qrfitted = qr.fitted(qrmat7,mat8)
    }, Renv,FLenv,
    verbose=F,
    expectation = "qrfitted",
    tolerance=1e-7)
})

test_that("qr.qy, qr.qty",{
    result = eval_expect_equal({
        ## dim, because value tests fail because of QR not identified
        qrqy = dim(qr.qy(qrmat7,mat8))
        qrqty = dim(qr.qty(qrmat7,mat8))
    },Renv,FLenv,
    expectation = c("qrqy","qrqty"))
})


## rankMatrix output may not match as noted here -
## https://app.asana.com/0/143316600934101/144942913968279
test_that("qr: rank from qr Decomposition function with tol",{
    result = eval_expect_equal({
        m1rank = qr(mat1,tol = 1e-10)$rank
    },Renv,FLenv,
    expectation = "m1rank",
    verbose=F)
})

test_that("qr.solve: low precision 1e-5",{
    result = eval_expect_equal({
        qrsolve = qr.solve(mat7,mat8)
    }, Renv,FLenv,
    verbose=F,
    expectation="qrsolve",
    tolerance=1e-5)
})

test_that("qr.Q, qr.R, cross product: low precision 1e-7",{
    result = eval_expect_equal({
        m7Q = qr.Q(qrmat7)
        m7R <- qr.R(qrmat7)
        QR <- m7Q %*% m7R
    },Renv,FLenv,
    noexpectation=c("m7Q","m7R"),
    expectation=c("QR"),
    tolerance=1e-7,
    verbose=F)
})

test_that("qr.resid",{
    result = eval_expect_equal({
        qrresid = qr.resid(qrmat7,mat8)
    },Renv,FLenv)
})

test_that("qr: Check Q is orthogonal ",{
    result = eval_expect_equal({
        tQ <- t(qr.Q(qrmat7))
        sQ <- solve(qr.Q(qrmat7))
        diff <- tQ-sQ
    },Renv,FLenv,
    noexpectation=c("tQ","sQ"),
    expectation = "diff",
    verbose=F)
    ##print(result)
})

## todo: double check
test_that("Check R is upper-triangular ",{
    result = eval_expect_equal({
        vtemp1 <- as.matrix(qr.R(qrmat7))
        vtemp2 <- vtemp1[lower.tri(vtemp1)]
        FLexpect_equal(rep(0,length(vtemp2))
                      ,vtemp2)
        rm(vtemp1)
        rm(vtemp2)
    },Renv,FLenv)
})

test_that("qr.coef: low precision tolerance=1e-6",{
    result = eval_expect_equal({
        qrcoefff = qr.coef(qrmat7,mat8)
    }, Renv,FLenv,
    verbose=F,
    tolerance=1e-6)
})
