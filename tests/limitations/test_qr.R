#No signature for function in as.FL.Thus, defined function in environment other than R environment.
# Asana Ticket = https://app.asana.com/0/143316600934101/144942913968262
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv = new.env(parent = globalenv())

Renv$mat1 = hilbert(9)
Renv$mat2 = 1:9/10
Renv$mat3 = matrix(runif(12), 4)
Renv$mat4 =  1:4
Renv$mat5 = matrix(runif(12), 3)
Renv$mat6 = 1:3

Renv$mat7 <- matrix(rnorm(25),5)
Renv$mat8 <- matrix(rnorm(25),5)

FLenv <- as.FL(Renv)

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

## qr.solve(A,b) solves AX=b in the following way:-
## A=QR => X=inv(t(A)A)%*%t(A)%*%b => inv(R)%*%t(Q)%*%b = X
## In short , solve(qr.R(qr(m1)))%*%t(qr.Q(qr(m1)))%*%m2 == qr.solve(m1,m2)
## and qr.solve(m)==solve(m)
## and qr.coef(qr(m1),m2)==solve(m1)%*%m2

test_that("qr.fitted: high precision",{
    result = eval_expect_equal({
        qrfitted = qr.fitted(qrmat7,mat8)
    }, Renv,FLenv,
    verbose=F,
    expectation = "qrfitted")
})

test_that("qr.solve: high precision",{
    result = eval_expect_equal({
        qrsolve = qr.solve(mat7,mat8)
    }, Renv,FLenv,
    verbose=F,
    expectation="qrsolve")
})


test_that("qr.Q, qr.R, cross product: high precision",{
    result = eval_expect_equal({
        m7Q = qr.Q(qrmat7)
        m7R <- qr.R(qrmat7)
        QR <- m7Q %*% m7R
    },Renv,FLenv,
    noexpectation=c("m7Q","m7R"),
    expectation=c("QR"),
    verbose=F)
})

test_that("qr.solve: singular matrix with tol argument",{
    result = eval_expect_equal({
        test3 = qr.solve(mat1,mat2,tol = 1e-10)
    },Renv,FLenv,
    expectation = "test3",
    check.attributes=F)
})

test_that("qr.solve: singular matrix without tol argument",{
    result = eval_expect_equal({
        test3 = qr.solve(mat1,mat2)
    },Renv,FLenv,
    expectation = "test3",
    check.attributes=F)
})

test_that("solve(qr(M1),M2): needs a numeric matrix as first argument",{
    result = eval_expect_equal({
        test5 = solve(qr(mat3),mat4)
    },Renv,FLenv)
})

test_that("solve: underdetermined function, double check: check for expected failure?",{
    result = eval_expect_equal({
        test6 = solve(mat5,mat6)
    },Renv,FLenv)
})
