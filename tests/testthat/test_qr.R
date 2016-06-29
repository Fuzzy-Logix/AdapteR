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

## todo gk: how can we compute: pivot part of r result
Renv$qrmat7 <- qr(Renv$mat7)
FLenv$qrmat7 <- qr(FLenv$mat7)


## rankMatrix output may not match as noted here -
## https://app.asana.com/0/143316600934101/144942913968279
test_that("Check rank from qr Decomposition function with tol",{
    result = eval_expect_equal({test2 = qr(mat1,tol = 1e-10)$rank
                                },Renv,FLenv)
    ##print(result)
    })

## qr.solve(A,b) solves AX=b in the following way:-
## A=QR => X=inv(t(A)A)%*%t(A)%*%b => inv(R)%*%t(Q)%*%b = X
## In short , solve(qr.R(qr(m1)))%*%t(qr.Q(qr(m1)))%*%m2 == qr.solve(m1,m2)
## and qr.solve(m)==solve(m)
## and qr.coef(qr(m1),m2)==solve(m1)%*%m2

test_that("Check qr.solve and qr.coef ",{
    result = eval_expect_equal({
        qrcoefff = qr.coef(qrmat7,mat8)
        qrsolve = qr.solve(mat7,mat8)
        qrfitted = qr.fitted(qrmat7,mat8)
        },Renv,FLenv,check.attributes=FALSE)
    ##print(result)
    })


test_that("Check QR gives same matrix in R,FL ",{
    result = eval_expect_equal({
        Qmat7 = qr.Q(qrmat7) %*% qr.R(qrmat7)
        qrresid = qr.resid(qrmat7,mat8)
        },Renv,FLenv)
    ##print(result)
    })

test_that("Check Q is orthogonal ",{
    result = eval_expect_equal({
        FLexpect_equal(t(qr.Q(qrmat7))
            ,solve(qr.Q(qrmat7)))
        },Renv,FLenv)
    ##print(result)
    })

test_that("Check R is upper-triangular ",{
    result = eval_expect_equal({
        vtemp1 <- as.matrix(qr.R(qrmat7))
        vtemp2 <- vtemp1[lower.tri(vtemp1)]
        FLexpect_equal(rep(0,length(vtemp2))
            ,vtemp2)
        rm(vtemp1)
        rm(vtemp2)
        },Renv,FLenv)
    ##print(result)
    })

## These fail because of different R and FL outputs for
## R and Q matrices
test_that("Check qr.qy, qr.qty, qr.resid working ",{
    result = eval_expect_equal({
        qrqy = qr.qy(qrmat7,mat8)
        qrqty = qr.qty(qrmat7,mat8)
        },Renv,FLenv)
    ##print(result)
    })

## not proper choice of mat1,mat2.. fails in R without tol.
#Test Failed
#Needs different arguments needed as first argument for R and FL qr.solve.
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968316
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test3 = qr.solve(mat1,mat2,tol = 1e-10)
                                },Renv,FLenv)
    ##print(result)
    })

#Test Failed
#Needs different arguments needed as first argument for R and FL qr.solve.
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968316
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test4 = qr.coef(qr(mat1,tol = 1e-10), mat2)
                                },Renv,FLenv)
    ##print(result)
    })

#Test failed.
#Overdetermined function.
#solve needs a numeric matrix as first argument in FL solve function.
#Asana Ticket = https://app.asana.com/0/143316600934101/144942913968316
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test5 = solve(qr(mat3),mat4)
                                },Renv,FLenv)
    ##print(result)
    })

#underdetermined function
test_that("Check for qr Decomposition function ",{
    result = eval_expect_equal({test6 = solve(mat5,mat6)
                                },Renv,FLenv)
    ##print(result)
    })
