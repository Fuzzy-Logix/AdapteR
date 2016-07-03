Renv<-new.env(parent = globalenv())

Renv$mat1 <- cbind(1, 0, 1:3)

test_that("Check for rankMatrix function",{
    result = eval_expect_equal({test1 = rankMatrix(mat1)},Renv)
    print(result)
    })

#dpoMatrix

#(meths <- eval(formals(rankMatrix)$method))
# a "border" case:
#H12 <- Hilbert(12)
#rankMatrix(H12, tol = 1e-20) # 12;  but  11  with default method & tol.
#sapply(meths, function(.m.) rankMatrix(H12, method = .m.))
#methods = tolNorm2    qr   qr.R  qrLINPACK  useGrad maybeGrad


#No inherited method for dpoMatrix in as.FLMatrix and as.FL function.
#Asana ticket - https://app.asana.com/0/143316600934101/144942913968255
#Resolved.
Renv$mat1 =  Hilbert(12)

test_that("Check for rankMatrix function",{
    result = eval_expect_equal({test2 = rankMatrix(mat1,tol =1e-20)},Renv)
    print(result)
    })

#No inherited method for signature function in as.FL function.
test_that("sapply using custom functions for rank Matrix: https://app.asana.com/0/143316600934101/144942913968262"),{
    result = eval_expect_equal({   
        rMQL <- function(ex, M) rankMatrix(M, method="qrLINPACK",tol = 10^-ex)
        rMQR <- function(ex, M) rankMatrix(M, method="qr.R",     tol = 10^-ex)
        test3 = sapply(5:15, rMQL, M = 1000 * mat1) # not identical unfortunately
        test4 = sapply(5:15, rMQR, M = mat1)
        test5 = sapply(5:15, rMQR, M = 1000 * mat1)
    },Renv)
    print(result)
})


#sparse matrix (15*15 dsc matrix)
#No inherited method for dSCMatrix.
#Asana Ticket - https://app.asana.com/0/143316600934101/144942913968269
#resolved
Renv$mat1 = kronecker(diag(x=c(100,1,10)), Hilbert(5))

#test_failed
#FL rankMatrix function is not able to give different results as expected in R for different methods.
#Asana ticket = https://app.asana.com/0/143316600934101/144942913968279
test_that("Check for rankMatrix function",{
    result = eval_expect_equal({
        test6 = rankMatrix(mat1,method = "qr")
        test7 = rankMatrix(mat1,method = "qr.R")
        test8 = rankMatrix(mat1,method = "qrLINPACK")
        test9 = rankMatrix(mat1,method = "useGrad")
        test10 = rankMatrix(mat1,method = "maybeGrad")
        test11 = rankMatrix(mat1,method = "tolNorm2")
    },Renv)
    print(result)
})

#Large sparse matrix
#No inherited method for dgCMatrix in as.Fl
#Asana Ticket - https://app.asana.com/0/143316600934101/144942913968285
#resolved
n <- 250000; p <- 33; nnz <- 10000
Renv$mat1 = sparseMatrix(i = sample.int(n, nnz, replace=TRUE),
                  j = sample.int(p, nnz, replace=TRUE), x = rnorm(nnz))

test_that("Check for rankMatrix function",{
    result = eval_expect_equal({
        test12 = rankMatrix(mat1)
        test13 = rankMatrix(mat1,method =qr)
    },Renv)
    print(result)
})


#Case of R which failed for a time period
# Not working for R also. 
set.seed(42)
f1 <- factor(sample(50, 1000, replace=TRUE))
f2 <- factor(sample(50, 1000, replace=TRUE))
f3 <- factor(sample(50, 1000, replace=TRUE))
rbind. <- if(getRversion() < "3.2.0") rBind else rbind
Renv$mat1 = t(do.call(rbind., lapply(list(f1,f2,f3), as, 'sparseMatrix')))

test_that("Check for rankMatrix function",{
    result = eval_expect_equal({
        ## test14 = stopifnot(rankMatrix(mat1,method='qr') == 148,
        ##                    rankMatrix(crossprod(mat1),method='qr') == 148)
        expect_equal(rankMatrix(mat1,method='qr'),148)
        expect_equal(rankMatrix(crossprod(mat1),method='qr'),148)
    },Renv,FLenv)
    print(result)
})
