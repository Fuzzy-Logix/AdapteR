Renv<-new.env(parent = globalenv())

Renv$mat1 <- cbind(1, 0, 1:3)
#sparse matrix (15*15 dsc matrix)
Renv$mat3 = kronecker(diag(x=c(100,1,10)), Hilbert(5))
#Large sparse matrix
n <- 250000; p <- 33; nnz <- 1000

FLenv <- as.FL(Renv)


# Testing rankMatrix
test_that("check rankMatrix result",{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::rankMatrix,
                      Matrix::rankMatrix,
                      n=5)
})

##phani: sparseMatrices not working on Aster
test_that("rankMatrix on sparseMatrix",{
    result = eval_expect_equal({
        test1 <- rankMatrix(mat1)
    },Renv,FLenv,
    expectation="test1",
    check.attributes=FALSE,
    platforms="TD")
})




## does not apply, rankMatrix does not require tol Argument
## test_that("rankMatrix: tol argument",{
##     result = eval_expect_equal({   
##         rMQL <- function(ex, M) rankMatrix(M, method="qrLINPACK",tol = 10^-ex)
##         rMQR <- function(ex, M) rankMatrix(M, method="qr.R",     tol = 10^-ex)
##         test3 = sapply(5:15, rMQL, M = 1000 * mat1) # not identical unfortunately
##         test4 = sapply(5:15, rMQR, M = mat1)
##         test5 = sapply(5:15, rMQR, M = 1000 * mat1)
##     },Renv,FLenv,
##     expectation=paste0("test",3:5),
##     check.attributes=FALSE)
## })
