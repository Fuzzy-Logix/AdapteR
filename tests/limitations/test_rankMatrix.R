Renv<-new.env(parent = globalenv())

Renv$mat1 <- cbind(1, 0, 1:3)
Renv$mat2 =  Hilbert(12)
#sparse matrix (15*15 dsc matrix)
Renv$mat3 = kronecker(diag(x=c(100,1,10)), Hilbert(5))
#Large sparse matrix
n <- 250000; p <- 33; nnz <- 1000
Renv$largeSparseM = sparseMatrix(i = sample.int(n, nnz, replace=TRUE),
                                 j = sample.int(p, nnz, replace=TRUE),
                                 x = rnorm(nnz))

FLenv <- as.FL(Renv)

test_that("rankMatrix: attributes of retval",{
    result = eval_expect_equal({
        test1 <- rankMatrix(mat1)
    },Renv,FLenv,
    expectation="test1",
    check.attributes=TRUE)
})

test_that("rankMatrix: different methods, https://app.asana.com/0/143316600934101/144942913968279",{
    result = eval_expect_equal({
        test6 = rankMatrix(mat3,method = "qr")
        test7 = rankMatrix(mat3,method = "qr.R")
        test8 = rankMatrix(mat3,method = "qrLINPACK")
        testUseGrad = rankMatrix(mat3,method = "useGrad")
        test10 = rankMatrix(mat3,method = "maybeGrad")
        test11 = rankMatrix(mat3,method = "tolNorm2")
    },Renv,FLenv,
    expectation=c("testUseGrad"),
    check.attributes=FALSE)
})


test_that("rankMatrix: large Sparse matrices",{
    result = eval_expect_equal({
        test12 = rankMatrix(largeSparseM,method="qr")
    },Renv,FLenv)
})


test_that("t: masking is not breaking Matrix::t",{
    ##Case of R which failed for a time period
    ## Not working for R also. 
    set.seed(42)
    f1 <- factor(sample(50, 1000, replace=TRUE))
    f2 <- factor(sample(50, 1000, replace=TRUE))
    f3 <- factor(sample(50, 1000, replace=TRUE))
    rbind. <- if(getRversion() < "3.2.0") rBind else rbind
    Renv$mat148 = t(do.call(rbind., lapply(list(f1,f2,f3), as, 'sparseMatrix')))
    FLenv$mat148 <- as.FL(Renv$mat148)
})

test_that("rankMatrix: rank 148 example",{
    ## rankMatrix(crossprod(mat148),method='qr') == 148)
    expect_equal(
        rankMatrix(FLenv$mat148,method='qr'),148)
    expect_equal(
        rankMatrix(crossprod(FLenv$mat148),method='qr'),148)
})


