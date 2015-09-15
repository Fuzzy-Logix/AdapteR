## assume m is assigned to be a in-database sparse Matrix object
require(Matrix)
m <- Diagonal(10)
colnames(m) <- c("a","b","c",4:10)
rownames(m) <- c("a","b","c",4:10)

### TODO: insert code here that can save the Matrix m into Teradata as a deep matrix.

### TODO: insert code here that replaces Matrix class with DBLytix in-database sparse matrix class.


require(testthat)

test_that("get dimensions of a Matrix",
{
    expect_true(
        is.numeric(
            ncol(m)
        ))
    expect_true(
        is.numeric(
            nrow(m)
        ))
})

test_that("get column and row names",
{
    expect_true(
        is.character(
            colnames(m)
        ))
    expect_true(
        is.character(
            rownames(m)
        ))
})




test_that("selection of cell row 1, column 1",
{
    expect_true(
        is.numeric(
            m[1,1]
        ))
    expect_true(
        length(m[1,1])==1
    )
})

test_that("selection of first row",
{
    expect_true(
        is.numeric(
            m[1,]
        ))
    expect_true(
        length(m[1,])==ncol(m))
})

test_that("selection of row by name works",
          expect_equal(
              m[1,],m[rownames(m)[[1]],]))

test_that("selection of column by name works",
          expect_equal(
              m[,1],m[,colnames(m)[[1]]]))

test_that("selection of a part of the matrix",
{
    expect_equal(3,
                 nrow(m[1:3,]))
    expect_equal(ncol(m),
                 ncol(m[1:3,]))
})


test_that("selection of a part of the matrix",
{
    expect_equal(3,
                 ncol(m[,1:3]))
    expect_equal(nrow(m),
                 nrow(m[,1:3]))
    expect_equal(rownames(m),
                 rownames(m[,1:3]))
})

test_that("selection of row by name works",
          expect_equal(
              m[1:2,],m[rownames(m)[1:2],]
          ))

test_that("selection of column by name works",
          expect_equal(
              m[,1:2],m[,colnames(m)[1:2]]
          ))


m2 <- matrix(1:50,nrow=10)
test_that("Transpose", {
    expect_equal(c(5,10),dim(t(m2)))
    expect_equal(as.vector(m2),as.vector(t(t(m2))))
})

test_that("Scalar Multiplication and Addition", {
    expect_equal(c(10,5),dim(m2 + m2))
    expect_equal(as.vector(m2 + m2),as.vector(2 * m2))
    expect_equal(c(10,5),dim(m2 - m2))
    expect_equal(as.vector(m2 + m2 - m2),as.vector(m2))
})

test_that("Matrix Multiplication", {
    expect_equal(c(10,5),dim(m2 * m2))
    expect_equal(as.vector(m2 * m2),as.vector(m2) * as.vector(m2))

    expect_equal(c(10,5),dim(m %*% m2))
    expect_equal(as.vector(m2),as.vector(m %*% m2))
})


test_that("Pseudo inverse is computed" {
    expect_equal(c(10,10),dim(ginv(m)))
    ## use FLMatrixPseudoInvUdt
})

## Also needed:
##      ## S4 method for signature 'CsparseMatrix,diagonalMatrix'
##      x %*% y
     
##      ## S4 method for signature 'dgeMatrix,missing'
##      crossprod(x, y = NULL, boolArith = NA, ...)
##      ## S4 method for signature 'CsparseMatrix,diagonalMatrix'
##      crossprod(x, y = NULL, boolArith = NA, ...)
##             ## .... and for many more signatures
     
##      ## S4 method for signature 'CsparseMatrix,ddenseMatrix'
##      tcrossprod(x, y = NULL, boolArith = NA, ...)
##      ## S4 method for signature 'TsparseMatrix,missing'
##      tcrossprod(x, y = NULL, boolArith = NA, ...)
##             ## .... and for many more signatures
