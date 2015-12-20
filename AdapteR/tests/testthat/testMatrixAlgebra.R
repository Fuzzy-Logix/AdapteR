## Hi Phani, Manish,
##
## I implemented further in a unified system of selects classes, and
## did change quite a bit in FLMatrix. Look at the constructor.
## 
## Not everything is working, please investigate 
## by running from here and looking at FLMatrix and
## involved functions.
##
## Your next steps:
## 1. investigate addition/multiplication, all tests fail
##
## My next steps:
## 1. design conditions in cbind
## 2. will we need FLVector, FLTable with the new classes?


## A remote matrix is easily created by specifying 
eqnRtn <- FLMatrix(
    connection,
    database          = "FL_DEMO",
    matrix_table      = "finEquityReturns",
    matrix_id_value   = "",
    matrix_id_colname = "",
    row_id_colname    = "TxnDate",
    col_id_colname    = "TickerSymbol",
    cell_val_colname  = "EquityReturn")


m <- eqnRtn[1:10,"MSFT"]
m2 <- eqnRtn[1:10,"ORCL"]

m
m2


require(testthat)
options(debugSQL=TRUE)
## Testing Subsetting
## Non-symmetric singular matrix of dimension 5x5
## in R memory
rMatrix <- matrix(1:25,5)
rMatrix
dim(rMatrix)
diag(rMatrix)

## converting the R matrix into 
## an in-DB object,
## CAREFUL: DATA IS TRANSFERED THROUGH NETWORK
m <- as.FLMatrix(rMatrix,connection)
dim(m)
m
rMatrix
##diag(m)

########################################
## STOP RUNNING HERE, below 





## gk: I do not know why this is now broken.  please fix
test_that("selections of submatrix by index",
{
    expect_equal(as.matrix(m),rMatrix)
    expect_equal(as.matrix(m[1,]),rMatrix[1,])
    expect_equal(as.matrix(m[2:3,4:5]),rMatrix[2:3,4:5])
})

test_that("selection of row by name ",{
    expect_equal(m[1,],
                 m[rownames(m)[[1]],])
    expect_equal(as.matrix(m[1,]),as.matrix(m[rownames(m)[[1]],]))
})


test_that("selection of column by name works",
          expect_equal(m[,1], m[,colnames(m)[[1]]]))


test_that("selection of row by name works",
          expect_equal(m[1:2,],
                       m[rownames(m)[1:2],]))

test_that("selection of column by name works",
          expect_equal(m[,1:2],
                       m[,colnames(m)[1:2]]))


## gk: these are not working, please fix!
                                        # Testing Addition/Subtraction
test_that("check matrix addition/subtraction",
{
    expect_equal(as.matrix(m2 <- .5*m),
                 as.matrix(.5*m))
    expect_equal(as.matrix(m + m2),
                 as.matrix(m)+as.matrix(m2))
    expect_equal(as.matrix(m - m2),
                 as.matrix(m)-as.matrix(m2))
})


test_that("check sparse matrix addition/subtraction",
{
    sm2 + sm2
    sm1 - sm1 
})


test_that("check sum of matrices",
          expect_equal(as.matrix(m + m), as.matrix(2*m)))

## gk:  the fate of sparse matrices?
test_that("check sum of sparse matrices",
          expect_equal(sm2 + sm2, 2*sm2))

test_that("check commutative law",
          expect_equal((m2 + m),(m + m2)))

test_that("check associative law for addition",
          expect_equal(
          ((m + m) + m2),(m + (m + m2))))

test_that("check identity of addition",
          expect_equal(
          (m + m0), (m0 + m)))

                                        # Testing Multiplication
test_that("check matrix multiplication",
{
    m * m2
    m3 %*% m2
    m %*% m2 
})

test_that("check associative law for multiplication",
          expect_equal((m3 %*% m) %*% m2),
          (m3 %*% (m %*% m2)))

test_that("check two scalar multiplication for matrices",
          expect_equal(((2 * 3) * m),
          (2 * (3 * m))))

test_that("check two scalar multiplication for sparse matrices",
          expect_equal(((2 * 3) * sm2),
          (2 * (3 * sm2))))

test_that("check distributive properties for matrices",
{
    expect_equal((m3 %*% (m2 + m)),
    ((m3 %*% m2) + (m3 %*% m)))
    expect_equal(((m2 + m) %*% m),
    ((m2 %*% m) + (m %*% m)))
    expect_equal((2 * (m2 + m)),
    ((2 * m2) + (2 * m)))  
    expect_equal(((2 + 3) *m),
    ((2 * m) + (3 * m)))
})


############################################################ 
##  Vectors


test_that("selection of entire vector",
{
    expect_true(
        is.FLVector(
            v1[]
        ))
    expect_true(length(v1[])==length(v1))
})

test_that("selection of part of vector",
{
    expect_true(
        is.FLVector(
            v1[2:3]
        ))
    expect_true(length(v1[2:3])==2)
})

test_that("selection of an element from a vector",
{
    expect_true(
        is.FLVector(
            v1[2]
        ))
    expect_true(length(v1[2])==1)
})

test_that("check vector addition/subtraction",
{
    v1 + v1
    v2 - v2 
})

test_that("check sum of vectors",
          expect_equal(
          ((v2 + v2) == 2*v2),
          TRUE
          ))



test_that("check two scalar multiplication for vectors",
          expect_equal(
          ( ((2 * 3) * v1) == (2 * (3 * v1)) ),
          TRUE
          ))

test_that("check scalar multiplication for matrices",
          expect_equal(
          ( ((2 * m) %*% m2) == (2 * (m %*% m2)) ),
          TRUE
          ))
