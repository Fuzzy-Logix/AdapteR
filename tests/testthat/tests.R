## The formal test suite.
##
## Tests have been abstracted such that
## function initF.FLMatrix creates equivalent
## matrices (one in R mem, one in-database).
## Then the expect_eval_equal function evaluates
## function in
## a) the R base/Matrix package (eval on client)
## b) the AdapteR package (eval in-database)
## are tested to be equal.
##
## Next, benchmarking will be incorporated in these functions.

library(AdapteR)
library(testthat)


## This script first tries to create a ODBC connection
if(!exists("connection"))
    connection <- flConnect(odbcSource = "Gandalf")


## If ODBC has failed we try to create a JDBC connection
if(!exists("connection")){
    ## set this to add jdbc driver and security jars to classpath:
    ## terajdbc4.jar tdgssconfig.jar
    ## CAVE: fully qualified PATH required
    yourJarDir <- "/Users/gregor/fuzzylogix"
    connection <- flConnect(host     = "10.200.4.116",
                            database = "fuzzylogix",
                            dir.jdbcjars = yourJarDir)
}


options(debugSQL=FALSE)

## Testing FLSolve
test_that("check inverse calculation of matrix", {
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::solve,
                      base::solve,
                      n=5,
                      isSquare=TRUE)
})

options(debugSQL=FALSE)
# Testing rankMatrix
test_that("check rankMatrix result",{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::rankMatrix,
                      Matrix::rankMatrix,
                      n=5)
})

## Testing FLGinv
test_that("check FLGinv",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::ginv,
                      MASS::ginv,
                      n=5)
})

## Testing FLDims
test_that("check FLDims if all elements of a row are zero",
{
  m <- Matrix(c(0,1,0,2),2,sparse=T)
  m <- as(m,"dgCMatrix")
  M <- as.FLMatrix(m)
  T1 <- initF.FLTable(rows=5,cols=5)
  T1R <- as.data.frame(T1)
  expect_equal(AdapteR::dim(M),
               base::dim(m),
               check.attributes=FALSE)
  expect_equal(AdapteR::dim(T1),
               base::dim(T1R),
               check.attributes=FALSE)
})

## Testing FLIs
test_that("check FLIs",
{
    expect_eval_equal(initF.FLMatrix,AdapteR::is.FLMatrix,base::is.matrix,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::is.FLVector,base::is.vector,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::is.FLVector,base::is.vector,n=5,isRowVec=TRUE)
    expect_is(initF.FLTable(rows=5,cols=4),"FLTable")
})

## Testing FLCastFunctions
## gk: these need reviewiing and commenting.
## todo phani, kumar:
## what you cannot comment, please remove
test_that("check FLCastFunctions",
{
  M1 <- initF.FLMatrix(n=5)
  V1 <- as.FLVector(sample(1:100,5))
  V1R <- as.vector(V1)
  P1 <- initF.FLVector(n=5,isRowVec=TRUE)
  T1 <- initF.FLTable(rows=5,cols=5)
    FLexpect_equal(as.vector(M1$FL),as.vector(M1$R),check.attributes=FALSE)
    FLexpect_equal(as.vector(P1$FL),as.vector(P1$R),check.attributes=FALSE)
    FLexpect_equal(as.data.frame(M1$FL),as.data.frame(M1$R),check.attributes=FALSE)
    testthat::expect_equal(as.matrix(P1$FL),as.matrix(P1$R),check.attributes=FALSE)
    testthat::expect_equal(as.matrix(V1),as.matrix(V1R),check.attributes=FALSE)
    FLexpect_equal(as.FLMatrix(M1$R),M1$FL,check.attributes=FALSE)
    FLexpect_equal(P1$FL,as.FLVector(P1$R),check.attributes=FALSE)
    FLexpect_equal(as.matrix(as.FLMatrix(V1)),as.matrix(V1R),check.attributes=FALSE)
    FLexpect_equal(as.matrix(as.FLMatrix(P1$R)),as.matrix(P1$R),check.attributes=FALSE)
    FLexpect_equal(as.vector(as.FLVector(M1$R)),as.vector(M1$R),check.attributes=FALSE)
    FLexpect_equal(as.FLVector(M1$FL),as.vector(M1$R),check.attributes=FALSE)
})

## Testing FLCholskeyDecomp
## needs a hermitian positive definite matrix as input
test_that("check FLCholskeyDecomp",
{
  m4 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  expect_equal(as.matrix(chol(m4)),
               Matrix::chol(as.matrix(m4)))
})

# Testing FLLUDecomp
# Function works but comparision fails 
# because of sparseMatrices in R output
test_that("check LU Decomposition",
{
  m <- initF.FLMatrix(n=5)
  FLexpect_equal(AdapteR::expand(AdapteR::lu(m$FL)),
               Matrix::expand(Matrix::lu(m$R)),check.attributes=FALSE)
})

## Testing FLLength
test_that("check length",
{
  T1 <- initF.FLTable(rows=5,cols=5)
  T1R <- as.data.frame(T1)
    expect_eval_equal(initF.FLMatrix,AdapteR::length,base::length,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::length,base::length,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::length,base::length,n=5,isRowVec=TRUE)
    expect_equal(AdapteR::length(T1),base::length(T1R),check.attributes=FALSE)
})


## Testing FLTrace
test_that("check FLTrace",
{
    expect_eval_equal(initF.FLMatrix,
                    AdapteR::tr,
                    psych::tr,
                    n=5,
                    isSquare=TRUE)
})

##Testing FLDiag
test_that("check the result of the diag of matrix",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::diag,
                      base::diag,
                      n=5)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=5)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=5,isRowVec=TRUE)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=1)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=1,isRowVec=TRUE)
})


## Testing M_Subtraction
test_that("check result for Matrix M_Subtraction",
{
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- initF.FLMatrix(n,isSquare)
      b <- FLMatrix(getOption("ResultDatabaseFL"), "tblmatrixMulti",
                    5, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("-",x)),
  function(x) do.call("-",x),n=5,isSquare=TRUE
  )
})

## Testing M_Subtraction
test_that("check result for M_Subtraction",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti", 5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  FLexpect_equal(M1$FL-M2,M1$R-M2R,check.attributes=FALSE)
  FLexpect_equal(V1-V2,V1R-V2R,check.attributes=FALSE)
  FLexpect_equal(P1$FL-P1$FL,P1$R-P1$R,check.attributes=FALSE)
  FLexpect_equal(V1-P1$FL,V1R-P1$R,check.attributes=FALSE)
  FLexpect_equal(P1$FL-V2,P1$R-V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL-V2),M1$R-V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL-P1$FL),M1$R-P1$R,check.attributes=FALSE)
  FLexpect_equal((V1-M2),V1R-M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL-M2),P1$R-M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL-P1$FL-V1-V2-M2-P1$FL-M1$FL-V2),
               P1$R-P1$R-V1R-V2R-M2R-P1$R-M1$R-V2R,
               check.attributes=FALSE)
})



## Testing M_IntegerDivision. Only 2 FLMatrices
test_that("check result for M_IntegerDivision",
{
  expect_eval_equal(initF=function(n) {
      a <- initF.FLMatrix(n=5,isSquare=TRUE)
      b <- FLMatrix(getOption("ResultDatabaseFL"), "tblmatrixMulti",
                    5, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("%/%",x)),
  function(x) do.call("%/%",x)
  )
})

## Testing M_IntegerDivision
test_that("check result for M_IntegerDivision",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

  FLexpect_equal((M1$FL%/%M2),M1$R%/%M2R,check.attributes=FALSE)
  FLexpect_equal((V1%/%V2),V1R%/%V2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%/%P1$FL),P1$R%/%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%/%P1$FL),V1R%/%P1$R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%/%V2),P1$R%/%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%/%V2),M1$R%/%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%/%P1$FL),M1$R%/%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%/%M2),V1R%/%M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%/%M2),P1$R%/%M2R,check.attributes=FALSE)
})

## Testing M_CrossProduct only two FLMatrices
test_that("check result for M_CrossProduct",
{
  expect_eval_equal(initF=function(n) {
      a <- initF.FLMatrix(n=5)
      b <- FLMatrix(getOption("ResultDatabaseFL"), "tblmatrixMulti",
                    3, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("%*%",x)),
  function(x) do.call("%*%",x)
  )
})

## Testing M_CrossProduct
test_that("check result for M_CrossProduct",
{
  M1 <- initF.FLMatrix(n=5) # 5*4 matrix
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",3,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL") # 4*5 matrix
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,5))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,5))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=5,isRowVec=TRUE)
    FLexpect_equal((M1$FL %*% M2),M1$R%*%M2R,check.attributes=FALSE)
    FLexpect_equal((V1%*%V1),V1R%*%V1R,check.attributes=FALSE)
    FLexpect_equal((P1$FL%*%P1$FL),P1$R%*%P1$R,check.attributes=FALSE)
    FLexpect_equal((V1%*%P1$FL),V1R%*%P1$R,check.attributes=FALSE)
    FLexpect_equal((P1$FL%*%V1),P1$R%*%V1R,check.attributes=FALSE)
    FLexpect_equal((M2%*%V2),M2R%*%V2R,check.attributes=FALSE)
    FLexpect_equal((M2%*%P1$FL),M2R%*%P1$R,check.attributes=FALSE)
    FLexpect_equal((V1%*%M1$FL),V1R%*%M1$R,check.attributes=FALSE)
    FLexpect_equal((P1$FL%*%M1$FL),P1$R%*%M1$R,check.attributes=FALSE)
})

## Testing M_Addition
test_that("check result for Matrix M_Addition",
{
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- initF.FLMatrix(n,isSquare)
      b <- FLMatrix(getOption("ResultDatabaseFL"), "tblmatrixMulti",
                    5, "MATRIX_ID",
                    "ROW_ID","COL_ID","CELL_VAL")
      list(R=list(a$R,
                  as.matrix(b)),
           FL=list(a$FL,
                   b))
  },function(x) (do.call("+",x)),
  function(x) do.call("+",x),n=5,isSquare=TRUE
  )
})

## Testing M_Addition
test_that("check result for M_Addition",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"), "tblmatrixMulti",
                  5, "MATRIX_ID",
                  "ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

  FLexpect_equal(M1$FL+M2,
               M1$R+M2R,
               check.attributes=FALSE)
  FLexpect_equal(V1+V2,
               V1R+V2R,
               check.attributes=FALSE)
  FLexpect_equal(P1$FL+P1$FL,
               P1$R+P1$R,
               check.attributes=FALSE)
  FLexpect_equal(V1+P1$FL,
               V1R+P1$R,
               check.attributes=FALSE)
  FLexpect_equal(P1$FL+V2,
               P1$R+V2R,
               check.attributes=FALSE)
  FLexpect_equal(M1$FL+V2,
               M1$R+V2R,
               check.attributes=FALSE)
  FLexpect_equal(M1$FL+P1$FL,
               M1$R+P1$R,
               check.attributes=FALSE)
  FLexpect_equal(V1+M2,
               V1R+M2R,
               check.attributes=FALSE)
  FLexpect_equal(P1$FL+M2,
               P1$R+M2R,
               check.attributes=FALSE)
  FLexpect_equal(P1$FL+P1$FL+V1+V2+M2+P1$FL+M1$FL+V2,
               P1$R+P1$R+V1R+V2R+M2R+P1$R+M1$R+V2R,
               check.attributes=FALSE)
})

## Testing M_Division
test_that("check result for M_Division",
{
    M1 <- initF.FLMatrix(n=5,
                         isSquare=TRUE)
    M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",
              5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    M2R <- as.matrix(M2)
    V1 <- as.FLVector(sample(1:100,
                             10))
    V1R <- as.vector(V1)
    V2 <- as.FLVector(sample(1:100,
                             10))
    V2R <- as.vector(V2)
    P1 <- initF.FLVector(n=10,
                         isRowVec=TRUE)
    FLexpect_equal((M1$FL/M2),
                 M1$R/M2R,
                 check.attributes=FALSE)
    FLexpect_equal((V1/V2),
                 V1R/V2R,
                 check.attributes=FALSE)
    FLexpect_equal((P1$FL/P1$FL),
                 P1$R/P1$R,
                 check.attributes=FALSE)
    FLexpect_equal((V1/P1$FL),
                 V1R/P1$R,
                 check.attributes=FALSE)
    FLexpect_equal((P1$FL/V2),
                 P1$R/V2R,
                 check.attributes=FALSE)
    FLexpect_equal((M1$FL/V2),
                 M1$R/V2R,
                 check.attributes=FALSE)
    FLexpect_equal((M1$FL/P1$FL),
                 M1$R/P1$R,
                 check.attributes=FALSE)
    FLexpect_equal((V1/M2),
                 V1R/M2R,
                 check.attributes=FALSE)
    FLexpect_equal((P1$FL/M2),
                 P1$R/M2R,
                 check.attributes=FALSE)
})

## Testing M_Multiplication
test_that("check result for M_Multiplication",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal(M1$FL*M2,M1$R*M2R,check.attributes=FALSE)
  FLexpect_equal(V1*V2,V1R*V2R,check.attributes=FALSE)
  FLexpect_equal(P1$FL*P1$FL,P1$R*P1$R,check.attributes=FALSE)
  FLexpect_equal(V1*P1$FL,V1R*P1$R,check.attributes=FALSE)
  FLexpect_equal(P1$FL*V2,P1$R*V2R,check.attributes=FALSE)
  FLexpect_equal(M1$FL*V2,M1$R*V2R,check.attributes=FALSE)
  FLexpect_equal(M1$FL*P1$FL,M1$R*P1$R,check.attributes=FALSE)
  FLexpect_equal(V1*M2,V1R*M2R,check.attributes=FALSE)
  FLexpect_equal(P1$FL*M2,P1$R*M2R,check.attributes=FALSE)
  
  FLexpect_equal(P1$FL*P1$FL*V1*V2*M2*P1$FL*M1$FL*V2,
               P1$R*P1$R*V1R*V2R*M2R*P1$R*M1$R*V2R,
               check.attributes=FALSE)
})

## Testing M_Remainder
test_that("check result for M_Remainder",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal((M1$FL%%M2),M1$R%%M2R,check.attributes=FALSE)
  FLexpect_equal((V1%%V2),V1R%%V2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%%P1$FL),P1$R%%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%%P1$FL),V1R%%P1$R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%%V2),P1$R%%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%%V2),M1$R%%V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL%%P1$FL),M1$R%%P1$R,check.attributes=FALSE)
  FLexpect_equal((V1%%M2),V1R%%M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL%%M2),P1$R%%M2R,check.attributes=FALSE)
})

## Testing Equality
test_that("check result for M_Equality",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(as.matrix(M2))
  M3R <- as.matrix(M2)
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal(M1$FL==M2,(M1$R==M2R),check.attributes=FALSE)
  FLexpect_equal(M1$FL==M1$FL,M1$R==M1$R,check.attributes=FALSE)
  FLexpect_equal(M2==M3,M2R==M3R,check.attributes=FALSE)
  FLexpect_equal(V1==V1R,V1R==V1R,check.attributes=FALSE)
  FLexpect_equal(P1$FL==P1$FL,P1$R==P1$R,check.attributes=FALSE)
  FLexpect_equal(V1==P1$FL,V1R==P1$R,check.attributes=FALSE)
  FLexpect_equal(P1$FL==P1$R,P1$R==P1$R,check.attributes=FALSE)
  ##FLexpect_equal(M1$FL==V2,M1$R==V2R,check.attributes=FALSE)
  ##FLexpect_equal(M1$FL==P1$FL,M1$R==P1$R,check.attributes=FALSE)
  FLexpect_equal(V1==V1,V1R==V1R,check.attributes=FALSE)
  ##FLexpect_equal(P1$FL==M2,P1$R==M2R,check.attributes=FALSE)
})

## Testing FLIdentical
test_that("check result for identical",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(as.matrix(M2))
  M3R <- as.matrix(M2)
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10))
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10))
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  ##
  FLexpect_equal(identical(M1$FL,M2),identical(M1$R,M2R),check.attributes=FALSE)
  FLexpect_equal(identical(M1$FL,M1$FL),identical(M1$R,M1$R),check.attributes=FALSE)
  FLexpect_equal(identical(M2,M3),identical(M2R,M3R),check.attributes=FALSE)
  FLexpect_equal(identical(V1,V1R),identical(V1R,V1R),check.attributes=FALSE)
  FLexpect_equal(identical(P1$FL,P1$FL),identical(P1$R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(V1,P1$FL),identical(V1R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(P1$FL,P1$R),identical(P1$R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(M1$FL,V2),identical(M1$R,V2R),check.attributes=FALSE)
  FLexpect_equal(identical(M1$FL,P1$FL),identical(M1$R,P1$R),check.attributes=FALSE)
  FLexpect_equal(identical(V1,V1),identical(V1R,V1R),check.attributes=FALSE)
  FLexpect_equal(identical(P1$FL,M2),identical(P1$R,M2R),check.attributes=FALSE)
})


## testing M_Subtraction with different length vectors
test_that("check FLVector subtraction",
{
  flt <- FLTable(getOption("ResultDatabaseFL"),"finequityreturns","txndate")
  flv1 <- flt[1:8,"equityreturn"]
  flv <- flt[1:10,"equityreturn"]
  flv1R <- as.vector(flv1)
  flvR <- as.vector(flv)
  FLexpect_equal(flv-flv1,flvR-flv1R,check.attributes=FALSE)
})

## Testing FLTranspose
test_that("check transpose",{
    expect_eval_equal(initF.FLMatrix,AdapteR::t,base::t,n=5)
})

## Testing FLRowMeans
test_that("check rowMeans",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::rowMeans,
                      base::rowMeans,
                      n=5)
})

## Testing FLRowSums
test_that("check rowSums",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::rowSums,
                      base::rowSums,
                      n=5)
})

## Testing FLColMeans
test_that("check colMeans",
{
    expect_eval_equal(initF.FLMatrix,AdapteR::colMeans,base::colMeans,n=5)
})

## Testing FLColSums
test_that("check colSums",
{
    expect_eval_equal(initF.FLMatrix,AdapteR::colSums,base::colSums,n=5)
})

## Testing FLSubsetting
test_that("check vector subsetting",
{
  ## Testing result
  expect_eval_equal(initF.FLVector,
                    function(x) do.call("[",list(x,5:3)),
                    function(x) do.call("[",list(x,5:3)),n=5)
  expect_eval_equal(initF.FLVector,
                    function(x) do.call("[",list(x)),
                    function(x) do.call("[",list(x)),n=5)  
})

## Testing FLCorrel
##Failing because of precision errors.
test_that("check FLCorrel result",
{
  fltDeep <- FLTable(getOption("ResultDatabaseFL"),"tblAbaloneDeep",
                "ObsID","VarID","Num_Val",
                whereconditions=paste0(getOption("ResultDatabaseFL"),".tblAbaloneDeep.ObsID < 21")
  RtDeep <- as.data.frame(fltDeep)
  fltWide <- FLTable(getOption("ResultDatabaseFL"),"tblAbaloneWide",
                "ObsID",whereconditions=paste0(getOption("ResultDatabaseFL"),".tblAbaloneWide.ObsID < 21")
  RtWide <- as.data.frame(fltWide)
  vRow <- initF.FLVector(20,TRUE)
  flvRow <- vRow$FL
  RvRow <- vRow$R
  RvCol <- rnorm(20)
  flvCol <- as.FLVector(RvCol)
  m <- initF.FLMatrix(20)
  flm <- m$FL
  Rm <- m$R
  FLexpect_equal(cor(flm,flm),cor(Rm,Rm),check.attributes=FALSE)
  FLexpect_equal(cor(flvRow,flvRow),cor(RvRow,RvRow),check.attributes=FALSE)
  FLexpect_equal(cor(flvCol,flvCol),cor(RvCol,RvCol),check.attributes=FALSE)
  FLexpect_equal(cor(fltDeep,fltDeep),cor(RtDeep,RtDeep),check.attributes=FALSE)
  FLexpect_equal(cor(flm,flvRow),cor(Rm,RvRow),check.attributes=FALSE)
  FLexpect_equal(cor(flm,flvCol),cor(Rm,RvCol),check.attributes=FALSE)
  FLexpect_equal(cor(flvCol,flvRow),cor(RvCol,RvRow),check.attributes=FALSE)
  FLexpect_equal(cor(flm,fltDeep),cor(Rm,RtDeep),check.attributes=FALSE)
  FLexpect_equal(cor(flvRow,fltDeep),cor(RvRow,RtDeep),check.attributes=FALSE)
  FLexpect_equal(cor(flvCol,fltDeep),cor(RvCol,RtDeep),check.attributes=FALSE)
  cor(fltDeep,fltWide)
  cor(fltWide,fltWide)
  cor(fltWide,fltDeep)
  cor(flm,fltWide)
  cor(flvRow,fltWide)
  cor(flvCol,fltWide)
  })

#################################################################
########### no equivalent R functions to test against ###########
################### but functions work ##########################
## Testing FLSV
test_that("check FLSV working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
    FLexpect_equal(
              length(FLSV(M)),
              nrow(M)
          )
})

## Testing FLHessenDecomp
test_that("check Hessenberg Decomposition",
{
    FLHessen(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})

## Testing FLMatrixRREF
test_that("check FLMatrixRREF working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
  FLexpect_equal(
      dim(FLMatrixRREF(M)),
      dim(M)
  )
  FLexpect_equal(
      dimnames(FLMatrixRREF(M)),
      dimnames(M)
  )
})

## Testing FLMatrixREF
test_that("check FLMatrixREF",
{
    FLMatrixREF(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})

## Testing FLMatrixNorm
test_that("check FLMatrixNorm working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
  FLMatrixNorm(M,3)
})

##Testing FLJordon
### works only with matrices with non-complex
### eigen values. So input taken from DbLytix manual.
test_that("check Jordan Decomposition",
{
  M <- FLMatrix(getOption("ResultDatabaseFL"),"tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    FLJordan(M)
})

## Testing FLSolveExcl
test_that("check FLSolveExcl",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
    FLexpect_equal(dim(FLSolveExcl(M,3)),dim(M)-1)
    FLexpect_equal(dim(FLSolveExcl(M,6)),dim(M))
})

## Testing FLTriDiag
test_that("check FLTriDiag",
{
    FLTriDiag(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})







































































































































































































































































































































































