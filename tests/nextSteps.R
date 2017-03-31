require(plyr)
library(AdapteR)
library(testthat)
require(Matrix)
require(reshape2)
require(psych)
require(MASS)
library(RJDBC)
##library(RODBC)

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
                            database = "Fl_demo",
                            dir.jdbcjars = yourJarDir)
}

options(debugSQL=FALSE)


test_that("failing if object length do not match up",
{
    FLexpect_equal((M1$FL-V2),M1$R-V2R,check.attributes=FALSE)
  FLexpect_equal((M1$FL-P1$FL),M1$R-P1$R,check.attributes=FALSE)
  FLexpect_equal((V1-M2),V1R-M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL-M2),P1$R-M2R,check.attributes=FALSE)
  FLexpect_equal((P1$FL-P1$FL-V1-V2-M2-P1$FL-M1$FL-V2),
               P1$R-P1$R-V1R-V2R-M2R-P1$R-M1$R-V2R,
               check.attributes=FALSE)
})


#################################################################################
################### Functions work but output slightly differs ##################
###################### from corresponding R functions ###########################

## gk: discuss with Raman for JIRA DBlytix
## Testing FLEigen
## results differ in Teradata and R
test_that("check FLEigen",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::eigen,
                      base::eigen,
                      n=5,
                      isSquare=TRUE)
})

## Testing FLSVDecomp
## results differ in Teradata and R
test_that("check Singular Value Decomposition",
{
    expect_eval_equal(initF.FLMatrix,AdapteR::svd,base::svd,n=5)
})

## Testing FLDet
### for some matrices R output = -(DBLytix output)
test_that("check determinant result",{
    expect_eval_equal(initF.FLMatrix,AdapteR::det,base::det,n=5,isSquare=TRUE)
})

## Testing FLQRDecomposition
## output qr matrix differs from R
## todo gk: how can we compute: pivot part of r result
test_that("check FLQRDecomposition",
{
  M <- initF.FLMatrix(n=5)
    expect_eval_equal(initF.FLMatrix,AdapteR::qr,base::qr,n=5)
})



#################################################################################
## binding matrices is limited to named matrices and requires unique names
## these tests currently fail

## Testing rbind
test_that("check rbind result",
{
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- initF.FLMatrix(n,isSquare)
        b <- FLMatrix(connection,
                      "FL_DEMO", "tblmatrixMulti",
                      5, "MATRIX_ID",
                      "ROW_ID","COL_ID","CELL_VAL")
        list(R=list(a$R,
                    as.matrix(b)),
             FL=list(a$FL,
                     b))
    },function(x) do.call("rbind",x),
    function(x) do.call("rbind",x),n=6
  )
  ##
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- initF.FLMatrix(n,isSquare)
        b <- initF.FLMatrix(n,isSquare)
        list(R=list(a$R,
                    b$R),
             FL=list(a$FL,
                     b$FL))
    },function(x) do.call("rbind",x),
    function(x) do.call("rbind",x),n=6
  )
})

## Testing cbind
test_that("check cbind result",
{
    expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- initF.FLMatrix(n,isSquare)
        b <- FLMatrix(connection,
                      "FL_DEMO", "tblmatrixMulti",
                      5, "MATRIX_ID",
                      "ROW_ID","COL_ID","CELL_VAL")
        list(R=list(a$R,
                    as.matrix(b)),
             FL=list(a$FL,
                     b))
    },function(x) do.call("cbind",x),
    function(x) do.call("cbind",x),n=5
  )
  expect_eval_equal(initF=function(n,isSquare=FALSE) {
        a <- initF.FLMatrix(n,isSquare)
        b <- initF.FLMatrix(n,isSquare)
        list(R=list(a$R,
                    b$R),
             FL=list(a$FL,
                     b$FL))
    },function(x) do.call("cbind",x),
    function(x) do.call("cbind",x),n=5
  )
})

## Testing both binds together
test_that("check binds result",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,
                "FL_DEMO", "tblmatrixMulti",
                5, "MATRIX_ID",
                "ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(matrix(runif(100,-30,30),10,10),connection)
  M4 <- as.FLMatrix(matrix(runif(75,-30,30),5,15),connection)
  expect_equal(rbind(cbind(rbind(M1$FL,M2),M3),M4),
               rbind(cbind(rbind(M1$R,as.matrix(M2)),as.matrix(M3)),as.matrix(M4)),
               check.attributes=FALSE)
})


## Testing functions on result of binds
test_that("check functions on binds result",
{
  test_that("check transpose on binds result",
  {
    M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
    M2 <- FLMatrix(connection,
                  "FL_DEMO", "tblmatrixMulti",
                  5, "MATRIX_ID",
                  "ROW_ID","COL_ID","CELL_VAL")
    M3 <- as.FLMatrix(matrix(runif(100,-30,30),10,10),connection)
    M4 <- as.FLMatrix(matrix(runif(75,-30,30),5,15),connection)
    expect_equal(t(rbind(cbind(rbind(M1$FL,M2),M3),M4)),
                 t(rbind(cbind(rbind(M1$R,as.matrix(M2)),as.matrix(M3)),as.matrix(M4))),
                 check.attributes=FALSE)
  })

  test_that("check solve on binds result",
  {
    M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
    M2 <- FLMatrix(connection,
                  "FL_DEMO", "tblmatrixMulti",
                  5, "MATRIX_ID",
                  "ROW_ID","COL_ID","CELL_VAL")
    M3 <- as.FLMatrix(matrix(runif(100,-30,30),10,10),connection)
    M4 <- as.FLMatrix(matrix(runif(75,-30,30),5,15),connection)
    expect_equal(solve(rbind(cbind(rbind(M1$FL,M2),M3),M4)),
                 solve(rbind(cbind(rbind(M1$R,as.matrix(M2)),as.matrix(M3)),as.matrix(M4))),
                 check.attributes=FALSE)
  })

  test_that("check M_Subtraction on binds result",
  {
    M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
    M2 <- FLMatrix(connection,
                  "FL_DEMO", "tblmatrixMulti",
                  5, "MATRIX_ID",
                  "ROW_ID","COL_ID","CELL_VAL")
    M3 <- as.FLMatrix(matrix(runif(50,-30,30),10,5),connection)
    M4 <- as.FLMatrix(matrix(runif(50,-30,30),5,10),connection)
    expect_equal(rbind(M1$FL,M2)-M3,
                 rbind(M1$R,as.matrix(M2))-as.matrix(M3),
                 check.attributes=FALSE)
    expect_equal(cbind(M1$FL,M2)-M4,
                 cbind(M1$R,as.matrix(M2))-as.matrix(M4),
                 check.attributes=FALSE)
  })
})

## Testing dimnames of binds result
test_that("check dimnames on binds result",
{
  aR <- matrix(runif(25,-30,30)
              ,5,5
              ,dimnames=list(letters[1:5],1:5))
  aFL <- as.FLMatrix(aR,connection)
  bR <- matrix(runif(25,-30,30)
              ,5,5
              ,dimnames=list(1:5,letters[1:5]))
  bFL <- as.FLMatrix(bR,connection)

  expect_equal(dimnames(rbind(aFL,bFL))
               dimnames(rbind(aR,bR)),
               check.attributes=FALSE)
  expect_equal(dimnames(cbind(aFL,bFL))
               dimnames(cbind(aR,bR)),
               check.attributes=FALSE)

  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,
                "FL_DEMO", "tblmatrixMulti",
                5, "MATRIX_ID",
                "ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(matrix(runif(100,-30,30),10,10),connection)
  M4 <- as.FLMatrix(matrix(runif(75,-30,30),5,15),connection)
  expect_equal(dimnames(rbind(cbind(rbind(M1$FL,M2),M3),M4)),
               dimnames(rbind(cbind(rbind(M1$R,as.matrix(M2)),as.matrix(M3)),as.matrix(M4))),
               check.attributes=FALSE)
})

#***************************************************************************************************
#***************************************************************************************************
#################################################################################
############################# Non Working Tests #################################
