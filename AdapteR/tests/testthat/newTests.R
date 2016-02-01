require(plyr)
library(AdapteR)
library(testthat)
require(Matrix)
require(reshape2)
require(psych)
require(MASS)
library(RJDBC)
library(RODBC)

if (exists("connection")) {
    dbDisconnect(connection)
    rm(connection)
}
if(!exists("connection")){
    ##connection <- odbcConnect("Gandalf")
    connection <- tdConnect(host,user,passwd,database,"jdbc")
}

FLStartSession(connection)

ignoreDimNames <- TRUE

options(debugSQL=FALSE)

expect_eval_equal <- function(initF,FLcomputationF,RcomputationF,benchmark=FALSE,...)
{
  I <- initF(...)
    expect_equal(FLcomputationF(I$FL),
                 RcomputationF(I$R),
                 check.attributes=FALSE)
}

expect_flequal <- function(a,b,...){
    if(is.list(a))
        for(i in 1:length(a))
            expect_flequal(a[[i]],b[[i]],...)

    expect_equal(a,b,...)
}

## Increase n for increasing length of FLVector.
## If isRowVec=TRUE, rowVector(one observation of all columns) is returned.
initF.FLVector <- function(n,isRowVec=FALSE)
{
  sqlSendUpdate(connection,
                      c(paste0("DROP TABLE FL_DEMO.test_vectortable_AdapteR;"),
                        paste0("CREATE TABLE FL_DEMO.test_vectortable_AdapteR 
                          AS(SELECT 1 AS VECTOR_ID,a.serialval AS VECTOR_INDEX,
                            CAST(RANDOM(0,100) AS FLOAT)AS VECTOR_VALUE  
                          FROM FL_DEMO.fzzlserial a 
                          WHERE a.serialval < ",ifelse(isRowVec,2,n+1),") WITH DATA ")))

  table <- FLTable(connection,
                 "FL_DEMO",
                 "test_vectortable_AdapteR",
                 "VECTOR_INDEX",
                 whereconditions=paste0("FL_DEMO.test_vectortable_AdapteR.VECTOR_ID = 1")
                 )

  if(isRowVec)
  flv <- table[1,base::sample(c("VECTOR_VALUE","VECTOR_INDEX"),n,replace=TRUE)]
  else
  flv <- table[1:n,"VECTOR_VALUE"]

  Rvector <- as.vector(flv)
  return(list(FL=flv,R=Rvector))
}

## Increase the value of n to increase the dimensions of FLMatrix returned.
## Returns n*n or n*(n-1) based on isSquare.
initF.FLMatrix <- function(n,isSquare=FALSE)
{
  sqlSendUpdate(connection,
                      c(paste0("DROP TABLE FL_DEMO.test_matrixtable_AdapteR;"),
                        paste0("CREATE TABLE FL_DEMO.test_matrixtable_AdapteR 
                          AS(SELECT 1 AS MATRIX_ID,a.serialval AS ROW_ID,
                            b.serialval AS COL_ID,CAST(random(0,100) AS FLOAT)AS CELL_VAL 
                          FROM FL_DEMO.fzzlserial a,FL_DEMO.fzzlserial b
                          WHERE a.serialval < ",n+1," and b.serialval < ",ifelse(isSquare,n+1,n),") WITH DATA ")))
  flm <- FLMatrix(connection,
              database          = "FL_DEMO",
              matrix_table      = "test_matrixtable_AdapteR",
              matrix_id_value   = 1,
              matrix_id_colname = "Matrix_ID",
              row_id_colname    = "Row_ID",
              col_id_colname    = "Col_ID",
              cell_val_colname  = "Cell_Val")
  Rmatrix <- as.matrix(flm)
  return(list(FL=flm,R=Rmatrix))
}

initF.FLTable <- function(rows,cols)
{
  WideTable <- FLTable(connection, 
                      "FL_DEMO", 
                      "fzzlserial",
                      "serialval",
                      whereconditions=paste0("FL_DEMO.fzzlserial.serialval<100"))
  return(WideTable[1:rows,base::sample(c("randval","serialval"),cols,replace=TRUE)])
}

setMethod("expect_equal",signature("FLMatrix","matrix"),
          function(object,expected,...) expect_equal(as.matrix(object),expected,...))
setMethod("expect_equal",signature("FLMatrix","FLMatrix"),
          function(object,expected,...) expect_equal(as.matrix(object),as.matrix(expected),...))
setMethod("expect_equal",signature("dgCMatrix","FLMatrix"),
          function(object,expected,...) expect_equal(object,as.matrix(expected),...))

setMethod("expect_equal",signature("FLVector","vector"),
          function(object,expected,...) expect_equal(as.vector(object),expected,...))
setMethod("expect_equal",signature("FLVector","FLVector"),
          function(object,expected,...) expect_equal(as.vector(object),as.vector(expected),...))
setMethod("expect_equal",signature("matrix","matrix"),
          function(object,expected,...) testthat::expect_equal(as.vector(object),as.vector(expected),...))

setMethod("expect_equal",signature("list","list"),
          function(object,expected,...)
              llply(names(object),
                    function(i)
                        expect_equal(object[[i]],expected[[i]],...)))


###############################################################
############# WORKING POSITIVE TEST CASES #####################
###############################################################

## Testing FLSolve
test_that("check inverse calculation of matrix", {
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::solve,
                      base::solve,
                      n=5,
                      isSquare=TRUE)
})

options(debugSQL=TRUE)
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
test_that("check FLDims",
{
  m <- Matrix(c(0,1,0,2),2,sparse=T)
  m <- as(m,"dgCMatrix")
  M <- as.FLMatrix(m,connection)
  T1 <- initF.FLTable(rows=5,cols=5)
  T1R <- as.data.frame(T1)
  expect_equal(AdapteR::dim.FLMatrix(M),
               base::dim(m),
               check.attributes=FALSE)
  expect_equal(AdapteR::dim.FLTable(T1),
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
test_that("check FLCastFunctions",
{
  M1 <- initF.FLMatrix(n=5)
  V1 <- as.FLVector(sample(1:100,5),connection)
  V1R <- as.vector(V1)
  P1 <- initF.FLVector(n=5,isRowVec=TRUE)
  T1 <- initF.FLTable(rows=5,cols=5)
    expect_equal(as.vector(M1$FL),as.vector(M1$R),check.attributes=FALSE)
    expect_equal(as.vector(P1$FL),as.vector(P1$R),check.attributes=FALSE)
    expect_equal(as.data.frame(M1$FL),as.data.frame(M1$R),check.attributes=FALSE)
    testthat::expect_equal(as.matrix(P1$FL),as.matrix(P1$R),check.attributes=FALSE)
    testthat::expect_equal(as.matrix(V1),as.matrix(V1R),check.attributes=FALSE)
    expect_equal(as.FLMatrix(M1$R,connection),as.matrix(M1$FL),check.attributes=FALSE)
    expect_equal(as.FLMatrix(P1$FL),as.matrix(P1$R),check.attributes=FALSE)
    expect_equal(as.FLMatrix(V1),as.matrix(V1R),check.attributes=FALSE)
    expect_equal(as.FLMatrix(P1$R,connection),as.matrix(P1$R),check.attributes=FALSE)
    expect_equal(as.FLVector(M1$R,connection),as.vector(M1$R),check.attributes=FALSE)
    expect_equal(as.FLVector(M1$FL),as.matrix(M1$R),check.attributes=FALSE)
})

## Testing FLCholskeyDecomp
### Phani-- needs a hermitian positive definite matrix as input
test_that("check FLCholskeyDecomp",
{
  m4 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  expect_equal(chol(m4),
               Matrix::chol(as.matrix(m4)))
})

# Testing FLLUDecomp
test_that("check LU Decomposition",
{
  m <- initF.FLMatrix(n=5)
  expect_equal(AdapteR::expand(AdapteR::lu(m$FL)),
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
## gk: todo: refactor SQL statements for performance.  This is bad performance.
test_that("check result for Matrix M_Subtraction",
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
  },function(x) (do.call("-",x)),
  function(x) do.call("-",x),n=5,isSquare=TRUE
  )
})


## Testing M_Subtraction
## gk: todo: refactor SQL statements for performance.  This is bad performance.
test_that("check result for M_Subtraction",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)
  expect_equal(M1$FL-M2,M1$R-M2R,check.attributes=FALSE)
  expect_equal(V1-V2,V1R-V2R,check.attributes=FALSE)
  expect_equal(P1$FL-P1$FL,P1$R-P1$R,check.attributes=FALSE)
  expect_equal(V1-P1$FL,V1R-P1$R,check.attributes=FALSE)
  expect_equal(P1$FL-V2,P1$R-V2R,check.attributes=FALSE)
  expect_equal((M1$FL-V2),M1$R-V2R,check.attributes=FALSE)
  expect_equal((M1$FL-P1$FL),M1$R-P1$R,check.attributes=FALSE)
  expect_equal((V1-M2),V1R-M2R,check.attributes=FALSE)
  expect_equal((P1$FL-M2),P1$R-M2R,check.attributes=FALSE)
  expect_equal((P1$FL-P1$FL-V1-V2-M2-P1$FL-M1$FL-V2),
               P1$R-P1$R-V1R-V2R-M2R-P1$R-M1$R-V2R,
               check.attributes=FALSE)
})

## Testing M_IntegerDivision. Only 2 FLMatrices
test_that("check result for M_IntegerDivision",
{
  expect_eval_equal(initF=function(n) {
      a <- initF.FLMatrix(n=5,isSquare=TRUE)
      b <- FLMatrix(connection,
                    "FL_DEMO", "tblmatrixMulti",
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
## Bad performance
test_that("check result for M_IntegerDivision",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

    expect_equal((M1$FL%/%M2),M1$R%/%M2R,check.attributes=FALSE)
    expect_equal((V1%/%V2),V1R%/%V2R,check.attributes=FALSE)
    expect_equal((P1$FL%/%P1$FL),P1$R%/%P1$R,check.attributes=FALSE)
    expect_equal((V1%/%P1$FL),V1R%/%P1$R,check.attributes=FALSE)
    expect_equal((P1$FL%/%V2),P1$R%/%V2R,check.attributes=FALSE)
    expect_equal((M1$FL%/%V2),M1$R%/%V2R,check.attributes=FALSE)
    expect_equal((M1$FL%/%P1$FL),M1$R%/%P1$R,check.attributes=FALSE)
    expect_equal((V1%/%M2),V1R%/%M2R,check.attributes=FALSE)
    expect_equal((P1$FL%/%M2),P1$R%/%M2R,check.attributes=FALSE)
})

## Testing M_CrossProduct only two FLMatrices
test_that("check result for M_CrossProduct",
{
  expect_eval_equal(initF=function(n) {
      a <- initF.FLMatrix(n=5)
      b <- FLMatrix(connection,
                    "FL_DEMO", "tblmatrixMulti",
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
## Bad performance
test_that("check result for M_CrossProduct",
{
  M1 <- initF.FLMatrix(n=5) # 5*4 matrix
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",3,"MATRIX_ID","ROW_ID","COL_ID","CELL_VAL") # 4*5 matrix
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,5),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,5),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=5,isRowVec=TRUE)
    expect_equal((M1$FL %*% M2),M1$R%*%M2R,check.attributes=FALSE)
    expect_equal((V1%*%V1),V1R%*%V1R,check.attributes=FALSE)
    expect_equal((P1$FL%*%P1$FL),P1$R%*%P1$R,check.attributes=FALSE)
    expect_equal((V1%*%P1$FL),V1R%*%P1$R,check.attributes=FALSE)
    expect_equal((P1$FL%*%V1),P1$R%*%V1R,check.attributes=FALSE)
    expect_equal((M2%*%V2),M2R%*%V2R,check.attributes=FALSE)
    expect_equal((M2%*%P1$FL),M2R%*%P1$R,check.attributes=FALSE)
    expect_equal((V1%*%M1$FL),V1R%*%M1$R,check.attributes=FALSE)
    expect_equal((P1$FL%*%M1$FL),P1$R%*%M1$R,check.attributes=FALSE)
})

## Testing M_Addition
## gk: todo: refactor SQL statements for performance.  This is bad performance.
test_that("check result for Matrix M_Addition",
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
  },function(x) (do.call("+",x)),
  function(x) do.call("+",x),n=5,isSquare=TRUE
  )
})

## Testing M_Addition
test_that("check result for M_Addition",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,
                  "FL_DEMO", "tblmatrixMulti",
                  5, "MATRIX_ID",
                  "ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

  expect_equal(M1$FL+M2,
               M1$R+M2R,
               check.attributes=FALSE)
  expect_equal(V1+V2,
               V1R+V2R,
               check.attributes=FALSE)
  expect_equal(P1$FL+P1$FL,
               P1$R+P1$R,
               check.attributes=FALSE)
  expect_equal(V1+P1$FL,
               V1R+P1$R,
               check.attributes=FALSE)
  expect_equal(P1$FL+V2,
               P1$R+V2R,
               check.attributes=FALSE)
  expect_equal(M1$FL+V2,
               M1$R+V2R,
               check.attributes=FALSE)
  expect_equal(M1$FL+P1$FL,
               M1$R+P1$R,
               check.attributes=FALSE)
  expect_equal(V1+M2,
               V1R+M2R,
               check.attributes=FALSE)
  expect_equal(P1$FL+M2,
               P1$R+M2R,
               check.attributes=FALSE)
  expect_equal(P1$FL+P1$FL+V1+V2+M2+P1$FL+M1$FL+V2,
               P1$R+P1$R+V1R+V2R+M2R+P1$R+M1$R+V2R,
               check.attributes=FALSE)
})

## Testing M_Division
test_that("check result for M_Division",
{
    M1 <- initF.FLMatrix(n=5,
                         isSquare=TRUE)
    M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",
              5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    M2R <- as.matrix(M2)
    V1 <- as.FLVector(sample(1:100,
                             10),
                      connection)
    V1R <- as.vector(V1)
    V2 <- as.FLVector(sample(1:100,
                             10),
                      connection)
    V2R <- as.vector(V2)
    P1 <- initF.FLVector(n=10,
                         isRowVec=TRUE)
    expect_equal((M1$FL/M2),
                 M1$R/M2R,
                 check.attributes=FALSE)
    expect_equal((V1/V2),
                 V1R/V2R,
                 check.attributes=FALSE)
    expect_equal((P1$FL/P1$FL),
                 P1$R/P1$R,
                 check.attributes=FALSE)
    expect_equal((V1/P1$FL),
                 V1R/P1$R,
                 check.attributes=FALSE)
    expect_equal((P1$FL/V2),
                 P1$R/V2R,
                 check.attributes=FALSE)
    expect_equal((M1$FL/V2),
                 M1$R/V2R,
                 check.attributes=FALSE)
    expect_equal((M1$FL/P1$FL),
                 M1$R/P1$R,
                 check.attributes=FALSE)
    expect_equal((V1/M2),
                 V1R/M2R,
                 check.attributes=FALSE)
    expect_equal((P1$FL/M2),
                 P1$R/M2R,
                 check.attributes=FALSE)
})

## Testing M_Multiplication
test_that("check result for M_Multiplication",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

    expect_equal(M1$FL*M2,M1$R*M2R,check.attributes=FALSE)
    expect_equal(V1*V2,V1R*V2R,check.attributes=FALSE)
    expect_equal(P1$FL*P1$FL,P1$R*P1$R,check.attributes=FALSE)
    expect_equal(V1*P1$FL,V1R*P1$R,check.attributes=FALSE)
    expect_equal(P1$FL*V2,P1$R*V2R,check.attributes=FALSE)
    expect_equal(M1$FL*V2,M1$R*V2R,check.attributes=FALSE)
    expect_equal(M1$FL*P1$FL,M1$R*P1$R,check.attributes=FALSE)
    expect_equal(V1*M2,V1R*M2R,check.attributes=FALSE)
    expect_equal(P1$FL*M2,P1$R*M2R,check.attributes=FALSE)

    expect_equal(P1$FL*P1$FL*V1*V2*M2*P1$FL*M1$FL*V2,
      P1$R*P1$R*V1R*V2R*M2R*P1$R*M1$R*V2R,
      check.attributes=FALSE)
})

## Testing M_Remainder
test_that("check result for M_Remainder",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

    expect_equal((M1$FL%%M2),M1$R%%M2R,check.attributes=FALSE)
    expect_equal((V1%%V2),V1R%%V2R,check.attributes=FALSE)
    expect_equal((P1$FL%%P1$FL),P1$R%%P1$R,check.attributes=FALSE)
    expect_equal((V1%%P1$FL),V1R%%P1$R,check.attributes=FALSE)
    expect_equal((P1$FL%%V2),P1$R%%V2R,check.attributes=FALSE)
    expect_equal((M1$FL%%V2),M1$R%%V2R,check.attributes=FALSE)
    expect_equal((M1$FL%%P1$FL),M1$R%%P1$R,check.attributes=FALSE)
    expect_equal((V1%%M2),V1R%%M2R,check.attributes=FALSE)
    expect_equal((P1$FL%%M2),P1$R%%M2R,check.attributes=FALSE)
})

## Testing Equality
test_that("check result for M_Equality",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(as.matrix(M2),connection)
  M3R <- as.matrix(M2)
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

    expect_equal(M1$FL==M2,(M1$R==M2R),check.attributes=FALSE)
    expect_equal(M1$FL==M1$FL,M1$R==M1$R,check.attributes=FALSE)
    expect_equal(M2==M3,M2R==M3R,check.attributes=FALSE)
    expect_equal(V1==V1R,V1R==V1R,check.attributes=FALSE)
    expect_equal(P1$FL==P1$FL,P1$R==P1$R,check.attributes=FALSE)
    expect_equal(V1==P1$FL,V1R==P1$R,check.attributes=FALSE)
    expect_equal(P1$FL==P1$R,P1$R==P1$R,check.attributes=FALSE)
    #expect_equal(M1$FL==V2,M1$R==V2R,check.attributes=FALSE)
    #expect_equal(M1$FL==P1$FL,M1$R==P1$R,check.attributes=FALSE)
    expect_equal(V1==V1,V1R==V1R,check.attributes=FALSE)
    #expect_equal(P1$FL==M2,P1$R==M2R,check.attributes=FALSE)
})

## Testing FLIdentical
test_that("check result for identical",
{
  M1 <- initF.FLMatrix(n=5,isSquare=TRUE)
  M2 <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
  M3 <- as.FLMatrix(as.matrix(M2),connection)
  M3R <- as.matrix(M2)
  M2R <- as.matrix(M2)
  V1 <- as.FLVector(sample(1:100,10),connection)
  V1R <- as.vector(V1)
  V2 <- as.FLVector(sample(1:100,10),connection)
  V2R <- as.vector(V2)
  P1 <- initF.FLVector(n=10,isRowVec=TRUE)

    expect_equal(identical(M1$FL,M2),identical(M1$R,M2R),check.attributes=FALSE)
    expect_equal(identical(M1$FL,M1$FL),identical(M1$R,M1$R),check.attributes=FALSE)
    expect_equal(identical(M2,M3),identical(M2R,M3R),check.attributes=FALSE)
    expect_equal(identical(V1,V1R),identical(V1R,V1R),check.attributes=FALSE)
    expect_equal(identical(P1$FL,P1$FL),identical(P1$R,P1$R),check.attributes=FALSE)
    expect_equal(identical(V1,P1$FL),identical(V1R,P1$R),check.attributes=FALSE)
    expect_equal(identical(P1$FL,P1$R),identical(P1$R,P1$R),check.attributes=FALSE)
    expect_equal(identical(M1$FL,V2),identical(M1$R,V2R),check.attributes=FALSE)
    expect_equal(identical(M1$FL,P1$FL),identical(M1$R,P1$R),check.attributes=FALSE)
    expect_equal(identical(V1,V1),identical(V1R,V1R),check.attributes=FALSE)
    expect_equal(identical(P1$FL,M2),identical(P1$R,M2R),check.attributes=FALSE)
})
#################################################################
########### no equivqlent R functions to test against ###########
################### but functions work ##########################
## Testing FLSV
test_that("check FLSV working",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
    expect_equal(
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
  expect_equal(
      dim(FLMatrixRREF(M)),
      dim(M)
  )
  expect_equal(
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
### Phani-- works only with matrices with non-complex
### eigen values. So input taken from DbLytix manual.
test_that("check Jordan Decomposition",
{
  M <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
    FLJordan(M)
})

## Testing FLSolveExcl
test_that("check FLSolveExcl",
{
  M <- initF.FLMatrix(n=5,isSquare=TRUE)$FL
    expect_equal(dim(FLSolveExcl(M,3)),dim(M)-1)
    expect_equal(dim(FLSolveExcl(M,6)),dim(M))
})

## Testing FLTriDiag
test_that("check FLTriDiag",
{
    FLTriDiag(initF.FLMatrix(n=5,isSquare=TRUE)$FL)
})

#################################################################################
################### Functions work but output slightly differs ##################
###################### from corresponding R functions ###########################

## gk: discuss with Raman for JIRA DBlytix
## Testing FLEigen
## Phani -- results differ in Teradata and R
test_that("check FLEigen",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::eigen,
                      function(m)
                          llply(base::eigen(m),
                                as.numeric),
                      n=5,
                      isSquare=TRUE)
})

## Testing FLSVDecomp
## Phani -- results differ in Teradata and R
test_that("check Singular Value Decomposition",
{
    expect_eval_equal(initF.FLMatrix,AdapteR::svd,base::svd,n=5)
})

## Testing FLDet
### Phani-- for some matrices R output = -(DBLytix output)
test_that("check determinant result",{
    expect_eval_equal(initF.FLMatrix,AdapteR::det,base::det,n=5,isSquare=TRUE)
})

## Testing FLQRDecomposition
### Phani-- could not calculate pivot properly,
### also, output qr matrix differs from R
test_that("check FLQRDecomposition",
{
  M <- initF.FLMatrix(n=5)
    expect_eval_equal(initF.FLMatrix,AdapteR::qr,base::qr,n=5)
})

####################################################################################################
####################################################################################################
#***************************************************************************************************
#***************************************************************************************************
#################################################################################
############################# Non Working Tests #################################
#################################################################################



## Testing FLTranspose
## gk:  that is broken since rbind
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

## Testing FLSubsetting
test_that("check matrix subsetting",
{
  ## Testing result
  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))         
    },function(x) do.call("[",list(x,c("b","a"),2)),
    function(x) do.call("[",list(x,c("b","a"),2)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))  
    },function(x) do.call("[",list(x)),
    function(x) do.call("[",list(x)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))  
    },function(x) "["(x,c("b","c"),),
    function(x)"["(x,c("b","c"),),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))  
    },function(x) "["(x,,2:3),
    function(x) "["(x,,2:3),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",1,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) do.call("[",list(x,c("b","a"),2)),
    function(x) do.call("[",list(x,c("b","a"),2)),n=6
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",1,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) do.call("[",list(x)),
    function(x) do.call("[",list(x)),n=6
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",1,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) "["(x,c("b","c"),),
    function(x)"["(x,c("b","c"),),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- FLMatrix(connection,"FL_DEMO","tblmatrixMulti",1,
                   "MATRIX_ID","ROW_ID","COL_ID","CELL_VAL",
                    dimnames=list(c("a","b","c"),1:3))
        list(R=as.matrix(a),
             FL=a)
    },function(x) "["(x,,2:3),
    function(x) "["(x,,2:3),n=4
  )

  #Testing dimnames of result
  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))         
    },function(x) dimnames("["(list(x,c("b","a"),2))),
    function(x) dimnames("["(list(x,c("b","a"),2))),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {

        a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))  
    },function(x) dimnames("["(list(x))),
    function(x) dimnames("["(list(x))),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))  
    },function(x) dimnames("["(x,c("b","c"),)),
    function(x) dimnames("["(x,c("b","c"),)),n=4
  )

  expect_eval_equal(initF=function(n,isSquare=FALSE) {
      a <- matrix(c(1:(n*(n-1))),n,dimnames=list(letters[1:(n%%26)],1:(n-1)))
        list(R=a,FL=as.FLMatrix(a,connection))  
    },function(x) dimnames("["(x,,2:3)),
    function(x) dimnames("["(x,,2:3)),n=4
  )
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

## testing M_Subtraction with different length vectors
test_that("check FLVector subtraction",
{
  flt <- FLTable(connection,"FL_DEMO","finequityreturns","txndate")
  flv1 <- flt[1:8,"equityreturn"]
  flv <- flt[1:10,"equityreturn"]
  flv1R <- as.vector(flv1)
  flvR <- as.vector(flv)
  expect_equal(flv-flv1,flvR-flv1R,check.attributes=FALSE)
})

## Testing FLCorrel
test_that("check FLCorrel result",
{
  fltDeep <- FLTable(connection,"FL_DEMO","tblAbaloneDeep",
                "ObsID","VarID","Num_Val",
                whereconditions="FL_DEMO.tblAbaloneDeep.ObsID < 21")
  RtDeep <- as.data.frame(fltDeep)
  fltWide <- FLTable(connection,"FL_DEMO","tblAbaloneWide",
                "ObsID",whereconditions="FL_DEMO.tblAbaloneWide.ObsID < 21")
  RtWide <- as.data.frame(fltWide)
  vRow <- initF.FLVector(20,TRUE)
  flvRow <- vRow$FL
  RvRow <- vRow$R
  RvCol <- rnorm(20)
  flvCol <- as.FLVector(RvCol,connection)
  m <- initF.FLMatrix(20)
  flm <- m$FL
  Rm <- m$R
  expect_equal(cor(flm,flm),cor(Rm,Rm),check.attributes=FALSE)
  expect_equal(cor(flvRow,flvRow),cor(RvRow,RvRow),check.attributes=FALSE)
  expect_equal(cor(flvCol,flvCol),cor(RvCol,RvCol),check.attributes=FALSE)
  expect_equal(cor(fltDeep,fltDeep),cor(RtDeep,RtDeep),check.attributes=FALSE)
  expect_equal(cor(flm,flvRow),cor(Rm,RvRow),check.attributes=FALSE)
  expect_equal(cor(flm,flvCol),cor(Rm,RvCol),check.attributes=FALSE)
  expect_equal(cor(flvCol,flvRow),cor(RvCol,RvRow),check.attributes=FALSE)
  expect_equal(cor(flm,fltDeep),cor(Rm,RtDeep),check.attributes=FALSE)
  expect_equal(cor(flvRow,fltDeep),cor(RvRow,RtDeep),check.attributes=FALSE)
  expect_equal(cor(flvCol,fltDeep),cor(RvCol,RtDeep),check.attributes=FALSE)
  ## ALL cases with wideTables not working as "wideToDeep is NotWorking"
  expect_equal(cor(fltDeep,fltWide),cor(RtDeep,RtWide),check.attributes=FALSE)
  expect_equal(cor(fltWide,fltWide),cor(RtWide,RtWide),check.attributes=FALSE)
  expect_equal(cor(fltWide,fltDeep),cor(RtWide,RtDeep),check.attributes=FALSE)
  expect_equal(cor(flm,fltWide),cor(Rm,RtWide),check.attributes=FALSE)
  expect_equal(cor(flvRow,fltWide),cor(RvRow,RtWide),check.attributes=FALSE)
  expect_equal(cor(flvCol,fltWide),cor(RvCol,RtWide),check.attributes=FALSE)
  })