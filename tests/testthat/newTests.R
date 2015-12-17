<<<<<<< .mine
##library(AdapteR)

library(testthat)
require(Matrix)

## if (exists("connection")) {
##     dbDisconnect(connection)
##     rm(connection)
## }
## if(!exists("connection")){
##     ##connection <- odbcConnect("Gandalf")
##     connection <- tdConnect(host,user,passwd,database,"jdbc")
## }

FLStartSession(connection, persistent="test")

ignoreDimNames <- TRUE

options(debugSQL=FALSE)

###############################################################
############# POSITIVE TEST CASES #############################
###############################################################

expect_eval_equal <- function(initF,FLcomputationF,RcomputationF,n) 
{
  expect_equal(FLcomputationF(initF(n)$FL),RcomputationF(initF(n)$R),check.attributes=FALSE)
}

## Increase the value of n to increase the dimensions of FLMatrix returned.
## Returns n*n or n*(n-1) based on isSquare.
initF.FLMatrix <- function(n,isSquare=FALSE)
{
  sqlSendUpdate(connection,
                      c(paste0("DROP TABLE FL_DEMO.test_matrixtable_AdapteR;"),
                        paste0("CREATE TABLE FL_DEMO.test_matrixtable_AdapteR 
                          AS(SELECT 1 AS MATRIX_ID,a.serialval AS ROW_ID,
                            b.serialval AS COL_ID,random(0,100) AS CELL_VAL 
                          FROM FL_DEMO.fzzlserial a,FL_DEMO.fzzlserial b
                          WHERE a.serialval < ",n," and b.serialval < ",ifelse(isSquare,n,n-1),") WITH DATA ")))
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


## Testing FLSolve
###Phani-- This fails because we dont'have expect_equal(flmatrixobject,Rmatrix)
### will try to overload all.equal to work for FLMatrix objects.
test_that("check inverse calculation of matrix", {
    expect_eval_equal(initF.FLMatrix,AdapteR::solve,base::solve,5)
})

# Testing rankMatrix
test_that("check rankMatrix result",{
    expect_eval_equal(initF.FLMatrix,AdapteR::rankMatrix,Matrix::rankMatrix,5)
})












































################################################################################
############################# OLD TESTS ########################################
################################################################################
## gk: todo: fix rownames
test_that("Casting base R matrix <---> in-database Matrices",{
    ## Creating simple base R matrix
    matrix1 <- matrix(1:25,5)
    matrix2 <- matrix(1:25,5)
    if(!ignoreDimNames){
        rownames(matrix1) <- c("a","b","c","d","e")
        colnames(matrix1) <- c("p","q","r","s","t")
        rownames(matrix2) <- c("A","B","C","D","E")
        colnames(matrix2) <- c("P","Q","R","S","T")
    }
    ##  FLMatrices from R matrices
    m1 <- as.FLMatrix(matrix1, connection)
    m2 <- as.FLMatrix(matrix2, connection)
    expect_equal(dim(m1),c(5,5))
    expect_equal(dim(m2),c(5,5))
    expect_equal(as.vector(m1), as.vector(matrix1))
    expect_equal(as.vector(m2), as.vector(matrix2))
    ##
    ##
    ## FLMatrix -> R matrix
    matrix3 <- as.matrix(m2)
    expect_equal(dim(matrix3),
                 c(5,5))
    expect_equal(as.vector(m2),
                 as.vector(matrix3))
    if(!ignoreDimNames)
    test_that({
        expect_equal(rownames(m2),
                     rownames(matrix3))
        expect_equal(colnames(m2),
                     colnames(matrix3))
        expect_equal(rownames(m1),
                     rownames(matrix1))
        expect_equal(colnames(m1),
                     colnames(matrix1))
        expect_equal(rownames(m2),
                     rownames(matrix2))
        expect_equal(colnames(m2),
                     colnames(matrix2))
    })
})


test_that("Casting R matrix Packages <---> in-database Matrices",{
    ## Creating simple FLMatrices from base R matrix
    matrix1 <- matrix(1:25,5)
    matrix2 <- matrix(1:25,5)
    if(!ignoreDimNames){
        rownames(matrix1) <- c("a","b","c","d","e")
        colnames(matrix1) <- c("p","q","r","s","t")
        rownames(matrix2) <- c("A","B","C","D","E")
        colnames(matrix2) <- c("P","Q","R","S","T")
    }
    m1 <- as.FLMatrix(matrix1, connection) # Identity matrix of dimension 5x5
    m2 <- as.FLMatrix(matrix2, connection) # Identity matrix of dimension 5x5
    expect_equal(dim(m1),c(5,5))
    expect_equal(dim(m2),c(5,5))
    ##
    matrix3 <- as.matrix(m2)
    expect_equal(dim(matrix3),c(5,5))
    expect_equal(as.vector(m2),as.vector(matrix3))
    if(!ignoreDimNames){
        expect_equal(rownames(m1),rownames(matrix1))
        expect_equal(colnames(m1),colnames(matrix1))
        expect_equal(rownames(m2),rownames(matrix2))
        expect_equal(colnames(m2),colnames(matrix2))
        expect_equal(rownames(m2),rownames(matrix3))
        expect_equal(colnames(m2),colnames(matrix3))
    }
})

test_that("Casting base R matrix <---> in-database Matrices",{
    smatrix1 <-sparseMatrix(c(1,3:8), c(2,9,6:10), x = 7 * (1:7))
    class(as.matrix(smatrix1))
    sm1 <- as.FLMatrix(smatrix1,connection)
    expect_equal(dim(smatrix1),dim(sm1))
    expect_equal(as.vector(sm1),as.vector(smatrix1))
    if(!ignoreDimNames){
        expect_equal(rownames(sm1),rownames(smatrix1))
        expect_equal(colnames(sm1),colnames(smatrix1))
    }
##    expect_equal(as.matrix(sm1,sparse=TRUE),smatrix1)
})

                                        # Testing FLIs
test_that("check class of a Matrix",
{
    m <- as.FLMatrix(matrix(1:25,5),connection)   # Non-symmetric singular matrix of dimension 5x5
    expect_is(
        m, "FLMatrix"
    )
})


                                        # Testing FLMatrix
test_that("check class and result of FLMatrix",
{
    matrix1 <- matrix(1:25,5)
    m <- as.FLMatrix(matrix1,connection)   # Non-symmetric singular ma
    expect_is(
        m, "FLMatrix"
    )
    expect_equal(
        nrow(m),nrow(as.matrix(m))
    )
    expect_equal(
        ncol(m),ncol(as.matrix(m))
    )
    expect_equal(
        as.vector(as.matrix(m)),as.vector(matrix1)
    )
    ## Testing FLTranspose
    
    test_that("check transpose",
              expect_equal(
              (t(m) == as.FLMatrix(t(as.matrix(m)),connection)),
              TRUE
              ))
    ##
    ## Testing FLDet
    test_that("check determinant return type",
              expect_true(
                  is.numeric(det(m))
              ))
    ##
    test_that("check determinant result",
              expect_true(
                  as.vector(det(m))==det(as.matrix(m))
              ))
    ##
    ##Testing FLDims
    test_that("get dimensions of a Matrix",
    { ## gk: redundant!?
        expect_true(
            is.numeric(
                ncol(m)
            ))
        expect_true(
            is.numeric(
                NCOL(m)
            ))
        expect_true(
            is.numeric(
                nrow(m)
            ))
        expect_true(
            is.numeric(
                NROW(m)
            ))
        expect_true(
            is.numeric(
                dim(m)
            ))
    }) 
})

                                        # Testing FLSparseMatrix
test_that("check class and result of FLSparseMatrix",
{
    smatrix1 <-sparseMatrix(c(1,3:8), c(2,9,6:10), x = 7 * (1:7))
    sm1 <- as.FLSparseMatrix(smatrix1,connection)
    expect_is(
        sm1, "FLMatrix" ## gk:: we do not need a separate class, introduces too much complexity
    )
    expect_equal(
        nrow(sm1),nrow(smatrix1)
    )
    expect_equal(
        ncol(sm1),ncol(smatrix1)
    )
})


test_that("get dimensions of a Sparse Matrix",
{
    sm2 <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMultiSparse", 2)
    expect_true(
        is.numeric(
            ncol(sm2)
        ))
    expect_true(
        is.numeric(
            nrow(sm2)
        ))
    expect_true(
        is.numeric(
            NCOL(sm2)
        ))
    expect_true(
        is.numeric(
            NROW(sm2)
        ))
    expect_true(
        is.numeric(
            dim(sm2)
        ))
})


## gk: works, please condition out warnings!
## Zero matrix of dimension 5x5
m0 <- as.FLMatrix(matrix(0,5,5), connection)

## Identity matrix of dimension 5x5
m <- as.FLMatrix(matrix(1:25,5),connection)   # Non-symmetric singular matrix of dimension 5x5
m2 <- as.FLMatrix(matrix(26:50,5), connection) # Non Symmetric singular matrix of dimension 5x5
m3 <- FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",3) #  Non-Square Matrix of dimension 4x5
m4 <- FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",5) # Symmetric non-singular matrix of dimension 5x5
m5 <- as.FLMatrix(matrix(runif(25,-30,30),5,5),connection) # Random matrix of dimension 5x5

WideTable <- FLTable(connection, "FL_TRAIN", "tblVectorWide","vector_key")
colnames(WideTable)
rownames(WideTable)
dim(WideTable)
as.data.frame(WideTable)


as.data.frame(WideTable)
v1 <- WideTable[,"vector_value"]
### Phani-- subsetting FLVector
v2 <- v1[2:1]
print(v2)
### Phani-- subsetting FLMatrix with mapping
m3 <- FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",1,dimnames=list(c("a","b","c"),1:3))
m3[c("b","c"),]
m3[,]
m3[]
m3[,2:1]
m3[c("b","c"),1:2]


WideTable
rownames(WideTable)
WideTable[1:3,]
WideTable[c("1","2"),]

c(as.vector(v1))

v1


#############################################################
## For in-database analytix the matrix is in the warehouse
## to begin with.
## 
## A remote matrix is easily created by specifying
##
m <- eqnRtn <- FLMatrix(connection,
                        database          = "FL_DEMO",
                        matrix_table      = "finEquityReturns",
                        matrix_id_value   = "",
                        matrix_id_colname = "",
                        row_id_colname    = "TxnDate",
                        col_id_colname    = "TickerSymbol",
                        cell_val_colname  = "EquityReturn")



DeepTable <- FLTable(connection,
                     database          = "FL_DEMO",
                     table      = "finEquityReturns",
                     obs_id_colname = "TxnDate",
                     var_id_colnames = "TickerSymbol",
                     cell_val_colname = "EquityReturn")
DeepTable[1:10,1:10]

DeepTable <- DeepTable[sort(rownames(DeepTable)),]

v2 <- DeepTable[,"WSR"]
v2
# options(debugSQL=FALSE)


require(reshape2)
as.FLVector(as.vector(v1),connection)

test_that("Wide tables and Vectors",
{
    ## # Creating FLVectors
    ## Testing FLvector
    expect_is(
        v1, "FLVector"
    )
    expect_is(
        v2, "FLVector"
    )
    test_that("check class and reproducibility of FLVector",
    {
        expect_equal(
            as.vector(v1),as.vector(
                              as.FLVector(as.vector(v1),connection)))
    })
    test_that("check class of a Table",
    {
        expect_is(
            DeepTable, "FLTable"
        )
        expect_is(
            WideTable, "FLTable"
        )
    })
    test_that("get dimensions of a Vector",
    {
        expect_true(
            is.numeric(
                NCOL(v1)
            ))
        expect_true(
            is.numeric(
                NROW(v1)
            ))
        expect_true(
            is.numeric(
                dim(v1)
            ))
    })
})

test_that("check storing of diagonal matrices is working.",  
          expect_equal(
              as.matrix(solve(m4)),
              as.matrix(
                  as.FLMatrix(
                      solve(as.matrix(m4)),
                      connection))))

## gk: I need to refactor FLVector, currently it seems to store strings?
##Testing FLDiag
test_that("check the result of the diag of matrix",
{
    expect_equal(
        as.vector(diag(m4)),
        diag(as.matrix(m4)))
    expect_equal(
        as.matrix(diag(as.FLVector(1:2,connection))) , diag(1:2), check.attributes=FALSE)
    expect_equal(
        as.matrix(diag(as.FLVector(2,connection))), diag(2), check.attributes=FALSE)
})

## Testing FLRankMatrix
test_that("check rankMatrix return type",
          expect_true(
              is.vector(rankMatrix(m4))
          ))
test_that("check rankMatrix result",{
    rRank <- rankMatrix(as.matrix(m4))
    expect_true(
        rankMatrix(m4)==rRank
    )
})


## gk: REFACTOR: rbind is using for loops and data copy!!
##Testing rbind
test_that("check return type and working for rbind",
          expect_true(
              is.FLMatrix(rbind(m4,1:3,data.frame(1,2,3,4,5),v1,matrix(1:25,5)))
          ))
test_that("check dimensions returned for rbind", {
    expect_true(
        nrow(rbind(m4,1:3))==nrow(m4)+1)
    expect_true(
        ncol(rbind(m4,1:3))==ncol(m4)
    )
    expect_true(
        ncol(rbind(1:5,1:3))==5
    )
})
##Testing cbind
test_that("check return type and working for cbind",
          expect_true(
              is.FLMatrix(cbind(m4,1:3,data.frame(1:5),v1,matrix(1:25,5)))
          ))
test_that("check dimensions returned for cbind", {
    expect_true(
        ncol(cbind(m4,1:3))==ncol(m4)+1)
    expect_true(
        nrow(cbind(m4,1:3))==nrow(m4)
    )
    expect_true(
        nrow(cbind(1:5,1:3))==5
    )
})


test_that("check Jordan Decomposition",
          jordan(m4))

## Testing FLHessenDecomp
test_that("check Hessenberg Decomposition",
{
    hessen(m4) 
    hessen(m)
})

expect_flequal <- function(a,b){
    if(is.list(a))
        for(i in 1:length(a))
            expect_flequal(a[[i]],b[[i]])

    if(is.FLMatrix(a)) a <- as.matrix(a)
    if(is.FLMatrix(b)) b <- as.matrix(b)
    expect_equal(a,b)
}

## Testing FLSVDecomp
## Phani -- results differ in Teradata and R
### but function works.
test_that("check Singular Value Decomposition",
{
    flv <- svd(m4)
    print(flv)
    expect_flequal(svd(m), svd(as.matrix(m)))
    svd(m3) 
})
                                        # Testing FLLUDecomp
test_that("check LU Decomposition",
{
    lu(m4) 
    lu(m) 
    lu(m3) 
})
## Testing FLTrace
test_that("check LU Decomposition",
{
    tr(m) 
    tr(m3) 
})
test_that("check FLTrace return type",
          expect_true(
              is.FLVector(tr(m))
          ))
test_that("check length of FLTrace result",
          expect_true(
              length(tr(m))==1
          ))


                                        #Testing FLSV
test_that("check return type and working for FLSV",
          expect_true(
              is.FLVector(FLSV(m5))
          ))

test_that("check dimensions returned for FLSV",
          expect_true(
              length(FLSV(m5))==nrow(m5)
          ))

                                        #Testing RowMeans
                                        ### Phani-- very less difference in results
                                        ### but test case-3 fails
test_that("check return type and working for rowMeans",
          expect_true(
              is.FLVector(rowMeans(m5))
          ))

test_that("check dimensions returned for rowMeans",
          expect_true(
              length(rowMeans(m5))==nrow(m5)
          ))

test_that("check result for rowMeans",
          expect_equal(as.vector(rowMeans(m5)), rowMeans(as.matrix(m5)))
          )

                                        #Testing ColMeans
test_that("check return type and working for colMeans",
          expect_true(
              is.FLVector(colMeans(m5))
          ))

test_that("check dimensions returned for colMeans",
          expect_true(
              length(colMeans(m5))==ncol(m5)
          ))

test_that("check result for colMeans",
          expect_equal(as.vector(colMeans(m5)), colMeans(as.matrix(m5)))
          )

                                        #Testing RowSums
test_that("check return type and working for rowSums",
          expect_true(
              is.FLVector(rowSums(m5))
          ))

test_that("check dimensions returned for rowSums",
          expect_true(
              length(rowSums(m5))==nrow(m5)
          ))

test_that("check result for rowSums",
          expect_equal(as.vector(rowSums(m5)), rowSums(as.matrix(m5)))
          )

                                        #Testing colSums
test_that("check return type and working for colSums",
          expect_true(
              is.FLVector(colSums(m5))
          ))

test_that("check dimensions returned for colSums",
          expect_true(
              length(colSums(m5))==ncol(m5)
          ))

test_that("check result for colSums",
          expect_equal(as.vector(colSums(m5)), colSums(as.matrix(m5)))
          )

                                        # Testing M_Addition
test_that("check result for M_Addition",
          expect_true(
          (m+1:3+matrix1+v1)==as.FLMatrix(matrix1+1:3+matrix1+as.vector(v1),connection)
          ))

                                        # Testing M_Subtraction
test_that("check result for M_Subtraction",
          expect_true(
          (m-1:3-matrix1-v1)==as.FLMatrix(matrix1-1:3-matrix1-as.vector(v1),connection)
          ))

                                        # Testing M_Multiplication
test_that("check result for M_Multiplication",
          expect_true(
          (m*1:3*matrix1*v1)==as.FLMatrix(matrix1*1:3*matrix1*as.vector(v1),connection)
          ))

                                        # Testing M_CrossProduct
test_that("check result for M_CrossProduct",
          expect_true(
          (1:5%*%matrix1%*%m)==as.FLMatrix(1:5%*%matrix1%*%matrix1,connection)
          ))

                                        # Testing M_IntegerDivision
test_that("check result for M_IntegerDivision",
          expect_true(
          (m%/%1:3%/%matrix1%/%v1)==as.FLMatrix(matrix1%/%1:3%/%matrix1%/%as.vector(v1),connection)
          ))

                                        # Testing M_Remainder
test_that("check working of M_Remainder",
          m%%1:3%%matrix1%%v1
          )

                                        # Testing FLEigen
test_that("check if FLEigen is working ",
{      eigen(m4)
    eigen(m5)
})

                                        # Testing FLLength
test_that("check if FLLength is working ",
{     length(m4)
    length(v1)
    length(sm1) })

                                        # Testing FL_CastingFunctions
test_that("different types of casting works",
{        as.vector(m)
    as.vector(v1)
    as.vector(v2)
    as.vector(sm1)
    as.matrix(m)
    as.matrix(v1)
    as.matrix(v2)
    as.matrix(sm1)
    as.FLMatrix(v1,connection)
    as.FLMatrix(v2,connection)
    as.FLMatrix(sm1,connection)
    as.FLMatrix(data.frame(1:2,3:4),connection)
    as.FLMatrix(as.vector(v1),connection)
    as.FLVector(m,connection)
    as.FLVector(sm1,connection)
    as.FLVector(smatrix1,connection)
    as.FLVector(data.frame(1:2,3:4),connection)
    as.FLVector(as.vector(v1),connection)
} )

                                        # Testing FLCorrel
test_that("different input combinations work for FLCorrel",
{         cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,2),connection))
    cor(as.FLMatrix(matrix(c(0,-1,4,-10),2),connection),as.FLMatrix(matrix(1:6,2),connection))
    cor(v1,as.FLMatrix(matrix(1:12,6),connection))
    cor(v1,v1)
    cor(matrix(1:12,6),v1)
    cor(1:2,as.FLMatrix(matrix(1:4,2),connection))
    cor(data.frame(-2:-1,c(1,0)),as.FLMatrix(matrix(1:6,2),connection))
    cor(v1,data.frame(6:1,11:16))
    cor(WideTable,WideTable)
                                        # cor(DeepTable,DeepTable)
})

test_that("check output dimensions for FLCorrel",
{
    expect_true(
        nrow(cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,2),connection)))==2
    )

    expect_true(
        ncol(cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,2),connection)))==3
    )
})

                                        # Testing FLQRDecomp
test_that("check if FLQRDecomp is working ",
{      qr(m4)
    qr(m)
    qr(m3)
})

                                        #Testing FLTriDiag
test_that("check result type and dimensions for FLTriDiag",
{
    expect_true(
        is.FLMatrix(FLTriDiag(m5))
    )

    expect_true(
        nrow(FLTriDiag(m5))==nrow(m5)
    )

    expect_true(
        ncol(FLTriDiag(m5))==ncol(m5)
    )
})

                                        #Testing FLMatrixREF
test_that("check result type and dimensions for FLMatrixREF",
{
    expect_true(
        is.FLMatrix(FLMatrixREF(m5))
    )

    expect_true(
        nrow(FLMatrixREF(m5))==nrow(m5)
    )

    expect_true(
        ncol(FLMatrixREF(m5))==ncol(m5)
    )
})

                                        #Testing FLMatrixRREF
test_that("check result type and dimensions for FLMatrixRREF",
{
    expect_true(
        is.FLMatrix(FLMatrixRREF(m5))
    )

    expect_true(
        nrow(FLMatrixRREF(m5))==nrow(m5)
    )

    expect_true(
        ncol(FLMatrixRREF(m5))==ncol(m5)
    )
})

                                        #Testing FLMatrixNorm
test_that("check result type and dimensions for FLMatrixNorm",
{
    expect_true(
        is.vector(FLMatrixNorm(m3,3))
    )

    expect_true(
        length(FLMatrixNorm(m3,4))==1
    )
})

                                        #Testing FLSolveExcl
test_that("check result type and dimensions for FLSolveExcl",
{
    expect_true(
        is.FLMatrix(solveExcl(m5,3))
    )
    
    expect_true(
        nrow(solveExcl(m5,6))==nrow(m5)
    )
    expect_true(
        ncol(solveExcl(m5,6))==ncol(m5)
    )

    expect_true(
        nrow(solveExcl(m5,3))==(nrow(m5)-1)
    )
    expect_true(
        ncol(solveExcl(m5,3))==(ncol(m5)-1)
    )

})

# Testing FLCholeskyDecomp
test_that("check if FLCholeskyDecomp is working ",
{
    chol(m4)
    expect_equal(as.matrix(chol(m4)), t(chol(as.matrix(m4))))
})

# Testing FLGInv
test_that("check if FLGInv is working ",
{     ginv(m2)
    expect_equal(as.matrix(ginv(m4)),ginv(as.matrix(m4)),check.attributes=FALSE)
    expect_equal(as.matrix(ginv(m3)),ginv(as.matrix(m3)),check.attributes=FALSE)
})
###############################################################
############# NEGATIVE TEST CASES #############################
###############################################################

test_that("check connection to correct DSN",
          expect_warning(
              odbcConnect("RandomDSN")
          )
          )

                                        # Testing FLIs
test_that("check class of a Matrix",
{
    expect_false(
        is.FLMatrix(matrix1)
    )
})

                                        # Testing FLSubsetting
test_that("check subscript out of bounds for matrix",
{
    expect_error(
        m[1:6,]
    )
    expect_error(
        m[,1:6]
    )
})

test_that("check non-equality of matrices",
          expect_false(
              m0 == m1
          ))

test_that("check non-compatibility of matrices for addition/subtraction",
{   
    expect_error(
    (m2 + m3)
    )
    expect_error(
    (m2 - m3)
    )
})

test_that("check non-compatibility of matrices for multiplication",
{   
    expect_error(
    (m2 * m3)
    )
    expect_error(
    (m2 %*% m3)
    )
})

                                        # Testing FLDet
test_that("check non-compatibility of non-square matrix for determinant",
          expect_error(
              det(m3)
          ))

test_that("check non-compatibility of det for flvector",
          expect_error(
              det(v1)
          ))

                                        # Testing FLJordanDecomp
test_that("check non-compatibility of matrix for Jordan Decomposition",
{     
    expect_error(
        jordan(m2)
    )
    expect_error(
        jordan(m3)
    )
})

                                        # Testing FLHessenDecomp
test_that("check non-compatibility of matrix for Hessenberg Decomposition",
          expect_error(
              hessen(m3)
          ))

                                        # Testing FLSolve
                                      
test_that("check non-compatibility of matrix for calculating inverse",
{  expect_error(solve(m3))
    expect_error(solve(m))
})

                                        # Testing FLCastFunctions
test_that("check non-compatibility of matrix for calculating inverse",
{  expect_null(
       as.FLMatrix(as.FLMatrix(matrix(1:4,2),connection),connection)
   )
       expect_null(
           as.FLVector(as.FLVector(c(1:4),connection),connection)
       )
})

                                        #Testing rbind
test_that("check ncol non-compatibility for rbind",
          expect_error(
              rbind(m4,matrix(1:16,4))
          ))

test_that("check input type non-compatibility for rbind",
          expect_error(
              rbind(m4,sm1)
          ))

                                        #Testing cbind
test_that("check nrow non-compatibility for cbind",
          expect_error(
              cbind(m4,matrix(1:16,4))
          ))

test_that("check input type non-compatibility for cbind",
          expect_error(
              cbind(m4,sm1)
          ))

                                        # Testing FLEigen
test_that("check non-compatibility of matrix for calculating eigenvalues and eigenvectors",
{  expect_error(eigen(m3))
    expect_error(eigen(m))
})

                                        # Testing FLCorrel

test_that("check non-compatibility of input dimensions for FLCorrel",
{ expect_error(
      cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,3),connection))
  )

      expect_error(
          cor(data.frame(1,2),as.FLMatrix(matrix(1:6,3),connection))
      )
})

                                        # Testing FLTriDiag
test_that("check non-compatibility for non-square matrices for FLTriDiag",
          expect_error(
              FLTriDiag(m3)
          ))

                                        # Testing FLMatrixRREF
test_that("check non-compatibility for non-square matrices for FLMatrixRREF",
          expect_error(
              FLMatrixRREF(m3)
          ))

                                        # Testing FLMatrixREF
test_that("check non-compatibility for non-square matrices for FLMatrixRREF",
          expect_error(
              FLMatrixREF(m3)
          ))

                                        #Testing FLMatrixNorm
test_that("check parameter NormMethod range for FLMatrixNorm",
          expect_error(
              FLMatrixNorm(m5,5))
          )

                                        # Testing FLCholeskyDecomp
                                      
test_that("check non-compatibility of matrices for Cholesky Decomposition",
{     expect_error(chol(m))
    expect_error(chol(m3))
})

                                        # Testing FLSolveExcl
test_that("check non-compatibility for non-square matrices for FLSolveExcl",
          expect_error(
              solveExcl(m3)
          ))

                                        # Testing M_Division
test_that("check division by zero for M_Division",
          expect_error(
          (m/1:3/matrix1/v1)==as.FLMatrix(matrix1/1:3/matrix1/as.vector(v1),connection)
          ))

=======
##library(AdapteR)

library(testthat)
require(Matrix)

## if (exists("connection")) {
##     dbDisconnect(connection)
##     rm(connection)
## }
## if(!exists("connection")){
##     ##connection <- odbcConnect("Gandalf")
##     connection <- tdConnect(host,user,passwd,database,"jdbc")
## }

FLStartSession(connection, persistent="test")

ignoreDimNames <- TRUE

options(debugSQL=FALSE)

###############################################################
############# POSITIVE TEST CASES #############################
###############################################################

expect_eval_equal <- function(initF,FLcomputationF,RcomputationF,n) 
{
  expect_equal(FLcomputationF(initF(n)$FL),RcomputationF(initF(n)$R),check.attributes=FALSE)
}


initF.FLMatrix <- function(n,isSquare=FALSE)
{
  sqlSendUpdate(connection,
                      c(paste0("DROP TABLE FL_DEMO.test_matrixtable_AdapteR;"),
                        paste0("CREATE TABLE FL_DEMO.test_matrixtable_AdapteR 
                          AS(SELECT 1 AS MATRIX_ID,a.serialval AS ROW_ID,
                            b.serialval AS COL_ID,random(0,100) AS CELL_VAL 
                          FROM FL_DEMO.fzzlserial a,FL_DEMO.fzzlserial b
                          WHERE a.serialval < ",n," and b.serialval < ",ifelse(isSquare,n,n-1),") WITH DATA ")))
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


## Testing FLSolve
###Phani-- This fails because we dont'have expect_equal(flmatrixobject,Rmatrix)
### will try to overload all.equal to work for FLMatrix objects.
test_that("check inverse calculation of matrix", {
    expect_eval_equal(initF.FLMatrix,AdapteR::solve,base::solve,5)
})

# Testing rankMatrix
test_that("check rankMatrix result",{
    expect_eval_equal(initF.FLMatrix,AdapteR::rankMatrix,Matrix::rankMatrix,5)
})












































################################################################################
############################# OLD TESTS ########################################
################################################################################
## gk: todo: fix rownames
test_that("Casting base R matrix <---> in-database Matrices",{
    ## Creating simple base R matrix
    matrix1 <- matrix(1:25,5)
    matrix2 <- matrix(1:25,5)
    if(!ignoreDimNames){
        rownames(matrix1) <- c("a","b","c","d","e")
        colnames(matrix1) <- c("p","q","r","s","t")
        rownames(matrix2) <- c("A","B","C","D","E")
        colnames(matrix2) <- c("P","Q","R","S","T")
    }
    ##  FLMatrices from R matrices
    m1 <- as.FLMatrix(matrix1, connection)
    m2 <- as.FLMatrix(matrix2, connection)
    expect_equal(dim(m1),c(5,5))
    expect_equal(dim(m2),c(5,5))
    expect_equal(as.vector(m1), as.vector(matrix1))
    expect_equal(as.vector(m2), as.vector(matrix2))
    ##
    ##
    ## FLMatrix -> R matrix
    matrix3 <- as.matrix(m2)
    expect_equal(dim(matrix3),
                 c(5,5))
    expect_equal(as.vector(m2),
                 as.vector(matrix3))
    if(!ignoreDimNames)
    test_that({
        expect_equal(rownames(m2),
                     rownames(matrix3))
        expect_equal(colnames(m2),
                     colnames(matrix3))
        expect_equal(rownames(m1),
                     rownames(matrix1))
        expect_equal(colnames(m1),
                     colnames(matrix1))
        expect_equal(rownames(m2),
                     rownames(matrix2))
        expect_equal(colnames(m2),
                     colnames(matrix2))
    })
})


test_that("Casting R matrix Packages <---> in-database Matrices",{
    ## Creating simple FLMatrices from base R matrix
    matrix1 <- matrix(1:25,5)
    matrix2 <- matrix(1:25,5)
    if(!ignoreDimNames){
        rownames(matrix1) <- c("a","b","c","d","e")
        colnames(matrix1) <- c("p","q","r","s","t")
        rownames(matrix2) <- c("A","B","C","D","E")
        colnames(matrix2) <- c("P","Q","R","S","T")
    }
    m1 <- as.FLMatrix(matrix1, connection) # Identity matrix of dimension 5x5
    m2 <- as.FLMatrix(matrix2, connection) # Identity matrix of dimension 5x5
    expect_equal(dim(m1),c(5,5))
    expect_equal(dim(m2),c(5,5))
    ##
    matrix3 <- as.matrix(m2)
    expect_equal(dim(matrix3),c(5,5))
    expect_equal(as.vector(m2),as.vector(matrix3))
    if(!ignoreDimNames){
        expect_equal(rownames(m1),rownames(matrix1))
        expect_equal(colnames(m1),colnames(matrix1))
        expect_equal(rownames(m2),rownames(matrix2))
        expect_equal(colnames(m2),colnames(matrix2))
        expect_equal(rownames(m2),rownames(matrix3))
        expect_equal(colnames(m2),colnames(matrix3))
    }
})

test_that("Casting base R matrix <---> in-database Matrices",{
    smatrix1 <-sparseMatrix(c(1,3:8), c(2,9,6:10), x = 7 * (1:7))
    class(as.matrix(smatrix1))
    sm1 <- as.FLMatrix(smatrix1,connection)
    expect_equal(dim(smatrix1),dim(sm1))
    expect_equal(as.vector(sm1),as.vector(smatrix1))
    if(!ignoreDimNames){
        expect_equal(rownames(sm1),rownames(smatrix1))
        expect_equal(colnames(sm1),colnames(smatrix1))
    }
##    expect_equal(as.matrix(sm1,sparse=TRUE),smatrix1)
})

                                        # Testing FLIs
test_that("check class of a Matrix",
{
    m <- as.FLMatrix(matrix(1:25,5),connection)   # Non-symmetric singular matrix of dimension 5x5
    expect_is(
        m, "FLMatrix"
    )
})


                                        # Testing FLMatrix
test_that("check class and result of FLMatrix",
{
    matrix1 <- matrix(1:25,5)
    m <- as.FLMatrix(matrix1,connection)   # Non-symmetric singular ma
    expect_is(
        m, "FLMatrix"
    )
    expect_equal(
        nrow(m),nrow(as.matrix(m))
    )
    expect_equal(
        ncol(m),ncol(as.matrix(m))
    )
    expect_equal(
        as.vector(as.matrix(m)),as.vector(matrix1)
    )
    ## Testing FLTranspose
    
    test_that("check transpose",
              expect_equal(
              (t(m) == as.FLMatrix(t(as.matrix(m)),connection)),
              TRUE
              ))
    ##
    ## Testing FLDet
    test_that("check determinant return type",
              expect_true(
                  is.numeric(det(m))
              ))
    ##
    test_that("check determinant result",
              expect_true(
                  as.vector(det(m))==det(as.matrix(m))
              ))
    ##
    ##Testing FLDims
    test_that("get dimensions of a Matrix",
    { ## gk: redundant!?
        expect_true(
            is.numeric(
                ncol(m)
            ))
        expect_true(
            is.numeric(
                NCOL(m)
            ))
        expect_true(
            is.numeric(
                nrow(m)
            ))
        expect_true(
            is.numeric(
                NROW(m)
            ))
        expect_true(
            is.numeric(
                dim(m)
            ))
    }) 
})

                                        # Testing FLSparseMatrix
test_that("check class and result of FLSparseMatrix",
{
    smatrix1 <-sparseMatrix(c(1,3:8), c(2,9,6:10), x = 7 * (1:7))
    sm1 <- as.FLSparseMatrix(smatrix1,connection)
    expect_is(
        sm1, "FLMatrix" ## gk:: we do not need a separate class, introduces too much complexity
    )
    expect_equal(
        nrow(sm1),nrow(smatrix1)
    )
    expect_equal(
        ncol(sm1),ncol(smatrix1)
    )
})


test_that("get dimensions of a Sparse Matrix",
{
    sm2 <- FLMatrix(connection, "FL_TRAIN", "tblMatrixMultiSparse", 2)
    expect_true(
        is.numeric(
            ncol(sm2)
        ))
    expect_true(
        is.numeric(
            nrow(sm2)
        ))
    expect_true(
        is.numeric(
            NCOL(sm2)
        ))
    expect_true(
        is.numeric(
            NROW(sm2)
        ))
    expect_true(
        is.numeric(
            dim(sm2)
        ))
})


## gk: works, please condition out warnings!
## Zero matrix of dimension 5x5
m0 <- as.FLMatrix(matrix(0,5,5), connection)

## Identity matrix of dimension 5x5
m <- as.FLMatrix(matrix(1:25,5),connection)   # Non-symmetric singular matrix of dimension 5x5
m2 <- as.FLMatrix(matrix(26:50,5), connection) # Non Symmetric singular matrix of dimension 5x5
m3 <- FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",3) #  Non-Square Matrix of dimension 4x5
m4 <- FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",5) # Symmetric non-singular matrix of dimension 5x5
m5 <- as.FLMatrix(matrix(runif(25,-30,30),5,5),connection) # Random matrix of dimension 5x5

WideTable <- FLTable(connection, "FL_TRAIN", "tblVectorWide","vector_key")
colnames(WideTable)
rownames(WideTable)
dim(WideTable)
as.data.frame(WideTable)


as.data.frame(WideTable)
v1 <- WideTable[,"vector_value"]
### Phani-- subsetting FLVector
v2 <- v1[2:1]
print(v2)
### Phani-- subsetting FLMatrix with mapping
m3 <- FLMatrix(connection,"FL_TRAIN","tblmatrixMulti",1,dimnames=list(c("a","b","c"),1:3))
m3[c("b","c"),]
m3[,]
m3[]
m3[,2:1]
m3[c("b","c"),1:2]


WideTable
rownames(WideTable)
WideTable[1:3,]
WideTable[c("1","2"),]

c(as.vector(v1))

v1


#############################################################
## For in-database analytix the matrix is in the warehouse
## to begin with.
## 
## A remote matrix is easily created by specifying
##
m <- eqnRtn <- FLMatrix(connection,
                        database          = "FL_DEMO",
                        matrix_table      = "finEquityReturns",
                        matrix_id_value   = "",
                        matrix_id_colname = "",
                        row_id_colname    = "TxnDate",
                        col_id_colname    = "TickerSymbol",
                        cell_val_colname  = "EquityReturn")



DeepTable <- FLTable(connection,
                     database          = "FL_DEMO",
                     table      = "finEquityReturns",
                     obs_id_colname = "TxnDate",
                     var_id_colnames = "TickerSymbol",
                     cell_val_colname = "EquityReturn")
DeepTable[1:10,1:10]

DeepTable <- DeepTable[sort(rownames(DeepTable)),]

v2 <- DeepTable[,"WSR"]
v2
# options(debugSQL=FALSE)


require(reshape2)
as.FLVector(as.vector(v1),connection)

test_that("Wide tables and Vectors",
{
    ## # Creating FLVectors
    ## Testing FLvector
    expect_is(
        v1, "FLVector"
    )
    expect_is(
        v2, "FLVector"
    )
    test_that("check class and reproducibility of FLVector",
    {
        expect_equal(
            as.vector(v1),as.vector(
                              as.FLVector(as.vector(v1),connection)))
    })
    test_that("check class of a Table",
    {
        expect_is(
            DeepTable, "FLTable"
        )
        expect_is(
            WideTable, "FLTable"
        )
    })
    test_that("get dimensions of a Vector",
    {
        expect_true(
            is.numeric(
                NCOL(v1)
            ))
        expect_true(
            is.numeric(
                NROW(v1)
            ))
        expect_true(
            is.numeric(
                dim(v1)
            ))
    })
})

test_that("check storing of diagonal matrices is working.",  
          expect_equal(
              as.matrix(solve(m4)),
              as.matrix(
                  as.FLMatrix(
                      solve(as.matrix(m4)),
                      connection))))

## gk: I need to refactor FLVector, currently it seems to store strings?
##Testing FLDiag
test_that("check the result of the diag of matrix",
{
    expect_equal(
        as.vector(diag(m4)),
        diag(as.matrix(m4)))
    expect_equal(
        as.matrix(diag(as.FLVector(1:2,connection))) , diag(1:2), check.attributes=FALSE)
    expect_equal(
        as.matrix(diag(as.FLVector(2,connection))), diag(2), check.attributes=FALSE)
})

## Testing FLRankMatrix
test_that("check rankMatrix return type",
          expect_true(
              is.vector(rankMatrix(m4))
          ))
test_that("check rankMatrix result",{
    rRank <- rankMatrix(as.matrix(m4))
    expect_true(
        rankMatrix(m4)==rRank
    )
})


## gk: REFACTOR: rbind is using for loops and data copy!!
##Testing rbind
test_that("check return type and working for rbind",
          expect_true(
              is.FLMatrix(rbind(m4,1:3,data.frame(1,2,3,4,5),v1,matrix(1:25,5)))
          ))
test_that("check dimensions returned for rbind", {
    expect_true(
        nrow(rbind(m4,1:3))==nrow(m4)+1)
    expect_true(
        ncol(rbind(m4,1:3))==ncol(m4)
    )
    expect_true(
        ncol(rbind(1:5,1:3))==5
    )
})
##Testing cbind
test_that("check return type and working for cbind",
          expect_true(
              is.FLMatrix(cbind(m4,1:3,data.frame(1:5),v1,matrix(1:25,5)))
          ))
test_that("check dimensions returned for cbind", {
    expect_true(
        ncol(cbind(m4,1:3))==ncol(m4)+1)
    expect_true(
        nrow(cbind(m4,1:3))==nrow(m4)
    )
    expect_true(
        nrow(cbind(1:5,1:3))==5
    )
})


test_that("check Jordan Decomposition",
          jordan(m4))

## Testing FLHessenDecomp
test_that("check Hessenberg Decomposition",
{
    hessen(m4) 
    hessen(m)
})

expect_flequal <- function(a,b){
    if(is.list(a))
        for(i in 1:length(a))
            expect_flequal(a[[i]],b[[i]])

    if(is.FLMatrix(a)) a <- as.matrix(a)
    if(is.FLMatrix(b)) b <- as.matrix(b)
    expect_equal(a,b)
}

## Testing FLSVDecomp
## Phani -- results differ in Teradata and R
### but function works.
test_that("check Singular Value Decomposition",
{
    flv <- svd(m4)
    print(flv)
    expect_flequal(svd(m), svd(as.matrix(m)))
    svd(m3) 
})
                                        # Testing FLLUDecomp
test_that("check LU Decomposition",
{
    lu(m4) 
    lu(m) 
    lu(m3) 
})
## Testing FLTrace
test_that("check LU Decomposition",
{
    tr(m) 
    tr(m3) 
})
test_that("check FLTrace return type",
          expect_true(
              is.FLVector(tr(m))
          ))
test_that("check length of FLTrace result",
          expect_true(
              length(tr(m))==1
          ))


                                        #Testing FLSV
test_that("check return type and working for FLSV",
          expect_true(
              is.FLVector(FLSV(m5))
          ))

test_that("check dimensions returned for FLSV",
          expect_true(
              length(FLSV(m5))==nrow(m5)
          ))

                                        #Testing RowMeans
                                        ### Phani-- very less difference in results
                                        ### but test case-3 fails
test_that("check return type and working for rowMeans",
          expect_true(
              is.FLVector(rowMeans(m5))
          ))

test_that("check dimensions returned for rowMeans",
          expect_true(
              length(rowMeans(m5))==nrow(m5)
          ))

test_that("check result for rowMeans",
          expect_equal(as.vector(rowMeans(m5)), rowMeans(as.matrix(m5)))
          )

                                        #Testing ColMeans
test_that("check return type and working for colMeans",
          expect_true(
              is.FLVector(colMeans(m5))
          ))

test_that("check dimensions returned for colMeans",
          expect_true(
              length(colMeans(m5))==ncol(m5)
          ))

test_that("check result for colMeans",
          expect_equal(as.vector(colMeans(m5)), colMeans(as.matrix(m5)))
          )

                                        #Testing RowSums
test_that("check return type and working for rowSums",
          expect_true(
              is.FLVector(rowSums(m5))
          ))

test_that("check dimensions returned for rowSums",
          expect_true(
              length(rowSums(m5))==nrow(m5)
          ))

test_that("check result for rowSums",
          expect_equal(as.vector(rowSums(m5)), rowSums(as.matrix(m5)))
          )

                                        #Testing colSums
test_that("check return type and working for colSums",
          expect_true(
              is.FLVector(colSums(m5))
          ))

test_that("check dimensions returned for colSums",
          expect_true(
              length(colSums(m5))==ncol(m5)
          ))

test_that("check result for colSums",
          expect_equal(as.vector(colSums(m5)), colSums(as.matrix(m5)))
          )

                                        # Testing M_Addition
test_that("check result for M_Addition",
          expect_true(
          (m+1:3+matrix1+v1)==as.FLMatrix(matrix1+1:3+matrix1+as.vector(v1),connection)
          ))

                                        # Testing M_Subtraction
test_that("check result for M_Subtraction",
          expect_true(
          (m-1:3-matrix1-v1)==as.FLMatrix(matrix1-1:3-matrix1-as.vector(v1),connection)
          ))

                                        # Testing M_Multiplication
test_that("check result for M_Multiplication",
          expect_true(
          (m*1:3*matrix1*v1)==as.FLMatrix(matrix1*1:3*matrix1*as.vector(v1),connection)
          ))

                                        # Testing M_CrossProduct
test_that("check result for M_CrossProduct",
          expect_true(
          (1:5%*%matrix1%*%m)==as.FLMatrix(1:5%*%matrix1%*%matrix1,connection)
          ))

                                        # Testing M_IntegerDivision
test_that("check result for M_IntegerDivision",
          expect_true(
          (m%/%1:3%/%matrix1%/%v1)==as.FLMatrix(matrix1%/%1:3%/%matrix1%/%as.vector(v1),connection)
          ))

                                        # Testing M_Remainder
test_that("check working of M_Remainder",
          m%%1:3%%matrix1%%v1
          )

                                        # Testing FLEigen
test_that("check if FLEigen is working ",
{      eigen(m4)
    eigen(m5)
})

                                        # Testing FLLength
test_that("check if FLLength is working ",
{     length(m4)
    length(v1)
    length(sm1) })

                                        # Testing FL_CastingFunctions
test_that("different types of casting works",
{        as.vector(m)
    as.vector(v1)
    as.vector(v2)
    as.vector(sm1)
    as.matrix(m)
    as.matrix(v1)
    as.matrix(v2)
    as.matrix(sm1)
    as.FLMatrix(v1,connection)
    as.FLMatrix(v2,connection)
    as.FLMatrix(sm1,connection)
    as.FLMatrix(data.frame(1:2,3:4),connection)
    as.FLMatrix(as.vector(v1),connection)
    as.FLVector(m,connection)
    as.FLVector(sm1,connection)
    as.FLVector(smatrix1,connection)
    as.FLVector(data.frame(1:2,3:4),connection)
    as.FLVector(as.vector(v1),connection)
} )

                                        # Testing FLCorrel
test_that("different input combinations work for FLCorrel",
{         cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,2),connection))
    cor(as.FLMatrix(matrix(c(0,-1,4,-10),2),connection),as.FLMatrix(matrix(1:6,2),connection))
    cor(v1,as.FLMatrix(matrix(1:12,6),connection))
    cor(v1,v1)
    cor(matrix(1:12,6),v1)
    cor(1:2,as.FLMatrix(matrix(1:4,2),connection))
    cor(data.frame(-2:-1,c(1,0)),as.FLMatrix(matrix(1:6,2),connection))
    cor(v1,data.frame(6:1,11:16))
    cor(WideTable,WideTable)
                                        # cor(DeepTable,DeepTable)
})

test_that("check output dimensions for FLCorrel",
{
    expect_true(
        nrow(cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,2),connection)))==2
    )

    expect_true(
        ncol(cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,2),connection)))==3
    )
})

                                        # Testing FLQRDecomp
test_that("check if FLQRDecomp is working ",
{      qr(m4)
    qr(m)
    qr(m3)
})

                                        #Testing FLTriDiag
test_that("check result type and dimensions for FLTriDiag",
{
    expect_true(
        is.FLMatrix(FLTriDiag(m5))
    )

    expect_true(
        nrow(FLTriDiag(m5))==nrow(m5)
    )

    expect_true(
        ncol(FLTriDiag(m5))==ncol(m5)
    )
})

                                        #Testing FLMatrixREF
test_that("check result type and dimensions for FLMatrixREF",
{
    expect_true(
        is.FLMatrix(FLMatrixREF(m5))
    )

    expect_true(
        nrow(FLMatrixREF(m5))==nrow(m5)
    )

    expect_true(
        ncol(FLMatrixREF(m5))==ncol(m5)
    )
})

                                        #Testing FLMatrixRREF
test_that("check result type and dimensions for FLMatrixRREF",
{
    expect_true(
        is.FLMatrix(FLMatrixRREF(m5))
    )

    expect_true(
        nrow(FLMatrixRREF(m5))==nrow(m5)
    )

    expect_true(
        ncol(FLMatrixRREF(m5))==ncol(m5)
    )
})

                                        #Testing FLMatrixNorm
test_that("check result type and dimensions for FLMatrixNorm",
{
    expect_true(
        is.vector(FLMatrixNorm(m3,3))
    )

    expect_true(
        length(FLMatrixNorm(m3,4))==1
    )
})

                                        #Testing FLSolveExcl
test_that("check result type and dimensions for FLSolveExcl",
{
    expect_true(
        is.FLMatrix(solveExcl(m5,3))
    )
    
    expect_true(
        nrow(solveExcl(m5,6))==nrow(m5)
    )
    expect_true(
        ncol(solveExcl(m5,6))==ncol(m5)
    )

    expect_true(
        nrow(solveExcl(m5,3))==(nrow(m5)-1)
    )
    expect_true(
        ncol(solveExcl(m5,3))==(ncol(m5)-1)
    )

})

# Testing FLCholeskyDecomp
test_that("check if FLCholeskyDecomp is working ",
{
    chol(m4)
    expect_equal(as.matrix(chol(m4)), t(chol(as.matrix(m4))))
})

# Testing FLGInv
test_that("check if FLGInv is working ",
{     ginv(m2)
    expect_equal(as.matrix(ginv(m4)),ginv(as.matrix(m4)),check.attributes=FALSE)
    expect_equal(as.matrix(ginv(m3)),ginv(as.matrix(m3)),check.attributes=FALSE)
})
###############################################################
############# NEGATIVE TEST CASES #############################
###############################################################

test_that("check connection to correct DSN",
          expect_warning(
              odbcConnect("RandomDSN")
          )
          )

                                        # Testing FLIs
test_that("check class of a Matrix",
{
    expect_false(
        is.FLMatrix(matrix1)
    )
})

                                        # Testing FLSubsetting
test_that("check subscript out of bounds for matrix",
{
    expect_error(
        m[1:6,]
    )
    expect_error(
        m[,1:6]
    )
})

test_that("check non-equality of matrices",
          expect_false(
              m0 == m1
          ))

test_that("check non-compatibility of matrices for addition/subtraction",
{   
    expect_error(
    (m2 + m3)
    )
    expect_error(
    (m2 - m3)
    )
})

test_that("check non-compatibility of matrices for multiplication",
{   
    expect_error(
    (m2 * m3)
    )
    expect_error(
    (m2 %*% m3)
    )
})

                                        # Testing FLDet
test_that("check non-compatibility of non-square matrix for determinant",
          expect_error(
              det(m3)
          ))

test_that("check non-compatibility of det for flvector",
          expect_error(
              det(v1)
          ))

                                        # Testing FLJordanDecomp
test_that("check non-compatibility of matrix for Jordan Decomposition",
{     
    expect_error(
        jordan(m2)
    )
    expect_error(
        jordan(m3)
    )
})

                                        # Testing FLHessenDecomp
test_that("check non-compatibility of matrix for Hessenberg Decomposition",
          expect_error(
              hessen(m3)
          ))

                                        # Testing FLSolve
                                      
test_that("check non-compatibility of matrix for calculating inverse",
{  expect_error(solve(m3))
    expect_error(solve(m))
})

                                        # Testing FLCastFunctions
test_that("check non-compatibility of matrix for calculating inverse",
{  expect_null(
       as.FLMatrix(as.FLMatrix(matrix(1:4,2),connection),connection)
   )
       expect_null(
           as.FLVector(as.FLVector(c(1:4),connection),connection)
       )
})

                                        #Testing rbind
test_that("check ncol non-compatibility for rbind",
          expect_error(
              rbind(m4,matrix(1:16,4))
          ))

test_that("check input type non-compatibility for rbind",
          expect_error(
              rbind(m4,sm1)
          ))

                                        #Testing cbind
test_that("check nrow non-compatibility for cbind",
          expect_error(
              cbind(m4,matrix(1:16,4))
          ))

test_that("check input type non-compatibility for cbind",
          expect_error(
              cbind(m4,sm1)
          ))

                                        # Testing FLEigen
test_that("check non-compatibility of matrix for calculating eigenvalues and eigenvectors",
{  expect_error(eigen(m3))
    expect_error(eigen(m))
})

                                        # Testing FLCorrel

test_that("check non-compatibility of input dimensions for FLCorrel",
{ expect_error(
      cor(matrix(1:4,2),as.FLMatrix(matrix(1:6,3),connection))
  )

      expect_error(
          cor(data.frame(1,2),as.FLMatrix(matrix(1:6,3),connection))
      )
})

                                        # Testing FLTriDiag
test_that("check non-compatibility for non-square matrices for FLTriDiag",
          expect_error(
              FLTriDiag(m3)
          ))

                                        # Testing FLMatrixRREF
test_that("check non-compatibility for non-square matrices for FLMatrixRREF",
          expect_error(
              FLMatrixRREF(m3)
          ))

                                        # Testing FLMatrixREF
test_that("check non-compatibility for non-square matrices for FLMatrixRREF",
          expect_error(
              FLMatrixREF(m3)
          ))

                                        #Testing FLMatrixNorm
test_that("check parameter NormMethod range for FLMatrixNorm",
          expect_error(
              FLMatrixNorm(m5,5))
          )

                                        # Testing FLCholeskyDecomp
                                      
test_that("check non-compatibility of matrices for Cholesky Decomposition",
{     expect_error(chol(m))
    expect_error(chol(m3))
})

                                        # Testing FLSolveExcl
test_that("check non-compatibility for non-square matrices for FLSolveExcl",
          expect_error(
              solveExcl(m3)
          ))

                                        # Testing M_Division
test_that("check division by zero for M_Division",
          expect_error(
          (m/1:3/matrix1/v1)==as.FLMatrix(matrix1/1:3/matrix1/as.vector(v1),connection)
          ))

>>>>>>> .r207
