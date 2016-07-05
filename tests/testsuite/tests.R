## The old programmatic test suite.
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


## Testing FLIs
test_that("check FLIs",
{
    expect_eval_equal(initF.FLMatrix,AdapteR::is.FLMatrix,base::is.matrix,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::is.FLVector,base::is.vector,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::is.FLVector,base::is.vector,n=5,isRowVec=TRUE)
    expect_is(initF.FLTable(rows=5,cols=4),"FLTable")
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


