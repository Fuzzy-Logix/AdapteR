test_that("check for FL Solve",{
    FL_test_generic(specs = list(list(n =5,isSquare =TRUE)),classes = c("FLMatrix"),operator = "solve")
    })

test_that("check for FL Ginv",{
    FL_test_generic(specs = list(list(n=5,isSquare = FALSE)),classes = c("FLMatrix"),operator = "ginv")
    })

test_that("check for FLdims",{
    FL_test_generic(specs = list(list(n=5,isSquare = TRUE)),classes = c("FLMatrix"),operator = "dim")
    FL_test_generic(specs = list(list(5,6)),classes = c("FLTable"),operator = "dim")
    })
#as.FLMatrix is not defined for FLMatrix signature and as.FLVector needs vconnection for FLVector.
#This test case is not working
test_that("check for FLCastFunctions",{
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE)),classes = c("FLVector"),operator = "as.FLVector")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "as.FLMatrix")
    })

#Problem in do.call for obj1 in FL_test_generic
test_that("check for FL Cholesky",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "chol")
    })

test_that("check for FL LU",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "lu")
    })

#Error for FL Vector test case.
test_that("check for FL length function",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "length")
    FL_test_generic(specs = list(list(rows=5,cols =4)),classes = c("FLTable"),operator = "length")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE)),classes = c("FLVector"),operator = "length")
    })

test_that("check for FL Trace",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE)),classes = c("FLMatrix"),operator = "tr")
    })

#Error for Fl Vector test case 
test_that("check for FL Diag",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "diag")
    FL_test_generic(specs = list(list(n=5,isRowVec=FALSE)),classes = c("FLVector"),operator = "diag")
    })

#Error for FL vector cases
#May be need to check initF.FLVector
test_that("check for FL subtraction ",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "-")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "-")
    FL_test_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "-")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "-")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "-")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = TRUE)),classes = c("FLVector","matrix"),operator = "-")

    })

#Error in FL vector cases
#Need some modifications to make this generic test to work for more than two objects with a single operator
test_that("check for FL division ",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=6,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%/%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix","FLMatrix"),operator = "%/%")
})

test_that("check for FLcrossproduct",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "%*%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%*%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%*%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%*%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%*%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%*%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%*%")
    })

test_that("check for FL addition",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "+")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "+")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "+")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "+")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "+")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "+")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "+")
    })


test_that("check for FL division",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "/")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "/")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "/")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "/")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "/")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "/")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "/")
    })

test_that("check for FL multiplication",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "*")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "*")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "*")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "*")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "*")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "*")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "*")
    })

test_that("check for FL remainder",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "%%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "%%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","FLVector"),operator = "%%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLMatrix","numeric"),operator = "%%")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "%%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "%%")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "%%")
    })

test_that("check for FL equality",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = FALSE)),classes = c("FLMatrix","FLMatrix"),operator = "==")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=5,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "==")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "==")
    FL_test_generic(specs = list(list(n=5,isRowVec=TRUE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "==")
    })

test_that("check for FL subtraction for two vectors of different length",{
    FL_test_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=9,isRowVec = FALSE)),classes = c("FLVector","FLVector"),operator = "-")
    FL_test_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=9,isRowVec = FALSE)),classes = c("FLVector","numeric"),operator = "-")
    })

test_that("check for FL Transpose",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE)),classes = c("FLMatrix"),operator = "t")
    })

test_that("check for FL row means",{
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE)),classes = c("FLMatrix"),operator = "rowMeans")
    })

test_that("check for FL row sums",{
    FL_test_generic(specs = list(list(n=8,isSquare=TRUE)),classes = c("FLMatrix"),operator = "rowSums")
    })

test_that("check for FL colMeans",{
    FL_test_generic(specs = list(list(n=8,isSquare=FALSE)),classes = c("FLMatrix"),operator = "colMeans")
    })

test_that("check for FL colMeans",{
    FL_test_generic(specs = list(list(n=8,isSquare=FALSE)),classes = c("FLMatrix"),operator = "colSums")
    })

test_that("check for FL subsetting",{
    FL_test_generic(specs = list(list(n=8,isRowVec=FALSE)),classes = c("FLVector"),operator = "[")
    })

#Test for FL Table is showing some sql syntax error in FL wide to deep
#initFgeneric will generate same FL Table and I think correalation would be a constant
test_that("check for FL correl",{
    FL_test_generic(specs = list(list(n=5,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "cor")
    FL_test_generic(specs = list(list(n=5,isSquare=TRUE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "cor")
    FL_test_generic(specs = list(list(n=5,isRowVec=FALSE),list(rows=5,cols= 5)),classes = c("FLVector","FLTable"),operator = "cor")
    FL_test_generic(specs = list(list(rows=5,cols=5),list(rows=5,cols= 5)),classes = c("FLTable","FLTable"),operator = "cor")
    FL_test_generic(specs = list(list(rows=5,cols=6),list(n=5,isSquare=TRUE)),classes = c("FLTable","FLMatrix"),operator = "cor")
    FL_test_generic(specs = list(list(n=6,isRowVec = TRUE),list(n=5,isRowVec=TRUE)),classes = c("FLVector","FLVector"),operator = "cor")

    })

test_that("check for FL crossprod",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","FLVector"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","FLVector"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","numeric"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","FLMatrix"),operator = "crossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","matrix"),operator = "crossprod")
    })

test_that("check for FL tcrossprod",{
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","FLVector"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLMatrix","numeric"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","matrix"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLMatrix","FLMatrix"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","FLMatrix"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","FLVector"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=5,isSquare = TRUE)),classes = c("FLVector","matrix"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=6,isRowVec=FALSE),list(n=6,isRowVec = TRUE)),classes = c("FLVector","numeric"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","FLVector"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=6,isRowVec = FALSE)),classes = c("matrix","numeric"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","FLMatrix"),operator = "tcrossprod")
    FL_test_generic(specs = list(list(n=5,isSquare=FALSE),list(n=5,isSquare = TRUE)),classes = c("matrix","matrix"),operator = "tcrossprod")
    })

#objects are not created.Sql problem.
## gk: create Asana
test_that("check for FL string functions",{
    FL_test_generic(specs = list(list(n=6,isRowVec = FALSE,type ="character"),list(n=5,isRowVec=FALSE,type = "character")),classes = c("FLVector","FLVector"),operator = "hamming.distance")
    })
# Not changed below test functions
#They are as it is as they were in previous tests file.
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
  M <- FLMatrix("FL_DEMO","tblmatrixMulti",5,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")
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









