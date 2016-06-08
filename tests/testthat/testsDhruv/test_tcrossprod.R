library(AdapteR)
library(testthat)

test_that("check tcrossprod",{

expect_eval_equal(initFdefault,AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE,specs = list(list(n=5,isSquare = TRUE),(list(n=5,isRowVec = TRUE))),classes = c("FLMatrix","FLVector"))
 
expect_eval_equal(initFdefault(specs = list(c(n=5,isSquare = FALSE),c(n=5,isRowVec = FALSE )),classes = c("FLMatrix","numeric")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isSquare = FALSE),c(n=4,isSquare = TRUE )),classes = c("FLMatrix","FLMatrix")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)
 
expect_eval_equal(initFdefault(specs = list(c(n=5,isSquare = TRUE),c(n=6,isSquare = FALSE )),classes = c("FLMatrix","matrix")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isRowVec = TRUE),c(n=6,isSquare = FAsLSE )),classes = c("FLVector","FLMatrix")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isRowVec = TRUE),c(n=5,isRowVec = TRUE )),classes = c("FLVector","FLVector")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isRowVec = TRUE),c(n=5,isSquare = TRUE )),classes = c("FLVector","matrix")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE) 

expect_eval_equal(initFdefault(specs = list(c(n=5,isRowVec = TRUE),c(n=5,isRowVec = FALSE )),classes = c("FLVector","numeric")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isSquare = TRUE),c(n=5,isRowVec = FALSE )),classes = c("matrix","FLVector")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=4,isSquare = TRUE),c(n=5,isSquare = FALSE )),classes = c("matrix","FLMatrix")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isSquare = TRUE),c(n=5,isSquare = FALSE )),classes = c("matrix","matrix")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)

expect_eval_equal(initFdefault(specs = list(c(n=5,isSquare = TRUE),c(n=5,isRowVec = TRUE )),classes = c("matrix","numeric")),AdapteR::tcrossprod,base::tcrossprod,benchmark = FALSE)
})