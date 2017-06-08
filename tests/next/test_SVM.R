## using Vectors.
## For Linear Kernel.
FLenv <- new.env(parent = globalenv())
FLenv$tbl  <- FLTable("tblSVMLinSepMultiDim", "OBSID", whereconditions= "OBSID>307")
Renv <- as.R(FLenv)


if(is.TDAster()) vformula <- dep~. else vformula <- DEP~.

eval_expect_equal({
    mod <- svm(vformula, data = tbl, fetchID = TRUE, kernel = "linear")
    },Renv,FLenv,
noexpectation = "fit")




## Means, coefficients & scores:
test_that("confusion, counts:", {eval_expect_equal({
    deg <- mod$degree
    cost <- mod$cost
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("deg", "cost"))
})


## test will fail as some values depends aren't constant.
test_that("",{
expect_equal(FLenv$mod$misclassifications,7 )
expect_equal(FLenv$mod$BValue,"Not applicable for this kernel")
expect_equal(FLenv$mod$crbfConstant,"Not applicable for this kernel")
expect_equal(FLenv$mod$lambda,"Not applicable for this kernel" )
})




## Using formula
## For Polynomial kernel.
FLenv <- new.env(parent = globalenv())

FLenv$tbl <- FLTable("tblSVMDense", "OBSID", whereconditions = "OBSID<307")
Renv <- as.R(FLenv)


eval_expect_equal({
    mod <- svm(vformula, data = tbl, fetchID = TRUE, kernel = "polynomial")
    },Renv,FLenv,
noexpectation = "fit")



## Means, coefficients & scores:
test_that("confusion, counts:", {eval_expect_equal({
    deg <- mod$degree
    cost <- mod$cost
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("deg", "cost"))
})


## test will fail as some values depends aren't constant.
## misclass, Bvalue:
test_that("", {
expect_equal(FLenv$mod$misclassifications,8 )
expect_equal(FLenv$mod$BValue,-6.79, tolerance = 2)
expect_equal(FLenv$mod$crbfConstant,"Not applicable for this kernel")
expect_equal(FLenv$mod$lambda,.219, tolerance = 1)
})
