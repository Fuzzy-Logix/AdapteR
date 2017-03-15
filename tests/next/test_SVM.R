## using Vectors.
FLenv <- new.env(parent = globalenv())
FLenv$tbl  <- FLTable("tblSVMLinSepMultiDim", "OBSID", whereconditions= "OBSID>307")
Renv <- as.R(FLenv)



eval_expect_equal({
    mod <- svm(DEP~., data = tbl, fetchID = TRUE, kernel = "linear")
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


expect_equal(FLenv$mod$misclassifications,5 )
expect_equal(FLenv$mod$BValue,"Not applicable for this kernel")
expect_equal(FLenv$mod$crbfConstant,"Not applicable for this kernel")
expect_equal(FLenv$mod$lambda,"Not applicable for this kernel" )





## Using formula
FLenv <- new.env(parent = globalenv())

FLenv$tbl <- FLTable("tblSVMDense", "OBSID", whereconditions = "OBSID<307")
Renv <- as.R(FLenv)


eval_expect_equal({
    mod <- svm(DEP~., data = tbl, fetchID = TRUE, kernel = "polynomial")
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

expect_equal(FLenv$mod$misclassifications,8 )
expect_equal(FLenv$mod$BValue,-6.79, tolerance = .0002)
expect_equal(FLenv$mod$crbfConstant,"Not applicable for this kernel")
expect_equal(FLenv$mod$lambda,.219, tolerance = .002)
