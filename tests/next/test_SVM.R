### Test Cases not running on Aster
## Asana Ticket: https://app.asana.com/0/136555696724838/364676248287856
## using Vectors.
## For Linear Kernel.
FLenv <- new.env(parent = globalenv())
FLenv$tbl  <- FLTable(getTestTableName("tblSVMLinSepMultiDim"), "OBSID", whereconditions= "OBSID>307")
Renv <- as.R(FLenv)


if(is.TDAster()) vformula <- dep~. else vformula <- DEP~.

eval_expect_equal({
    mod <- svm(vformula, data = tbl, fetchID = TRUE, kernel = "linear")
    len <- length(predict(mod))
    },Renv,FLenv,
noexpectation = c("fit","mod"))



## Means, coefficients & scores:
test_that("confusion, counts:", {eval_expect_equal({
    cost <- mod$cost
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectation = c("cost"))
})


## test will fail as some values depends aren't constant.
test_that("",{
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
    len <- length(predict(mod))
    },Renv,FLenv,
noexpectation = c("fit","mod"))



## Means, coefficients & scores:
test_that("confusion, counts:", {eval_expect_equal({
    deg <- mod$degree
    cost <- mod$cost
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectation = c("deg", "cost"))
})


## test will fail as some values depends aren't constant.
## misclass, Bvalue:
test_that("", {
expect_equal(FLenv$mod$BValue,-6.79, tolerance = 2)
expect_equal(FLenv$mod$crbfConstant,"Not applicable for this kernel")
expect_equal(FLenv$mod$lambda,.219, tolerance = 1)
})


##Using formula
## for gaussian kernel
FLenv <- new.env(parent = globalenv())

FLenv$tbl <- FLTable("tblSVMDense", "OBSID", whereconditions = "OBSID<307")
Renv <- as.R(FLenv)


eval_expect_equal({
    mod <- svm(vformula, data = tbl, fetchID = TRUE, kernel = "radial")
    len <- length(predict(mod))
    },Renv,FLenv,
noexpectation = c("fit","mod"))



## Means, coefficients & scores:
test_that("confusion, counts:", {eval_expect_equal({
    mod <- svm(vformula, data = tbl, fetchID = TRUE, kernel = "radial")
    cost <- mod$cost
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectation = c("cost"),
noexpectation= c("mod"))
})



######################
## a random matrix with 100 2-D data points
## with known class is checked for classification by svm(). 
test_that("check prediction of svm matches input class",{
    # m is 100 X 3 matrix, 3rd column is the class of input data points.
    m<- rbind(cbind(matrix(rnorm(100, sd = 0.3), ncol = 2),c(-1)),
          cbind(matrix(rnorm(100, mean = 5, sd = 0.3), ncol = 2),c(1)))
    # shuffling rows of m
    m<- m[sample(nrow(m)),]
    RDataFrame <- data.frame(m)
    for(i in c("linear", "polynomial", "radial basis")){
        FLTbl <- as.FLTable(RDataFrame, temporary= FALSE, drop= TRUE)
        FLmod<- svm(X3~., data= FLTbl, kernel= i)
        FLPred <- predict(FLmod)
        FLexpect_equal(FLPred, FLTbl[,4])
    }
})

