## Test for <length()> function using 4 examples from R Documentation for length()
## creating new R environment
Renv = new.env(parent = globalenv())
## 4 examples for testing taken from R documentation for length()
## 1
Renv$diag_4 = diag(4)


## Test for <length()> function using additionally classes supported by AdapteR
## gk: You should add tests to length for all objects that are supported by AdapteR, matrix, vector, table
Renv$nvector <- 1:10
Renv$cvector <- c("a","b","c") 
Renv$bvector <- c(TRUE,TRUE,FALSE,TRUE)
Renv$nmatrix <- matrix(1:20,nrow=5)
##Renv$cmatrix <- matrix(rep(Renv$cvector,2),nrow=2) ## gk: TODO: support of character matrices
## ommitted from test because not related to AdapteR:
## Renv$opt = options()  ##options()is giving error in test_that() 
## length(y ~ x1 + x2 + x3)  # 3
## fm1 <- lm(breaks ~ wool * tension, data = warpbreaks)
## length(fm1$call)      ## 3, lm() and two arguments.
## length(formula(fm1))  ## 3, ~ lhs rhs

## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)


test_that("length of matrix",{
    result = eval_expect_equal({
        ## gk: If you do not assign the results to the variable, it will not be tested for equality of R execution and Fuzzy Logix DB Lytix execution.
        L1 <- length(diag_4)
        Lnm <- length(nmatrix)
        ##Lcm <- length(cmatrix)
    },Renv,FLenv)
    ##print(result)
})

test_that("length of vector",{
    result = eval_expect_equal({
        Lnv <- length(nvector)
        Lcv <- length(cvector)
        Lbv <- length(bvector)
    },Renv,FLenv)
    ##print(result)
})



############################################################
## initF based tests
## initF.FLTable broken on Hadoop
## initF depcrecated
if(!is.Hadoop())
test_that("check length",
{
    T1 <- initF.FLTable(rows=5,cols=5)
    T1R <- as.data.frame(T1)
    expect_eval_equal(initF.FLMatrix,AdapteR::length,base::length,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::length,base::length,n=5)
    expect_eval_equal(initF.FLVector,AdapteR::length,base::length,n=5,isRowVec=TRUE)
    expect_equal(AdapteR::length(T1),base::length(T1R),check.attributes=FALSE)
})

