## Test for <log> function using examples from R Documentation

## creating new R environment
Renv = new.env(parent = globalenv())
## 4 examples for testing taken from R documentation for length()
## 1
Renv$n1 = 3
Renv$n2 = 1e7


## Test for <length()> function using additionally classes supported by AdapteR
## gk: You should add tests to length for all objects that are supported by AdapteR, matrix, vector, table
Renv$nvector <- 1:9
## Renv$cvector <- c("a","b","c") 
## Renv$bvector <- c(TRUE,TRUE,FALSE,TRUE)
## Renv$nmatrix <- matrix(1:20,nrow=5)
##Renv$cmatrix <- matrix(rep(Renv$cvector,2),nrow=2) ## gk: TODO: support of character matrices
##Renv$df <- data.frame(a=1:5,b=6:10)

## ommitted from test because not related to AdapteR:
## Renv$opt = options()  ##options()is giving error in test_that() 
## length(y ~ x1 + x2 + x3)  # 3
## fm1 <- lm(breaks ~ wool * tension, data = warpbreaks)
## length(fm1$call)      ## 3, lm() and two arguments.
## length(formula(fm1))  ## 3, ~ lhs rhs

## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)

#log/exp
test_that("Log and Exp",{
    result = eval_expect_equal({
            len1 <- log(exp(n1))
            ln2 <- log10(n2)
            1
    }, Renv, FLenv)
    ##print(result)
})

test_that("power of vector arithmetic",{
    result = eval_expect_equal({
        constVec <- 2*nvector
        x <- 10^-(1+constVec)
        length(nvector)
    }, Renv, FLenv)
    ##print(result)
})

test_that("cbind to a matrix",{
    result = eval_expect_equal({
        m <- cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
        dim(m)
    }, Renv, FLenv)
    ##print(result)
})
