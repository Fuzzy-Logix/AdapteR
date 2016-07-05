## Test for <length()> function using 4 examples from R Documentation for length()
## creating new R environment
Renv = new.env(parent = globalenv())
## 4 examples for testing taken from R documentation for length()
## 1
Renv$l = letters
Renv$fx <- freeny.x
Renv$freeny.y <- freeny.y
##Renv$tT <- stats::ftable(Titanic)

## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)

#head/tail
test_that("Head/Tail of a vector",
          eval_expect_equal({
            hl <- head(l)
            tl <- tail(l)
            hy <- head(freeny.y)
            ty <- tail(freeny.y)
          }, Renv, FLenv))
            
test_that("Head/Tail of a vector with negative n",
          eval_expect_equal({
            tl2 <- tail(l, n = -6L)
            hl2 <- head(l, n = -6L)
          }, Renv, FLenv))

## R's rownames for tail are no proper
test_that("Head/Tail of a matrix",
          eval_expect_equal({
            hx <- head(fx, n = 10L)
            tx <- tail(fx)
            ##tail(library) ## does not apply to AdapteR
            ##htT <- head(tT)
          }, Renv, FLenv,
          expectation=c("hx","tx"),
          check.attributes=FALSE))

