## Test for <length()> function using 4 examples from R Documentation for length()
## creating new R environment
Renv = new.env(parent = globalenv())
## 4 examples for testing taken from R documentation for length()
## 1
Renv$l = letters
Renv$fx <- freeny.x
Renv$freeny.y <- freeny.y
Renv$tT <- stats::ftable(Titanic)

## In order for examples to work in a chain, FLenv is needed
FLenv <- as.FL(Renv)

#head/tail
test_that("Head/Tail",
          eval_expect_equal({
            hl <- head(l)
            hl2 <- head(l, n = -6L)
            
            hx <- head(fx, n = 10L)
            hy <- head(freeny.y)
            
            tl <- tail(l)
            tl2 <- tail(l, n = -6L)
            
            tx <- tail(fx)
            ty <- tail(freeny.y)
            
            ##tail(library)
            
            htT <- head(tT)
          }, Renv))

