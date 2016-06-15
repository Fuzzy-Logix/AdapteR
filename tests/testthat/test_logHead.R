library(testthat)
library(AdapteR)

test_check("AdapteR")

#log/exp
test_that("Log and Exp",
          eval_expect_equal({
            log(exp(3))
            log10(1e7)
            x <- 10^-(1+2*1:9)
            cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
                      }, Renv))

#head/tail
test_that("Head/Tail",
          eval_expect_equal({
            head(letters)
            head(letters, n = -6L)
            
            head(freeny.x, n = 10L)
            head(freeny.y)
            
            tail(letters)
            tail(letters, n = -6L)
            
            tail(freeny.x)
            tail(freeny.y)
            
            tail(library)
            
            head(stats::ftable(Titanic))
          }, Renv))

