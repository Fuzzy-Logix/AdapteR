library(testthat)
library(AdapteR)

test_check("AdapteR")

#log/exp
test_that("Log and Exp",
          eval_expect_equal({
            test1 <- log(exp(3))
            test2 <-log10(1e7)
            x <- 10^-(1+2*1:9)
            test3 <-cbind(x, log(1+x), log1p(x), exp(x)-1, expm1(x))
                      }, Renv))

#head/tail
test_that("Head/Tail",
          eval_expect_equal({
            test4 <-head(letters)
            test5 <-head(letters, n = -6L)
            
            test6 <-head(freeny.x, n = 10L)
            test7 <-head(freeny.y)
            
            test8 <-tail(letters)
            test9 <-tail(letters, n = -6L)
            
            test10 <-tail(freeny.x)
            test11 <-tail(freeny.y)
            
            test12 <-tail(library)
            
            test13 <-head(stats::ftable(Titanic))
          }, Renv))

