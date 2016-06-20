
Renv <- new.env(parent = globalenv())

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
Renv$h8 <- hilbert(8); 

FLenv <- as.FL(Renv)

#Error                   asana -   https://app.asana.com/0/143316600934101/144595699593757
test_that("Solve Function", {eval_expect_equal({
  sh8 <- solve(h8)
  }, Renv, FLenv)
  })
  

