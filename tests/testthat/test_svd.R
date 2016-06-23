Renv <- new.env(parent = globalenv())
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv$X <- hilbert(9)[, 1:6]

FLenv <- as.FL(Renv)

#ERROR                  asana ticket - https://app.asana.com/0/143316600934101/145369346525139 
test_that("Singular Value Decompostion(svd)",{eval_expect_equal({
  s <- svd(X)
  D <- diag(s$d)
}, Renv,FLenv)
})
