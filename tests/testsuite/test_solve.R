############################################################
## base::solve example based tests

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv <- new.env(parent = globalenv())
Renv$A <- hilbert(4)

options(debugSQL=F)
FLenv <- as.FL(Renv)

test_that("solve(m): m %*% solve(m) == I", {
    eval_expect_equal({
        aI <- A %*% solve(A)
    }, Renv, FLenv,
    expectation=c("aI"))
})

############################################################
## initF based tests
test_that("solve(m), matrices with small numbers", {
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::solve,
                      base::solve,
                      n=5,
                      isSquare=TRUE)
})
