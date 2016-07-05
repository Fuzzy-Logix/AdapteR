hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv <- new.env(parent = globalenv())
Renv$h8 <- hilbert(8); 
Renv$A <- hilbert(4)

FLenv <- as.FL(Renv)


test_that("solve(hilbert(n)), precision for matrices with large numbers: https://app.asana.com/0/143316600934101/144595699593757", {
    eval_expect_equal({
        sA <- solve(A)
        sh8 <- solve(h8)
        h8I <- h8 %*% sh8
    }, Renv, FLenv,
    expectation=c("sA","sh8","h8I"))
})

test_that("Solve Function on complex matrix", {
    FLenv$Ac <- as.FL(Renv$Ac <- as.complex(Renv$A))
    eval_expect_equal({
        sAc <- solve(Ac)
    }, Renv, FLenv)
})
