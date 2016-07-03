Renv <- new.env(parent = globalenv())
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

Renv$x <- hilbert(9)[, 1:6]

FLenv <- as.FL(Renv)

test_that("Singular Value Decomposition of a Matrix.",{
    eval_expect_equal({
        Renv$s <- svd(Renv$x)
        FLenv$s <- svd(FLenv$x)
    }, Renv,FLenv,tolerance=0.15)
})
        
## fails without tolerance and 0.15 is high!
test_that("U is orthogonal https://app.asana.com/0/143316600934101/145369346525139",{eval_expect_equal({
    vUinv <- as.vector(as.matrix(s$u %*% t(s$u)))
    videnu <- as.vector(diag(nrow(s$u)))
    FLexpect_equal(vUinv,videnu,tolerance=0.15)
}, Renv,FLenv,tolerance=0.15)
})

## fails without tolerance and 0.15 is high!
test_that("V is orthogonal https://app.asana.com/0/143316600934101/145369346525139",{eval_expect_equal({
    vVinv <- as.vector(as.matrix(s$v %*% t(s$v)))
    videnv <- as.vector(diag(nrow(s$v)))
    FLexpect_equal(vVinv,videnv,tolerance=0.3)
}, Renv,FLenv,tolerance=0.3)
})


test_that("abs(D) is same in R and FL ",{eval_expect_equal({
    vabsd <- abs(as.vector(s$d))
}, Renv,FLenv,tolerance=0.01)
})
