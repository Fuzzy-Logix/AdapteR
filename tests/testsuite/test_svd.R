Renv <- new.env(parent = globalenv())
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
Renv$x <- hilbert(9)[, 1:6]
FLenv <- as.FL(Renv)

test_that(" svd: check X=UDV' ",{
    eval_expect_equal({
        s <- svd(x)
        mysd <- s$d
        mysu <- s$u
        mysv <- s$v
        original <- mysu %*% diag(mysd) %*% t(mysv)
    }, Renv,FLenv,
    noexpectation=c("s","mysd","mysv","mysu"),
    expectation = c("original"),
    tolerance=1e-6,
    check.attributes=FALSE)
})
