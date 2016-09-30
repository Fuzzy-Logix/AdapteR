Renv <- new.env(parent = globalenv())
Renv$x <- swiss$Education[1:25]
Renv$x1 <- rnorm(10)
Renv$x2 <- c(10:3, 2:12)
FLenv <- as.FL(Renv)

test_that("sort: partial=TRUE",
          eval_expect_equal({
              test2 <- sort(x,partial=TRUE)
          },Renv,FLenv))

test_that("sort: method=qucik, radix",
          eval_expect_equal({
              tquick <- sort(x2, method = "quick", index.return = TRUE)
              test5 <- sort(x2, method = "radix")
          },Renv,FLenv))

