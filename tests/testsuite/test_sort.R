Renv <- new.env(parent = globalenv())
Renv$x <- swiss$Education[1:25]
Renv$x1 <- rnorm(10)
Renv$x2 <- c(10:3, 2:12)
FLenv <- as.FL(Renv)

test_that("sort:",{
          eval_expect_equal({
              test1 <- sort(x)
              test2 <- sort(x1)
          },Renv,FLenv)
})


test_that("sort: method=shell",
          eval_expect_equal({
              tshell <- sort(x2, method = "shell", index.return = TRUE)
          },Renv,FLenv,
          expectation = c("tshell","tquick")))

options(debugSQL=F)
ls(envir = Renv)
