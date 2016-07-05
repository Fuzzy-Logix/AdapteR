Renv <- new.env(parent = globalenv())
Renv$x <- swiss$Education[1:25]
                                        #set.seed(1213)
Renv$x1 <- rnorm(10)

FLenv <- as.FL(Renv)

test_that("sorting",eval_expect_equal({
  test1 <- sort(x)
  test2 <- sort(x1)
  test3 <- sort(c(10:3, 2:12), method = "shell", index.return = TRUE)
  test4 <- sort(c(10:3, 2:12), method = "quick", index.return = TRUE)
  test5 <- sort(c(10:3, 2:12), method = "radix")
},Renv,FLenv))
