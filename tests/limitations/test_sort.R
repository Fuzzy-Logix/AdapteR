Renv <- new.env(parent = globalenv())
Renv$x <- swiss$Education[1:25]

## partial not supported in sort
test_that("sorting",eval_expect_equal({
  test2 <- sort(x,partial=TRUE)
},Renv,FLenv))
