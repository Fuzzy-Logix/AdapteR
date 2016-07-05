#Test for <colSums> <rowSums> <colMeans> <rowMeans> using examples from R documentation
#Creating new R environment
Renv = new.env(parent = globalenv())

#Creating a block diagonal matrix M
Renv$M <- bdiag(Diagonal(2), matrix(1:3, 3,4), diag(3:2))
Renv$M <- as.array(Renv$M)


FLenv <- as.FL(Renv)

test_that("Check1 for colSums function",{
  result = eval_expect_equal({colSums(M) },Renv, FLenv)
})

test_that("Check2 for rowSums function",{
  result = eval_expect_equal({rowSums(M) },Renv,FLenv)
})

test_that("Check3 for colMeans function",{
  result = eval_expect_equal({colMeans(M) },Renv,FLenv)
})

test_that("Check4 for rowMeans function",{
  result = eval_expect_equal({rowMeans(M) },Renv,FLenv)
})
