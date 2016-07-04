
Renv = new.env(parent = globalenv())

Renv$df1 = swiss
colnames(Renv$df1) <- gsub("[^a-zA-Z]","",colnames(Renv$df1))
## because of '.' in colnames not supported for FLTable.
## https://app.asana.com/0/143316600934101/147523528458761
rownames(Renv$df1) <- 1:nrow(swiss)

FLenv <- as.FL(Renv)


test_that("Check for covariance with use = pairwise",{
  result = eval_expect_equal({
    test4 = cov(df1,use ="pairwise")
  }, Renv,FLenv)
  ## print(result)
  ## print(Renv$test4)
  ## print(FLenv$test4)
})
