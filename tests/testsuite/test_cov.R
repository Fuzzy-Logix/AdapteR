
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

## Only use="pairwise" is supported in DB-Lytix.

# test_that("Check for covariance ",{
#     result = eval_expect_equal({ test1 = cov(df1)},
#                                 Renv,FLenv,check.attributes=FALSE)
#     print(result)
#     })

# #Error should come because missing values.
# test_that("Check for covariance with use = all",{
#     result = eval_expect_equal({ test2 = cov(df1, use = "all")},
#                                 Renv,FLenv,check.attributes=FALSE)
#     print(result)
#     })

# #To check whether result of use = complete and use = na.or.complete remains same.
# test_that("Check for covariance with complete use",{
#     result = eval_expect_equal({ test3 = cov(df1,use ="complete")
#                                 stopifnot(identical(test3, cov(df1, use = "na.or.complete")))},
#                                 Renv,FLenv,check.attributes=FALSE)
#     print(result)
#     })
