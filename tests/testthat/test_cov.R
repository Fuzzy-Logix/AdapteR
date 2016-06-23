#not able to run because error in FLWideToDeep .
#Asana Ticket = https://app.asana.com/0/143316600934101/145657030318433

Renv = new.env(parent = globalenv())

Renv$df1 = swiss


FLenv <- as.FL(Renv)


test_that("Check for covariance ",{
    result = eval_expect_equal({ test1 = cov(df1)},
                                Renv,FLenv)
    print(result)
    })

#Error should come because missing values.
test_that("Check for covariance with use = all",{
    result = eval_expect_equal({ test2 = cov(df1, use = "all")},
                                Renv,FLenv)
    print(result)
    })

#To check whether result of use = complete and use = na.or.complete remains same.
test_that("Check for covariance with complete use",{
    result = eval_expect_equal({ test3 = cov(df1,use ="complete")
                                stopifnot(identical(test3, cov(df1, use = "na.or.complete")))},
                                Renv,FLenv)
    print(result)
    })

test_that("Check for covariance with use = pairwise",{
    result = eval_expect_equal({ test4 = cov(df1,use ="pairwise")},
                                Renv,FLenv)
    print(result)
    })


