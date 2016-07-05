Renv = new.env(parent = globalenv())

Renv$df1 = cbind(x = 1:10, y = c(1:3, 8:5, 8:10))
Renv$wt1 = c(0,0,0,1,1,1,1,1,0,0)

FLenv <- as.FL(Renv)

#Test failed . Diffrent covariance result matrix and different weight list too.
#Asana Ticket - https://app.asana.com/0/143316600934101/145657030318423
test_that("Check for weighted covariance with unbiased method",{
    result = eval_expect_equal({
        test1 = cov.wt(df1, wt = wt1)
    }, Renv,FLenv)
})

#test Failed. Different results for R and AdapteR.
#Asana Ticket - https://app.asana.com/0/143316600934101/145657030318423
test_that("Check for weighted covariance with ML method",{
    result = eval_expect_equal({
        test2 = cov.wt(df1, wt = wt1,method = "ML",cor = TRUE)
    }, Renv,FLenv)
})
