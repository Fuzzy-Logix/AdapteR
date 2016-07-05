Renv <- new.env(parent = globalenv())
Renv$a <-3
Renv$b<-1:3

FLenv <- as.FL(Renv)

# cbind fails
test_that(
  "diag for matrix with names after cbind: https://app.asana.com/0/143316600934101/145657030318443",
  {
    result4=eval_expect_equal({
      v<-var(M <- cbind(x,y))
      test4<-diag(v)
      },Renv,FLenv)
    ##print(result4)
  })


##Testing FLDiag
test_that("check the result of the diag of matrix",
{
    expect_eval_equal(initF.FLMatrix,
                      AdapteR::diag,
                      base::diag,
                      n=5)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=5)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=5,isRowVec=TRUE)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=1)
    expect_eval_equal(initF.FLVector,
                      AdapteR::diag,
                      base::diag,
                      n=1,isRowVec=TRUE)
})
