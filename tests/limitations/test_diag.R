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
