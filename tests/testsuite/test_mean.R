## Testing arithemetic mean
Renv <- new.env(parent = globalenv())
Renv$a <-c(0:10, 50)
Renv$b<-mean(Renv$a)
FLenv <- as.FL(Renv)
test_that("Testing mean",
    {
      result1=eval_expect_equal({test1<-mean(a)},Renv,FLenv)
      ##print(result1)
    }
)
