#testing arithemetic mean
Renv <- new.env(parent = globalenv())
Renv$a <-c(0:10, 50)
Renv$b<-mean(Renv$a)
FLenv <- as.FL(Renv)
test_that(
  "Testing mean",
  {
result1=eval_expect_equal({test1<-mean(a)},Renv,FLenv)
print(result1)
}
)
test_that(
  "Testing mean",
  {
    result2=eval_expect_equal({test2<-c(b, mean(a, trim = 0.10))}
                      ,Renv,FLenv)
    print(result2)
  }
)


