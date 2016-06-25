Renv <- new.env(parent = globalenv())
Renv$a <-1:10
Renv$b<-c(1:5, 1:5)
FLenv <- as.FL(Renv)
test_that(
  "Testing variance",
{
result1=eval_expect_equal({test1<-var(a)},Renv,FLenv)
print(result1)

}
)
test_that(
  "Testing variance",
  {
   result2=eval_expect_equal({test2<-var(b)},Renv,FLenv)
   print(result2)
  }
)



