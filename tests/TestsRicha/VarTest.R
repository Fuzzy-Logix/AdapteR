#testing variance
Renv <- new.env(parent = globalenv())
test_that(
  "Testing variance",
{
Renv$a <-1:10
Renv$b<-c(1:5, 1:5)
eval_expect_equal({test1<-var(a)},Renv)
eval_expect_equal({test2<-var(b)},Renv)
}
)

