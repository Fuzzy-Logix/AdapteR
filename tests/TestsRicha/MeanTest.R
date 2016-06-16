#testing arithemetic mean
Renv <- new.env(parent = globalenv())
test_that(
  "Testing mean",
  {
Renv$a <-c(0:10, 50)
eval_expect_equal({test1<-mean(a)},Renv)
})


