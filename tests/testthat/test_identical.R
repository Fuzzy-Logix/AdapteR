#testing identical function
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
test_that(
  "Testing ",
  {
    result1=eval_expect_equal({test1<-identical(1,NULL)},Renv,FLenv)
    print(result1)
    
  })

test_that(
  "Testing ",
  {
    result2=eval_expect_equal({test2<-identical(1,1.)},Renv,FLenv)
    print(result2)
  })

test_that(
  "Testing ",
  {
    result3=eval_expect_equal({test3<-identical(1,as.integer(1))},Renv,FLenv)
    print(result3)
  })
test_that(
  "Testing ",
  {
    result4=eval_expect_equal({test4<-identical(0.,-0.)},Renv,FLenv)
    print(result4)
  })
test_that(
  "Testing ",
  {
    result5=eval_expect_equal({test5<-identical(0.,-0.)},Renv,FLenv)
    print(result5)
  })

test_that(
  "Testing ",
  {
    result6=eval_expect_equal({test6<-identical(NaN, -NaN)},Renv,FLenv)
    print(result6)
  })
test_that(
  "Testing ",
  {
    result7=eval_expect_equal({test7<-identical(-NaN, NaN)},Renv,FLenv)
    print(result7)
  })
test_that(
  "Testing ",
  {
    result8=eval_expect_equal({
      f <- function(x) x
      g <- compiler::cmpfun(f)
      test8<-identical(f,g)},Renv,FLenv)
   # test9<-identical(f,g,ignore.bytecode = FALSE),Renv,FLenv)
    print(result8)
    
  })
















