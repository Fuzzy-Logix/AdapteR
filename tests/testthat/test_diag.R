#testing diag function
Renv <- new.env(parent = globalenv())
Renv$a <-3
Renv$b<-1:3
Renv$c<-1:5
FLenv <- as.FL(Renv)
test_that(
  "Testing",
  {
    result1=eval_expect_equal({test1<-dim(diag(a))},Renv,FLenv)
    print(result1)
    
  })

test_that(
  "Testing",
  {
    result2=eval_expect_equal({test2<-diag(10,3,4)},Renv,FLenv)
    print(result2)
  })

#error: no method for coercing this S4 class to a vector
test_that(
  "Testing",
  {
    result3=eval_expect_equal({test3<-all(diag(b))},Renv,FLenv)
    print(result3)
  })



#error: get(n, envir = Renv) not equal to get(n, envir = FLenv).
test_that(
  "Testing",
  {
    result4=eval_expect_equal({
      v<-var(M <- cbind(X = 1:5, Y = stats::rnorm(5)))
      test4<-all(diag(v))},Renv,FLenv)
    print(result4)
  })


#error: get(n, envir = Renv) not equal to get(n, envir = FLenv).
test_that(
  "Testing",
  {
    result5=eval_expect_equal({
      rownames(M) <- c(colnames(M), rep("", 3));
      
     test5<-diag(M)},Renv,FLenv)
    print(result5)
  })



