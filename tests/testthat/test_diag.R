#testing diag function
Renv <- new.env(parent = globalenv())
Renv$a <-3
Renv$b<-1:3
Renv$c<-1:5
Renv$d <- matrix(0,3,3)
diag(Renv$d) <- 1:3
Renv$x <- 1:5
Renv$y <- stats::rnorm(5)
Renv$z <- matrix(rnorm(6),3,
            dimnames=list(c("a","b",""),
              c("a","b")))

FLenv <- as.FL(Renv)
test_that(
  "Testing dim of diag for vector of length 1 ",
  {
    result1=eval_expect_equal({test1<-dim(diag(a))},Renv,FLenv)
    ##print(result1)
  })

test_that(
  "Testing diag for vector of length > 1 ",
  {
    result2=eval_expect_equal({test2<-diag(10,3,4)},Renv,FLenv)
    ##print(result2)
  })

test_that(
  "Testing diag for matrix with unconventional names ",
  {
    result5=eval_expect_equal({
     test5<-diag(z)
     },Renv,FLenv)
    ##print(result5)
  })

test_that(
  "Testing equality works for vectors from diag ",
  {
    result3=eval_expect_equal({
              test3<-all(diag(b)==d)
            },Renv,FLenv)
    ##print(result3)
  })


# cbind fails
test_that(
  "Testing diag for matrix with names ",
  {
    result4=eval_expect_equal({
      v<-var(M <- cbind(x,y))
      test4<-diag(v)
      },Renv,FLenv)
    ##print(result4)
  })
