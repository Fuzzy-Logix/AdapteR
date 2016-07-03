#testing sum function
Renv <- new.env(parent = globalenv())
Renv$a <-1:5
Renv$b<-c(1,2,3,4,5)
Renv$c<-c(1:2,3:5)

#Renv$d<-c(1:5, NA)

FLenv <- as.FL(Renv)
test_that( "Testing sum",{
    result1=eval_expect_equal({test1<-sum(a)},Renv,FLenv)
    ##print(result1)
})
test_that("testing sum",{
    result2=eval_expect_equal({test2<-sum(b)},Renv,FLenv)
    ##print(result2)
})
test_that("testing sum",{
  result3=eval_expect_equal({test3<-sum(c)},Renv,FLenv)
  ##print(result3)
})

## casting with NULL,NA are not yet supported.
## https://app.asana.com/0/143316600934101/146934264360563
# test_that("testing sum",{
#   result4=eval_expect_equal({test4<-sum(d)},Renv,FLenv)
#   print(result4)
# })
