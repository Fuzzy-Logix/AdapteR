#testing abs function
#No ABS function in FL currently
Renv <- new.env(parent = globalenv())
Renv$a <- -9:9
Renv$b<-101
FLenv <- as.FL(Renv)
test_that(
  "Testing abs",
  {
    result1=eval_expect_equal({test1<-abs(a)},Renv,FLenv)
    print(result1)
    
  }
)
test_that(
"Testing abs",
 {
  result2=eval_expect_equal({test2<-plot(a, sqrt(abs(a)),  col = "red")},Renv,FLenv)
  print(result2)
  
}
)

test_that(
  "Testing abs",
  {
    result3=eval_expect_equal({test3<-lines(spline(a, sqrt(abs(a)), b), col = "pink")},Renv,FLenv)
    print(result3)
    
  }
)
