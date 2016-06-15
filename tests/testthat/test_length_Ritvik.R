#Test for <length()> function using 4 examples from R Documentation for length()
#creating new R environment
Renv = new.env(parent = globalenv())
#4 examples for testing taken from R documentation for length()
#1
Renv$diag_4 = diag(4)
#2
##Renv$opt = options()  ##options()is giving error in test_that() 

#3
##length(y ~ x1 + x2 + x3)  # 3

#4
require(stats)
fm1 <- lm(breaks ~ wool * tension, data = warpbreaks)
##length(fm1$call)      ## 3, lm() and two arguments.
##length(formula(fm1))  ## 3, ~ lhs rhs

#test1
test_that("Check1 for length function",{
  result = eval_expect_equal({length(diag_4) },Renv)
  print(result)
})

#test2  ## test for options() giving error. 
##test_that("Check2 for length function",{
##  eval_expect_equal({length(opt) },Renv)
##})

#test3
test_that("Check3 for length function",{
  result = eval_expect_equal({length(y ~ x1 + x2 + x3) },Renv)
  print(result)
})

#test4
test_that("Check4a for length function",{
  result = eval_expect_equal({length(fm1$call) },Renv)
  print(result)
})
test_that("Check4b for length function",{
  result = eval_expect_equal({length(formula(fm1)) },Renv)
  print(result)
})
