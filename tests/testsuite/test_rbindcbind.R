# Tests for <cbind> <rbind> functions using examples from R documentation
# Creating new r environment
Renv = new.env(parent = globalenv())
#Creating examples for testing

##Only simple variables have been defined outside of eval_expect_equal.
##All Xbind functions have been called inside eval_expect_equal.

#1
##Renv$m1 <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
#2
##Renv$m2 <- cbind(Renv$m1, 8:14)[, c(1, 3, 2)] # insert a column

#3
##Renv$m3 <- cbind(I = 0, X = rbind(a = 1, b = 1:3)) # use some names

#4
xx <- data.frame(I = rep(0,2))
##Renv$m4 <- cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently

#5
dd <- 10
##Renv$m5a <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0) # middle 2 rownames
##Renv$m5b <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1) # 3 rownames (default)
##Renv$m5c <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2) # 4 rownames

#6
b0 <- gl(3,4, labels=letters[1:3])
bf <- setNames(b0, paste0("o", seq_along(b0)))
df  <- data.frame(a = 1, B = b0, f = gl(4,3))
df. <- data.frame(a = 1, B = bf, f = gl(4,3))
new <- data.frame(a = 8, B ="B", f = "1")
##Renv$m6a <- (df1  <- rbind(df , new))
##Renv$m6b <- (df.1 <- rbind(df., new))

FLenv <- as.FL(Renv)
#Tests


#test1
test_that("Check1 for cbind function",{
  result = eval_expect_equal({m1 <- cbind(1, 1:7)},Renv,FLenv)
  ##     print(result)
})

#test2
test_that("Check2 for cbind function",{
  result = eval_expect_equal({m2 <- cbind(m1, 8:14)[, c(1, 3, 2)]},Renv,FLenv)
  ##  print(result)
})

#test3
test_that("Check3 for cbind function",{
  result = eval_expect_equal({m3 <- cbind(I = 0, X = rbind(a = 1, b = 1:3))},Renv,FLenv)
  ##  print(result)
})

#test4
test_that("Check4 for cbind function",{
  result = eval_expect_equal({m4 <- cbind(xx, X = rbind(a = 1, b = 1:3))},Renv,FLenv)
  ##  print(result)
})

#test5a
test_that("Check5a for rbind function",{
  result = eval_expect_equal({m5a <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0)},Renv,FLenv)
  ##  print(result)
})
#test5b
test_that("Check5b for rbind function" ,{
  result = eval_expect_equal({m5b <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1)},Renv,FLenv)
  ##  print(result)
})
#test5c
test_that("Check5c for rbind function",{
  result = eval_expect_equal({m5c <- rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2)},Renv,FLenv)
  ##  print(result)
})

#test6a
test_that("Check6a for rbind function",{
  result = eval_expect_equal({m6a <- (df1  <- rbind(df , new))},Renv,FLenv)
  ##  print(result)
})
#test6b
test_that("Check6b for rbind function",{
  result = eval_expect_equal({m6b <- (df.1 <- rbind(df., new))},Renv,FLenv)
  ##  print(result)
})
