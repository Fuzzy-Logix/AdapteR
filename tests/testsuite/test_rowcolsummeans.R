#Test for <colSums> <rowSums> <colMeans> <rowMeans> using examples from R documentation
#Creating new R environment
Renv = new.env(parent = globalenv())

#Creating a block diagonal matrix M
Renv$M <- bdiag(Diagonal(2), matrix(1:3, 3,4), diag(3:2))
Renv$M <- as.array(Renv$M)


FLenv <- as.FL(Renv)
#colSums(M)
#rowSums(M)
#colMeans(M)
#rowMeans(M)



#Tests

test_that("Check1 for colSums function",{
  result = eval_expect_equal({colSums(M) },Renv, FLenv)
})

test_that("Check2 for rowSums function",{
  result = eval_expect_equal({rowSums(M) },Renv,FLenv)
})

test_that("Check3 for colMeans function",{
  result = eval_expect_equal({colMeans(M) },Renv,FLenv)
})

test_that("Check4 for rowMeans function",{
  result = eval_expect_equal({rowMeans(M) },Renv,FLenv)
})

#Creating a digaonal numeric matrix MM: a result of kronecker product

# Renv$d <- Diagonal(10, c(0,0,10,0,2,rep(0,5)))
# Renv$MM <- kronecker(Renv$d, Renv$M)
# dim(Renv$MM) # 70 80
# length(Renv$MM@x) # 160, but many are '0' ; drop those:
# Renv$MM <- drop0(Renv$MM)
# length(Renv$MM@x) # 32
# Renv$MM <- as.array(Renv$MM)
#    cm <- colSums(MM)
##   scm <- colSums(Renv$MM, sparseResult = TRUE) ##Error:unused argument sparseResult = T
##   FLColSums.R:22 - source of error

#rowSums (MM, sparseResult = TRUE) # 14 of 70 are not zero
#colMeans(MM, sparseResult = TRUE) # 16 of 80 are not zero

#Renv$MM

# All four tests for MM give error: 
# * unable to find an inherited method for function 'as.FL' for signature '"ddiMatrix"'
# MM is an object of class ddiMatrix because it is kronecker product of d and M; 
# where d is diagonal matrix

#Tests

# test_that("Check1 for colSums function",{
#   result = eval_expect_equal({colSums(MM) },Renv)
#   print(result)
# })
# 
# test_that("Check2 for rowSums function",{
#   result = eval_expect_equal({rowSums(MM) },Renv,)
#   print(result)
# })
# 
# test_that("Check3 for colMeans function",{
#   result = eval_expect_equal({colMeans(MM) },Renv)
#   print(result)
# })
# 
# test_that("Check4 for rowMeans function",{
#   result = eval_expect_equal({rowMeans(MM) },Renv)
#   print(result)
# })


  
  
