#Test for <colSums> <rowSums> <colMeans> <rowMeans> using examples from R documentation
#Creating new R environment
Renv = new.env(parent = globalenv())

#Creating a block diagonal matrix M
Renv$M <- bdiag(Diagonal(2), matrix(1:3, 3,4), diag(3:2))
Renv$M <- as.array(Renv$M)


FLenv <- as.FL(Renv)

test_that("Check1 for colSums function",{
  r<-colSums(Renv$M)
  fl<-rowcolOps(FLenv$M,2,"Sum")
  FLexpect_equal(r,fl)
})

test_that("Check2 for rowSums function",{
  r<-rowSums(Renv$M)
  fl<-rowcolOps(FLenv$M,1,"Sum")
  FLexpect_equal(r,fl)
})

test_that("Check3 for colMeans function",{
  r<-colMeans(Renv$M)
  fl<-rowcolOps(FLenv$M,2,"Mean")
  FLexpect_equal(r,fl)
})

test_that("Check4 for rowMeans function",{
  r<-rowMeans(Renv$M)
  fl<-rowcolOps(FLenv$M,1,"Mean")
  FLexpect_equal(r,fl)
})

