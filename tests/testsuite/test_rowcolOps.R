#Test for <colSums> <rowSums> <colMeans> <rowMeans> using examples from R documentation
#Creating new R environment
Renv = new.env(parent = globalenv())
#Creating a block diagonal matrix M
Renv$M <- bdiag(Diagonal(2), matrix(1:3, 3,4), diag(3:2))
Renv$M <- as.array(Renv$M)


FLenv <- as.FL(Renv)

test_that("colSums",{
    eval_expect_equal({
        cols<-colSums(M)
    }, Renv, FLenv)
})

test_that("rowSums",{
    eval_expect_equal({
        rows<-rowSums(M)
    }, Renv, FLenv)
})

test_that("colMeans",{
    eval_expect_equal({
        colm<-colMeans(M)
    }, Renv, FLenv)
})

test_that("Check4 for rowMeans function",{
    eval_expect_equal({
        rowm<-rowMeans(M)
    }, Renv, FLenv)
})

