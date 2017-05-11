Renv = new.env(parent = globalenv())
Renv$x <- matrix(rnorm(100), nrow = 5)

FLenv <- as.FL(Renv)

test_that("dist Function: Result correctness",{
    result = eval_expect_equal({
        dist2matrix <- function(pDist,
                              upper=FALSE,
                              diag=FALSE){
                            if(is.FL(pDist))
                              return(pDist)
                            vsize <- attr(pDist,"Size")
                            vmat <- matrix(0,vsize,vsize)
                            ventries <- as.vector(pDist)
                            k <- 1
                            for(i in 1:(vsize-1))
                              for(j in (i+1):vsize){
                                vmat[i,j] <- ventries[k]
                                k <- k+1
                              }
                            if(upper) vmat+t(vmat)
                            else t(vmat)
                        }
        result1 <- dist(x)
        result1 <- dist2matrix(result1)
        result2 <- dist(x, diag = TRUE)
        result2 <- dist2matrix(result2)
        result3 <- dist(x, upper = TRUE)
        result3 <- dist2matrix(result3,upper=TRUE)
    }, Renv, FLenv,
    expectation=c("result1","result2","result3"))
})
