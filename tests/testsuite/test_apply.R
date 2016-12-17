## Testing apply
Renv <- new.env(parent = globalenv())
Renv$m <- matrix(rnorm(25),5,
                dimnames=list(letters[1:5],
                            letters[6:10]))
FLenv <- as.FL(Renv)

test_that("apply over FLMatrix", {
    result1=eval_expect_equal({
        test1<-apply(m,2,mean)
        test2 <- apply(m,1,max)
    },Renv,FLenv,
    expectation=c("test1","test2"),
    tolerance=1e-6)
})

