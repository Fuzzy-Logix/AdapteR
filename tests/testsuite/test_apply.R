## Testing apply
Renv <- new.env(parent = globalenv())
Renv$m <- matrix(rnorm(25),5,
                dimnames=list(letters[1:5],
                            letters[6:10]))
FLenv <- as.FL(Renv)

test_that("apply over FLMatrix", {
    result1=eval_expect_equal({
        colmean <- apply(m,2,mean)
        rowmean <- apply(m,1,mean)
        rowsd <- apply(m,1,sd)
        colsd <- apply(m,2,sd)
        rowmax <- apply(m,1,max)
    },Renv,FLenv,
    expectation=c("test1","test2"),
    tolerance=1e-6)
})
