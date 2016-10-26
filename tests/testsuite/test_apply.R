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
    expectation=c("test1","test2"))
})


## Not working
FLenv$sv <- FLSerial(1,5)
names(FLenv$sv) <- letters[1:5]
Renv$sv <- as.vector(FLenv$sv)

test_that("max on FLSimpleVector and vector:", {
    result1=eval_expect_equal({
        test3<-max(sv,6,sv)
    },Renv,FLenv,
    expectation="test3")
})