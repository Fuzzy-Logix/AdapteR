## here goes all stuff that I am working on and that is
## not yet ready for inclusion in the test suite

## gk @dhruv: please use this for reating a benchmarking for lm
N <- 100
sdResidual <- .1
sdZ <- 1

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)


## simulation step
Renv$D <- data.frame(x=rbinom(N,1,.5),
                     z=rnorm(N,0,sdZ),
                     eps=rnorm(N,0,sdResidual))

## gk: TODO implement R function for creating such a simulated dataset:
FLenv$D <- FLTable(x=rbinom(N,1,.5),
                   z=rnorm(N,0,1),
                   eps=rnorm(N,0,sdResidual))

"SELECT a.SerialVal,
       FLSimBinomial(a.SerialVal, 0.5, 1) AS x,
       FLSimNormal(a.SerialVal, 0.0, ",sdZ,") AS z,
       FLSimNormal(a.SerialVal, 0.0, ",sdResidual,") AS eps
FROM fzzlSerial a WHERE a.SerialVal <= ",N," ORDER BY 1;"



test_that("Kmeans returns objects correctly",{
    eval_expect_equal({
        D$y <- 2+1*D$x+3*D$z + D$eps
        N
    }, Renv, FLenv,
    "simulate outcomes")
    eval_expect_equal({
        ## data mining / recovery
        fit <- lm(y ~ x*z, data=D)
    }, Renv, FLenv,
    "linear model")
})



## next: extend this to 100 columns
