Renv = new.env(parent = globalenv())

Renv$var1 = c(3, 1, 4, 15, 92)
Renv$var2 = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(Renv$var2) = letters[1:11]
Renv$vector1 = sample(1:100,60,replace = TRUE)

FLenv = as.FL(Renv)

## All these methods are not supported.. DB-Lytix and  R have average.
## In addition FL has "perc" and "duplicate" which are not in R
for(meth in c("first","last","random","max","min"))
    tryCatch(
        test_that(paste0("rank: base R ties.method=",meth),{
            result = eval_expect_equal({
                testm = rank(var2, ties.method= meth)  
            },Renv,FLenv,
            expectation="testm")
        }),
        error=function(e) e)
