# DB-Lytix Example.
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
deeptbl  <- FLTable("tbllda", "ObsID", "VarID", "Num_Val")
FLenv$DAT <- deeptbl

RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <-  letters[1:4] 
Renv$DAT <- RD

eval_expect_equal({
    fit <- lda(a~., data = DAT)
},Renv,FLenv,
noexpectation = "fit")


## Means, coefficients & scores:
## https://fuzzyl.atlassian.net/browse/TDFL-836
test_that("Means and scale, counts:", {eval_expect_equal({
    scal <- fit$scaling
    mn <- fit$means
    n <- fit$N
    coun <- fit$counts    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("scal", "mn", "n", "coun"))
})


## Means, coefficients & scores:
test_that("coefficients:", {eval_expect_equal({
    cof <- coefficients(fit)
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("cof"))
})
