# DB-Lytix Example.
## https://fuzzyl.atlassian.net/browse/TDFL-835
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
deeptbl  <- FLTable(getTestTableName("tblPLSDeep2y"), "ObsID", "VarID", "Num_Val")
FLenv$DAT <- deeptbl

RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <-  letters[1:16] 
Renv$DAT <- RD

eval_expect_equal({
    fit <- mvr(a~., data = DAT, ncomp = 5)
},Renv,FLenv,
noexpectation = "fit")


## Means, coefficients:
test_that("Means and coefficients:", {eval_expect_equal({
    ymn <- fit$Ymeans
    xmn <- fit$Xmeans
    cof <- coefficients(fit)
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectation = c("ymn", "xmn"))
})

## Scores: (Do not match)
test_that("Scores:", {eval_expect_equal({
    yscr <- fit$Yscores
    xscr <- fit$scores
    names(xscr) <- NULL
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectation = c("xscr", "yscr"))
})



## loadings , ...(Do not match)
test_that("loadings:", {result <- eval_expect_equal({
    yload <- fit$Yloadings
    lwt <- fit$loading.weights
    load <- fit$loadings
   
},Renv,FLenv,
verbose = TRUE,
expectation = c("yload", "lwt", "load"),
check.atributes = FALSE)
})


## predict, residuals .... (Do not match)
test_that("predict, residuals",{
	Renv$pred <- unname(predict(Renv$fit))
	Renv$res <- unname(residuals(Renv$fit)[,,5]) 
	FLenv$pred <- predict(FLenv$fit)
	FLenv$res <- residuals(FLenv$fit)

	FLexpect_equal(Renv$pred, FLenv$pred)
	FLexpect_equal(Renv$res, FLenv$res)
})

deeptbl  <- FLTable(getTestTableName("tblPLSDeep2y"), "ObsID", "VarID", "Num_Val")
## opls function
test_that("opls function: coef, predict, residuals method work: ", {
    flmod<- opls(a~., data =deeptbl, ncomp = 5,northo = 5 )
    cof <- coefficients(flmod)
    pred <- predict(flmod);res <- residuals(flmod) 
})
