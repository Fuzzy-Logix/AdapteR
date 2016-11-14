# DB-Lytix Example.
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
deeptbl  <- FLTable("tblPLSDeep2y", "ObsID", "VarID", "Num_Val")
FLenv$DAT <- deeptbl

RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <-  letters[1:16] 
Renv$DAT <- RD

eval_expect_equal({
    fit <- mvr(a~., data = DAT, ncomp = 5)
},Renv,FLenv,
noexpectation = "fit")


## Means, coefficients & scores:
test_that("Means and coefficients:", {eval_expect_equal({
    ymn <- fit$Ymeans
    xmn <- fit$Xmeans
    yscr <- fit$Yscores
    xscr <- fit$scores
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("xscr", "yscr", "ymn", "xmn"))
})


## loadings , ...
test_that("loadings:", {result <- eval_expect_equal({
    yload <- fit$Yloadings
    lwt <- fit$loading.weights
    load <- fit$loadings
   
},Renv,FLenv,
verbose = TRUE,
expectations = c("yload", "lwt", "load"),
check.atributes = FALSE)
})


## predict, residuals ....

Renv$pred <- unname(predict(Renv$fit, ncomp = 5))
Renv$res <- unname(residuals(Renv$fit)[,,5]) 
FLenv$pred <- predict(FLenv$fit)
FLenv$res <- residuals(FLenv$fit)

FLexpect_equal(Renv$pred, FLenv$pred)
FLexpect_equal(Renv$res, FLenv$res)

## R Example:
