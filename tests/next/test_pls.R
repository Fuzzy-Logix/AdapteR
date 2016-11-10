##TO-DO:1.)using ncomp from results for loadings
##      2.)find a better method for prediction.
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

## loadings etc....
test_that("loadings and other parameter:", {eval_expect_equal({
    fitload <- fit$loadings
    ymn <- fit$Ymeans
    xmn <- fit$Xmeans
},Renv,FLenv,
verbose = TRUE,
expectations = c("fitload", "ymn", "xmn"))
})



## scores, Yloadings , ...
test_that("loadings and other parameter:", {eval_expect_equal({
    yload <- fit$Yloadings
    load.wt <- fit$loadings.weights
    xscr <- fit$scores
},Renv,FLenv,
verbose = TRUE,
expectations = c("yload", "load.wt", "xscr"))
})
