## DB-Lytix Example.

Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
deeptbl <- FLTable("tblMDA","ObsID", "VarID", "Num_Val")
FLenv$DAT <- deeptbl

RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
names(RD) <-  letters[1:length(RD[1,])] 
Renv$DAT <- RD


FLenv$fit <- mda(a~. , data = FLenv$DAT)
Renv$fit <- mda(a~., data = Renv$DAT)

## Weights, count, prior.
## https://fuzzyl.atlassian.net/browse/TDFL-837

test_that("weights,prior, counts:", {eval_expect_equal({
    pri <- fit$prior
    count <- fit$count
    wt <- fit$weights
       
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c( "wt", "pri","count" ))
})
  
## predict:
test_that("predict:", {eval_expect_equal({
    pred <- predict(fit)
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("pred"))
})



table(predict(FLenv$fit) == predict(Renv$fit))
