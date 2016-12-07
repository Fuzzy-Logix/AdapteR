# DB-Lytix Example.
Renv <- new.env(parent = globalenv())
FLenv <- as.FL(Renv)
deeptbl <- FLTable("tblIrisDeep", "ObsID", "VarID", "Num_Val")
FLenv$DAT <- deeptbl

RD <- as.data.frame(deeptbl, drop.intercept=TRUE)
RD <- RD[, -2]
names(RD) <-  letters[1:length(RD[1,])] 
Renv$DAT <- RD

FLenv$fit <- fda(a~. , data = FLenv$DAT)
Renv$fit <- fda(a~ 1+. , data = Renv$DAT, method = mars)

## Means, coefficients & scores:
test_that("confusion, counts:", {eval_expect_equal({
    conf <- confusion(fit)
    mn <- fit$means
    pri <- fit$prior
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("conf", "pri","mn" ))
})
  
##  coefficients & predict:
test_that("coefficients:", {eval_expect_equal({
    cof <- coefficients(fit)
    pred <- predict(fit)
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("pred", "cof"))
})
