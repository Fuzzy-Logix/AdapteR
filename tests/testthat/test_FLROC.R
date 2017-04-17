## ROC Test:
rm(list = setdiff(ls(),"connection"))



FLenv = new.env(parent = globalenv())
FLenv$tbl <- FLTable(getTestTableName("tblROCCurve"), "ObsID")
colnames(FLenv$tbl) <- tolower(colnames(FLenv$tbl))
Renv = as.R(FLenv)



test_that("ROC model.",{
    result = eval_expect_equal({
        mod <- roc(tbl$actualval, tbl$probval)

    },Renv,FLenv,
    expectation = c("mod"),
    check.attributes=T,
    tolerance = .0001
    )
})





## area, controls:
test_that("$ Operators for ROC model:-", {eval_expect_equal({
    area <- mod$auc
    controls <- mod$controls
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("area", "controls"))
})




## area, controls:
test_that("$ Operators for ROC model:-", {eval_expect_equal({
    spec <- mod$specificities
    sen <- mod$sensitivities
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("sen", "spec"))
})









## test case for creditcard.
## ROC made using predictions of FL models of glm, rf, dt.
setwd("f:/Use Case/")
glm.predict <- dget(file = "glm.predict")
dt.predict <- dget(file = "dt.predict")
rf.predict <- dget(file = "rf.predict")
dt.predict <- dt.predict$depVar
rf.predict <- rf.predict$depVar
glm.predict <- glm.predict$depVar
depVar <- dget(file = "depVar")
depVar <- depVar$depVar
rf.roc <- roc(depVar, rf.predict)
glm.roc <- roc(depVar, glm.predict)
dt.roc <- roc(depVar, dt.predict)
