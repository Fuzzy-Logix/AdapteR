## ROC Test:
rm(list = setdiff(ls(),"connection"))



FLenv = new.env(parent = globalenv())
FLenv$tbl <- FLTable("tblROCCurve", "ObsID")
Renv = as.R(FLenv)



test_that("ROC model.",{
    result = eval_expect_equal({
        mod <- roc(tbl$ActualVal, tbl$ProbVal)

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



