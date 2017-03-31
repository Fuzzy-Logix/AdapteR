## ROC Test:
## Test Case for failing dt Score
## implementing roc for FLTable.
rm(list = setdiff(ls(),"connection"))

FLenv = new.env(parent = globalenv())
FLenv$tbl <- FLTable("tblROCCurve", "ObsID")
Renv = as.R(FLenv)

test_that("ROC model.",{
    result = eval_expect_equal({
        mod <- roc(ActualVal~ProbVal, data = tbl)

    },Renv,FLenv,
    expectation = c("mod"),
    check.attributes=T,
    tolerance = .0001
    )
})

## area, controls:
test_that("$ Operators for ROC model:-", {eval_expect_equal({
    area <- mod$auc
##    controls <- mod$controls
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("area", "controls"))
})

## specificities, sensitivities:
test_that("$ Operators for ROC model:-", {eval_expect_equal({
    spec <- mod$specificities
    sen <- mod$sensitivities
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("sen", "spec"))
})

## Example2: from pROC package
## implement for formula type
Renv = new.env(parent = globalenv())
data(aSAH)
aSAH <- aSAH[-c(55), ]
Renv$outcome <- aSAH$outcome
Renv$prob <- aSAH$s100b
Renv$outcome <- as.integer(Renv$outcome)-1
FLenv <- as.FL(Renv)

test_that("ROC model.",{
    result = eval_expect_equal({
        mod <- roc(outcome, prob)
    },Renv,FLenv,
    expectation = c("mod"),
    check.attributes=T,
    tolerance = .0001
    )
})


## area, controls:
test_that("$ Operators for ROC model:-", {eval_expect_equal({
    area <- mod$auc
##    controls <- mod$controls
    
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("area", "controls"))
})

## specificities, sensitivities:
test_that("$ Operators for ROC model:-", {eval_expect_equal({
    spec <- mod$specificities
    sen <- mod$sensitivities
},Renv,FLenv,
verbose = TRUE,
check.attributes = FALSE,
expectations = c("sen", "spec"))
})



## failing dt score
vSampleDataTables <- suppressWarnings(SampleData(pTableName="ARcreditcard",
                                  pObsIDColumn="ObsID",
                                  pTrainTableName="ARcreditcardTrain",
                                  pTestTableName="ARcreditcardTest",
                                  pTrainDataRatio=0.7,
                                  pTemporary=FALSE,
                                  pDrop=TRUE))
vTrainTableName <- vSampleDataTables["TrainTableName"]
vTestTableName <- vSampleDataTables["TestTableName"]
FLtbl <- FLTable(vTrainTableName,"ObsID",fetchIDs=FALSE)
rf.model <- randomForest.FLTable(Classvar ~ ., data = FLtbl, minsplit = 15, cp = .9999, maxdepth = 7)
rf.predict <- predict(rf.model,type = "prob")
val <- sqlQuery(connection, "select Count(*) FROM ARcreditCardTrain")
