## ROC Test:
## Test Case for failing dt Score
## implementing roc for FLTable.
rm(list = setdiff(ls(),"connection"))

FLenv = new.env(parent = globalenv())
FLenv$tbl <- FLTable(getTestTableName("tblROCCurve"), "ObsID")
colnames(FLenv$tbl) <- tolower(colnames(FLenv$tbl))
Renv = as.R(FLenv)

## Test case fails because mod is not created in Renv
## Asana Ticket: 
test_that("ROC model.",{
    result = eval_expect_equal({
        mod <- roc(ActualVal~ProbVal, data = tbl)
        #mod <- roc(tbl$actualval, tbl$probval)
    },Renv,FLenv,
    expectation = c("mod"),
    check.attributes=T,
    tolerance = .0001
    )
})

## area, controls:
test_that("$ Operators for ROC model:-", {
  result = eval_expect_equal({
    area <- mod$auc
##    controls <- mod$controls
  },Renv,FLenv,
  verbose = TRUE,
  check.attributes = FALSE,
  expectations = c("area", "controls")
  )
})

## specificities, sensitivities:
test_that("$ Operators for ROC model:-", {
  eval_expect_equal({
    spec <- mod$specificities
    sen <- mod$sensitivities
  },Renv,FLenv,
  verbose = TRUE,
  check.attributes = FALSE,
  expectations = c("sen", "spec")
  )
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
test_that("$ Operators for ROC model:-", {
  eval_expect_equal({
    area <- mod$auc
##    controls <- mod$controls   
  },Renv,FLenv,
  verbose = TRUE,
  check.attributes = FALSE,
  expectations = c("area", "controls")
  )
})

## specificities, sensitivities:
test_that("$ Operators for ROC model:-", {
  eval_expect_equal({
    spec <- mod$specificities
    sen <- mod$sensitivities
  },Renv,FLenv,
  verbose = TRUE,
  check.attributes = FALSE,
  expectations = c("sen", "spec")
  )
})

